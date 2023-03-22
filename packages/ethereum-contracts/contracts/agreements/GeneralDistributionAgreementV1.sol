// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {
    EnumerableSet
} from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperApp,
    SuperAppDefinitions,
    ContextDefinitions,
    SuperfluidGovernanceConfigs
} from "../interfaces/superfluid/ISuperfluid.sol";
import {
    BasicParticle,
    PDPoolMember,
    Value,
    Time,
    PDPoolIndex,
    SemanticMoney,
    FlowRate,
    Unit
} from "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import { SuperTokenPool } from "../superfluid/SuperTokenPool.sol";
import { SuperTokenPoolDeployerLibrary } from "../libs/SuperTokenPoolDeployerLibrary.sol";
import {
    IGeneralDistributionAgreementV1,
    ISuperfluidToken
} from "../interfaces/agreements/IGeneralDistributionAgreementV1.sol";
import { ISuperTokenPool } from "../interfaces/superfluid/ISuperTokenPool.sol";
import { AgreementBase } from "./AgreementBase.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";

/**
 * @title General Distribution Agreement
 * @author Superfluid
 * @notice
 * // document storage layout of this contract. CFA, IDA in a separate PR
 * event design
 * slots bitmap instead of _connectionsMap
 * add some of this to SuperTokenV1Library
 * connect to NFT contracts (flow nfts, pool admin)
 */
contract GeneralDistributionAgreementV1 is
    AgreementBase,
    IGeneralDistributionAgreementV1
{
    using EnumerableSet for EnumerableSet.AddressSet;
    using SemanticMoney for BasicParticle;

    mapping(address owner => EnumerableSet.AddressSet connections) internal _connectionsMap;
    mapping(bytes32 flowAddress => FlowRate flowRate) public flowRates;
    mapping(ISuperTokenPool pool => bool exists) public pools;

    constructor(ISuperfluid host) AgreementBase(address(host)) {}

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        ISuperfluidToken token,
        address account,
        uint256 time
    )
        external
        view
        override
        returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
    {
        (, BasicParticle memory uIndexData) = _getUniversalIndexData(
            token,
            
            _getUniversalIndexId(account)
        );

        Value x = uIndexData.rtb(Time.wrap(uint32(time)));

        // pending distributions from pool
        if (pools[ISuperTokenPool(account)]) {
            x = x + Value.wrap(ISuperTokenPool(account).getPendingDistribution());
        }

        // pool-connected balance
        {
            EnumerableSet.AddressSet storage connections = _connectionsMap[
                account
            ];
            for (uint256 i = 0; i < connections.length(); ++i) {
                address p = connections.at(i);
                x = x + Value.wrap(ISuperTokenPool(p).getClaimable(uint32(time), account));
            }
        }

        dynamicBalance = Value.unwrap(x) > int256(0)
            ? int256(Value.unwrap(x))
            : int256(0);
    }

    function getNetFlowRate(
        address account
    ) external view override returns (int96) {}

    function getFlowRate(
        address from,
        address to
    ) external view override returns (int96) {

    }

    // test view function conditions where net flow rate makes sense given pending distribution
    // balance of the pool will capture pending flow rate

    function createPool(
        address admin,
        ISuperfluidToken token
    ) external override returns (SuperTokenPool pool) {
        pool = SuperTokenPoolDeployerLibrary.deploy(
            admin,
            GeneralDistributionAgreementV1(address(this)),
            token
        );
        pools[pool] = true;
        // why do I need to approve the pool to spend its own tokens?
    }

    // @note This is same as approveSubscription
    function connectPoole(
        ISuperTokenPool pool,
        bytes calldata ctx
    ) external returns (bytes memory newCtx) {
        return connectPool(pool, true, ctx);
    }

    // @note This is same as revokeSubscription
    function disconnectPool(
        ISuperTokenPool pool,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        return connectPool(pool, false, ctx);
    }

    function connectPool(
        ISuperTokenPool pool,
        bool doConnect,
        bytes calldata ctx
    ) public returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(pool._superToken(), ctx);
        address msgSender = currentContext.msgSender;
        newCtx = ctx;
        if (doConnect) {
            if (!_connectionsMap[msgSender].contains(address(pool))) {
                _connectionsMap[msgSender].add(address(pool));
                assert(
                    pool.operatorConnectMember(
                        uint32(block.timestamp),
                        msgSender,
                        true
                    )
                );
            }
        } else {
            if (_connectionsMap[msgSender].contains(address(pool))) {
                _connectionsMap[msgSender].remove(address(pool));
                assert(
                    pool.operatorConnectMember(
                        uint32(block.timestamp),
                        msgSender,
                        false
                    )
                );
            }
        }
    }

    function isMemberConnected(
        address pool,
        address member
    ) external override view returns (bool) {
        return _connectionsMap[member].contains(pool);
    }

    // # Universal Index operations
    //
    // Universal Index packing:
    //
    // -------- ------------------ ------------------ ------------------
    // WORD 1: |     flowRate     |     settledAt    |       free       |
    // -------- ------------------ ------------------ ------------------
    //         |       128b       |       32b        |        96b       |
    // -------- ------------------ ------------------ ------------------
    // WORD 2: |                      settledValue                      |
    // -------- ------------------ ------------------ ------------------
    //         |                          256b                          |
    // -------- ------------------ ------------------ ------------------

    function _getUniversalIndexId(
        address account
    ) private pure returns (bytes32) {
        return keccak256(abi.encode("universalIndex", account));
    }

    function absorbParticlesFromPool(
        ISuperfluidToken token,
        address[] calldata accounts,
        BasicParticle[] calldata ps
    ) public returns (bool) {
        if (pools[ISuperTokenPool(msg.sender)] == false)
            revert ONLY_SUPER_TOKEN_POOL();
        assert(accounts.length == ps.length);

        for (uint i = 0; i < accounts.length; i++) {
            (, BasicParticle memory accountParticle) = _getUniversalIndexData(
                token,
                _getUniversalIndexId(accounts[i])
            );

            // update account particle
            _updateUniversalIndex(
                token,
                _getUniversalIndexId(accounts[i]),
                accountParticle.mappend(ps[i])
            );
        }
        return true;
    }

    function distribute(
        ISuperfluidToken token,
        SuperTokenPool pool,
        uint256 requestedAmount,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);
        newCtx = ctx;

        // msg.sender cannot be equal to pool

        PDPoolIndex memory pdidx = pool.getIndex();

        bytes32 senderUniversalIndexId = _getUniversalIndexId(
            address(currentContext.msgSender)
        );
        (, BasicParticle memory fromUIndexData) = _getUniversalIndexData(
            token,
            senderUniversalIndexId
        );

        // @note might be unsafe casting here from uint256 -> int256
        (fromUIndexData, pdidx, ) = fromUIndexData.shift2(
            pdidx,
            Value.wrap(int256(requestedAmount))
        );
        _updateUniversalIndex(token, senderUniversalIndexId, fromUIndexData);
        assert(pool.operatorSetIndex(pdidx));
    }

    function distributeFlow(
        ISuperfluidToken token,
        SuperTokenPool pool,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);

        newCtx = ctx;

        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 distributionFlowAddress = _getDistributionFlowId(currentContext.msgSender, pool);

        bytes32 senderUniversalIndexId = _getUniversalIndexId(
            address(currentContext.msgSender)
        );
        (, BasicParticle memory fromUIndexData) = _getUniversalIndexData(
            token,
            senderUniversalIndexId
        );

        FlowRate oldFlowRate = fromUIndexData.flow_rate.inv();
        PDPoolIndex memory pdidx = pool.getIndex();

        FlowRate actualFlowRate;
        (fromUIndexData, pdidx, actualFlowRate) = fromUIndexData.shiftFlow2b(
            pdidx,
            FlowRate.wrap(requestedFlowRate) - flowRates[distributionFlowAddress],
            t
        );
        pool.operatorSetIndex(pdidx);
        flowRates[distributionFlowAddress] =
            flowRates[distributionFlowAddress] +
            actualFlowRate -
            oldFlowRate;
    }

    function _getDistributionFlowId(address from, ISuperTokenPool pool) internal view returns (bytes32) {
        return keccak256(abi.encode(block.chainid, "distributionflow", from, address(pool)));
    }

    function _encodeUniversalIndex(
        BasicParticle memory p
    ) private pure returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] = bytes32(
            (uint256(int256(FlowRate.unwrap(p.flow_rate))) << 128) |
                (uint256(Time.unwrap(p.settled_at)) << 96)
        );
        data[1] = bytes32(uint256(Value.unwrap(p.settled_value)));
    }

    function _getUniversalIndexData(
        ISuperfluidToken token,
        bytes32 universalIndexId
    ) private view returns (bool exists, BasicParticle memory particle) {
        bytes32[] memory data = token.getAgreementData(
            address(this),
            universalIndexId,
            2
        );

        uint256 wordA = uint256(data[0]);
        uint256 wordB = uint256(data[1]);

        exists = wordA > 0;

        if (exists) {
            particle = BasicParticle({
                settled_at: Time.wrap(uint32(wordA >> 96)),
                flow_rate: FlowRate.wrap(int96(int256(wordA))),
                settled_value: Value.wrap(int256(wordB))
            });
        }
    }

    function _updateUniversalIndex(
        ISuperfluidToken token,
        bytes32 universalIndexId,
        BasicParticle memory particle
    ) private {
        bytes32[] memory data = _encodeUniversalIndex(particle);
        token.updateAgreementData(universalIndexId, data);
    }
}
