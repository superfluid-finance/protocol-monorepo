// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
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
} from "../libs/SemanticMoney.sol";
import {SuperTokenPool} from "../superfluid/SuperTokenPool.sol";
import {
    IGeneralDistributionAgreementV1,
    ISuperfluidToken
} from "../interfaces/agreements/IGeneralDistributionAgreementV1.sol";
import { ISuperTokenPool } from "../interfaces/superfluid/ISuperTokenPool.sol";
import { AgreementBase } from "./AgreementBase.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";

type FlowId is uint32;

/**
 * @title General Distribution Agreement
 * @author Superfluid
 * @notice
 */
contract GeneralDistributionAgreementV1 is AgreementBase, IGeneralDistributionAgreementV1 {
    using EnumerableSet for EnumerableSet.AddressSet;
    using SemanticMoney for BasicParticle;

    mapping(address owner => EnumerableSet.AddressSet) internal _connectionsMap;
    mapping(bytes32 flowAddress => FlowRate) public flowRates;
    mapping(ISuperTokenPool pool => bool exist) public pools;

    constructor(ISuperfluid host) AgreementBase(address(host)) {}

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(ISuperfluidToken token, address account, uint256 time)
        external
        view
        override
        returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
    {
        (, BasicParticle memory uIndexData) = _getUniversalIndexData(token, _getUniversalIndexId(account));

        Value x = uIndexData.rtb(Time.wrap(uint32(time)));

        // pending distributions from pool
        if (pools[ISuperTokenPool(account)]) {
            // NB! Please ask solidity designer why "+=" is not derived for overloaded operator custom types
            x = x + Value.wrap(ISuperTokenPool(account).getPendingDistribution());
        }

        // pool-connected balance
        EnumerableSet.AddressSet storage connections = _connectionsMap[account];
        for (uint256 i = 0; i < connections.length(); ++i) {
            address p = connections.at(i);
            x = x + Value.wrap(ISuperTokenPool(p).getClaimable(uint32(time), account));
        }

        dynamicBalance = Value.unwrap(x) > int256(0) ? int256(Value.unwrap(x)) : int256(0);
    }

    function createPool(address admin, ISuperfluidToken token) external returns (SuperTokenPool pool) {
        pool = new SuperTokenPool(admin, GeneralDistributionAgreementV1(address(this)), token);
        pools[pool] = true;
        // why do I need to approve the pool to spend its own tokens?
    }

    // @note This is same as approveSubscription
    function connectPool(SuperTokenPool pool) external returns (bool success) {
        return connectPool(pool, true);
    }

    // @note This is same as revokeSubscription
    function disconnectPool(SuperTokenPool pool) external returns (bool success) {
        return connectPool(pool, false);
    }

    function connectPool(SuperTokenPool pool, bool doConnect) public returns (bool success) {
        if (doConnect) {
            if (!_connectionsMap[msg.sender].contains(address(pool))) {
                _connectionsMap[msg.sender].add(address(pool));
                assert(pool.operatorConnectMember(uint32(block.timestamp), msg.sender, true));
            }
        } else {
            if (_connectionsMap[msg.sender].contains(address(pool))) {
                _connectionsMap[msg.sender].remove(address(pool));
                assert(pool.operatorConnectMember(uint32(block.timestamp), msg.sender, false));
            }
        }
        success = true;
    }

    function isMemberConnected(address pool, address member) external view returns (bool) {
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

    function _getUniversalIndexId(address account) private pure returns (bytes32) {
        return keccak256(abi.encode(account));
    }

    function _shift(ISuperfluidToken token, address from, address to, int256 amount, bool checkAllowance) internal
        returns (bool success)
    {
        // require(!pools[ISuperTokenPool(to)], "Is a pool!");
        // require(Value.unwrap(amount) >= 0, "don't even try");
        address spender = msg.sender;
        // TODO: replace this with a supertoken check allowance
        // if (checkAllowance) _spendAllowance(from, spender, uint256(Value.unwrap(amount))); // FIXME SafeCast
        // Make updates
        bytes32 fromUniversalIndexId = _getUniversalIndexId(from);
        (, BasicParticle memory fromUIndexData) = _getUniversalIndexData(token, fromUniversalIndexId);
        
        bytes32 toUniversalIndexId = _getUniversalIndexId(to);
        (, BasicParticle memory toUIndexData) = _getUniversalIndexData(token, toUniversalIndexId);

        Value wrappedAmount = Value.wrap(amount);
        (fromUIndexData, toUIndexData) = fromUIndexData.shift2(toUIndexData, wrappedAmount);

        _updateUniversalIndex(token, fromUniversalIndexId, fromUIndexData);
        _updateUniversalIndex(token, toUniversalIndexId, toUIndexData);

        return true;
    }

    // This is the non-ERC20 version of instant transfer, that can trigger actions defined by "to"
    function shift(ISuperfluidToken token, address from, address to, int256 amount) external
        returns (bool success)
    {
        return _shift(token, from, to, amount, from != to);
    }

    function absorbParticleFromPool(ISuperfluidToken token, address account, BasicParticle memory p)
        public
        returns (bool)
    {
        (, BasicParticle memory accountParticle) = _getUniversalIndexData(token, _getUniversalIndexId(account));

        // new particle
        accountParticle = accountParticle.mappend(p);

        // update account particle
        _updateUniversalIndex(token, _getUniversalIndexId(account), accountParticle);
        return true;
    }

    function _encodeUniversalIndex(BasicParticle memory p) private pure returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] =
            bytes32((uint256(int256(FlowRate.unwrap(p.flow_rate))) << 128) | (uint256(Time.unwrap(p.settled_at)) << 96));
        data[1] = bytes32(uint256(Value.unwrap(p.settled_value)));
    }

    function _getPoolMemberData(ISuperfluidToken token, bytes32 poolMemberId)
        private
        view
        returns (bool exists, PDPoolMember memory member)
    {
        bytes32[] memory data = token.getAgreementData(address(this), poolMemberId, 2);

        uint256 wordA = uint256(data[0]);
        uint256 wordB = uint256(data[1]);

        exists = wordA > 0;

        if (exists) {
            member = PDPoolMember({
                owned_units: Unit.wrap(int128(int256(wordA >> 128))),
                synced_particle: BasicParticle({
                    settled_at: Time.wrap(uint32(wordA >> 96)),
                    flow_rate: FlowRate.wrap(int96(int256(wordA))),
                    settled_value: Value.wrap(int256(wordB))
                }),
                settled_value: Value.wrap(int256(wordB))
            });
        }
    }

    function _getUniversalIndexData(ISuperfluidToken token, bytes32 universalIndexId)
        private
        view
        returns (bool exists, BasicParticle memory particle)
    {
        bytes32[] memory data = token.getAgreementData(address(this), universalIndexId, 2);

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

    function _updateUniversalIndex(ISuperfluidToken token, bytes32 universalIndexId, BasicParticle memory particle)
        private
    {
        bytes32[] memory data = _encodeUniversalIndex(particle);
        token.updateAgreementData(universalIndexId, data);
    }

    function distribute(ISuperfluidToken token, SuperTokenPool pool, uint256 requestedAmount, bytes calldata ctx)
        external
        returns (bytes memory newCtx)
    {
        ISuperfluid.Context memory currentContext = AgreementLibrary.authorizeTokenAccess(token, ctx);
        newCtx = ctx;

        PDPoolIndex memory pdidx = pool.getIndex();

        bytes32 senderUniversalIndexId = _getUniversalIndexId(address(currentContext.msgSender));
        (, BasicParticle memory fromUIndexData) = _getUniversalIndexData(token, senderUniversalIndexId);

        // @note might be unsafe casting here from uint256 -> int256
        (fromUIndexData, pdidx,) = fromUIndexData.shift2(pdidx, Value.wrap(int256(requestedAmount)));
        _updateUniversalIndex(token, senderUniversalIndexId, fromUIndexData);
        assert(pool.operatorSetIndex(pdidx));
    }

    function distributeFlow(
        ISuperfluidToken token,
        SuperTokenPool pool,
        FlowId flowId,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary.authorizeTokenAccess(token, ctx);

        newCtx = ctx;

        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowAddress = keccak256(abi.encode(currentContext.msgSender, address(pool), flowId));

        bytes32 senderUniversalIndexId = _getUniversalIndexId(address(currentContext.msgSender));
        (, BasicParticle memory fromUIndexData) = _getUniversalIndexData(token, senderUniversalIndexId);

        FlowRate oldFlowRate = fromUIndexData.flow_rate.inv();
        PDPoolIndex memory pdidx = pool.getIndex();

        FlowRate actualFlowRate;
        (fromUIndexData, pdidx, actualFlowRate) =
            fromUIndexData.shiftFlow2b(pdidx, FlowRate.wrap(requestedFlowRate) - flowRates[flowAddress], t);
        pool.operatorSetIndex(pdidx);
        flowRates[flowAddress] = flowRates[flowAddress] + actualFlowRate - oldFlowRate;
    }
}
