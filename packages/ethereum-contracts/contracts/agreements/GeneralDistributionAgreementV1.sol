// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import {
    IBeacon
} from "@openzeppelin/contracts/proxy/beacon/UpgradeableBeacon.sol";

import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperApp,
    SuperAppDefinitions,
    ContextDefinitions,
    SuperfluidGovernanceConfigs
} from "../interfaces/superfluid/ISuperfluid.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {
    TokenMonad
} from "@superfluid-finance/solidity-semantic-money/src/TokenMonad.sol";
import { SuperTokenPool } from "../superfluid/SuperTokenPool.sol";
import {
    SuperTokenPoolDeployerLibrary
} from "../libs/SuperTokenPoolDeployerLibrary.sol";
import {
    IGeneralDistributionAgreementV1
} from "../interfaces/agreements/IGeneralDistributionAgreementV1.sol";
import {
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluidToken.sol";
import { ISuperTokenPool } from "../interfaces/superfluid/ISuperTokenPool.sol";
import { SlotsBitmapLibrary } from "../libs/SlotsBitmapLibrary.sol";
import { AgreementBase } from "./AgreementBase.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";

/**
 * @title General Distribution Agreement
 * @author Superfluid
 * @notice
 *
 * Storage Layout Notes
 * Agreement State
 * NOTE The Agreement State slot is computed with the following function:
 * keccak256(abi.encode("AgreementState", msg.sender, account, slotId))
 * slotId               = 0
 * msg.sender           = address of GDAv1
 * account              = pool address
 * Pool Agreement State stores the address of the pool which indicates existence.
 *
 * keccak256(abi.encode("AgreementState", msg.sender, account, slotId))
 * slotId               = 1
 * msg.sender           = address of GDAv1
 * account              = distributor
 * Distributor Agreement State stores the FlowDistributionData state (totals) for a distributor.
 *
 * Agreement Data
 * NOTE The Agreement Data slot is calculated with the following function:
 * keccak256(abi.encode("AgreementData", agreementClass, agreementId))
 * agreementClass       = address of GDAv1
 * agreementId          = UniversalIndexId | DistributionFlowId | PoolMemberId
 *
 * UniversalIndexId     =
 * keccak256(abi.encode(block.chainid, "universalIndex", account))
 * UniversalIndexId stores a BasicParticle struct for an `account`.
 *
 * DistributionFlowId   =
 * keccak256(abi.encode(block.chainid, "distributionFlow", from, pool))
 * DistributionFlowId stores FlowDistributionData between a sender (from) and pool.
 *
 * PoolMemberId         =
 * keccak256(abi.encode(block.chainid, "poolMember", member, pool))
 * PoolMemberId stores PoolMemberData for a member at a pool.
 */
contract GeneralDistributionAgreementV1 is
    AgreementBase,
    TokenMonad,
    IGeneralDistributionAgreementV1
{
    using SafeCast for uint256;
    using SemanticMoney for BasicParticle;

    address public constant SLOTS_BITMAP_LIBRARY_ADDRESS =
        address(SlotsBitmapLibrary);

    /// @dev Pool existence state slot id for storing whether a pool exists
    uint256 private constant _POOL_EXISTENCE_STATE_SLOT_ID = 0;
    /// @dev Distributor agreement state slot id for storing totalBuffer
    uint256 private constant _DISTRIBUTOR_AGREEMENT_STATE_SLOT_ID = 1;
    /// @dev Pool member state slot id for storing subs bitmap
    uint256 private constant _POOL_SUBS_BITMAP_STATE_SLOT_ID = 2;
    /// @dev Pool member state slot id starting point for pool connections
    uint256 private constant _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START =
        1 << 128;
    /// @dev CFAv1 PPP Config Key
    bytes32 private constant CFAV1_PPP_CONFIG_KEY =
        keccak256(
            "org.superfluid-finance.agreements.ConstantFlowAgreement.v1.PPPConfiguration"
        );

    struct FlowDistributionData {
        int96 flowRate;
        uint256 buffer; // stored as uint96
    }

    struct PoolMemberData {
        address pool;
        uint32 poolId; // the slot id in the pool's subs bitmap
    }

    IBeacon immutable superTokenPoolBeacon;

    constructor(
        ISuperfluid host,
        IBeacon superTokenPoolBeacon_
    ) AgreementBase(address(host)) {
        superTokenPoolBeacon = superTokenPoolBeacon_;
    }

    function realtimeBalanceVectorAt(
        ISuperfluidToken token,
        address account,
        uint256 time
    ) public view override returns (int256 available, int256 buffer) {
        BasicParticle memory uIndexData = _getUIndex(
            abi.encode(token),
            account
        );

        available = Value.unwrap(uIndexData.rtb(Time.wrap(uint32(time))));

        if (_isPool(token, account)) {
            available =
                available +
                ISuperTokenPool(account).getPendingDistribution();
        }

        {
            (
                uint32[] memory slotIds,
                bytes32[] memory pidList
            ) = _listPoolConnectionIds(token, account);
            for (uint256 i = 0; i < slotIds.length; ++i) {
                address pool = address(uint160(uint256(pidList[i])));
                (
                    bool exist,
                    PoolMemberData memory poolMemberData
                ) = _getPoolMemberData(token, account, ISuperTokenPool(pool));
                assert(exist);
                assert(poolMemberData.pool == pool);
                available =
                    available +
                    ISuperTokenPool(pool).getClaimable(uint32(time), account);
            }
        }

        (
            ,
            FlowDistributionData memory distributorAgreementState
        ) = _getDistributorAgreementState(token, account);

        buffer = int256(distributorAgreementState.buffer);
    }

    function realtimeBalanceOf(
        ISuperfluidToken token,
        address account,
        uint256 time
    )
        public
        view
        override
        returns (int256 rtb, uint256 buf, uint256 owedBuffer)
    {
        (int256 available, int256 buffer) = realtimeBalanceVectorAt(
            token,
            account,
            time
        );
        rtb = available - buffer;

        buf = uint256(buffer);
        owedBuffer = 0;
    }

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOfNow(
        ISuperfluidToken token,
        address account
    ) external view override returns (int256 rtb) {
        (rtb, , ) = realtimeBalanceOf(token, account, block.timestamp);
    }

    function getNetFlowRate(
        ISuperfluidToken token,
        address account
    ) external view override returns (int96 netFlowRate) {
        BasicParticle memory uIndexData = _getUIndex(
            abi.encode(token),
            account
        );
        netFlowRate = int96(FlowRate.unwrap(uIndexData._flow_rate));

        if (_isPool(token, account)) {
            netFlowRate =
                netFlowRate +
                ISuperTokenPool(account).getPendingDistributionFlowRate();
        }

        {
            (
                uint32[] memory slotIds,
                bytes32[] memory pidList
            ) = _listPoolConnectionIds(token, account);
            for (uint i = 0; i < slotIds.length; ++i) {
                ISuperTokenPool pool = ISuperTokenPool(
                    address(uint160(uint256(pidList[i])))
                );
                netFlowRate = netFlowRate + pool.getMemberFlowRate(account);
            }
        }
    }

    function getFlowRate(
        ISuperfluidToken token,
        address from,
        address to
    ) external view override returns (int96) {
        (, FlowDistributionData memory data) = _getFlowDistributionData(
            token,
            from,
            ISuperTokenPool(to)
        );
        return data.flowRate;
    }

    function getFlowDistributionActualFlowRate(
        ISuperfluidToken token,
        address from,
        ISuperTokenPool to,
        int96 requestedFlowRate
    ) external view override returns (int96 finalFlowRate) {
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 distributionFlowAddress = _getDistributionFlowId(from, to);

        BasicParticle memory fromUIndexData = _getUIndex(
            abi.encode(token),
            from
        );

        PDPoolIndex memory pdpIndex = _getPDPIndex("", address(to));

        FlowRate actualFlowRate;
        FlowRate flowRateDelta = FlowRate.wrap(requestedFlowRate) -
            _getFlowRate(abi.encode(token), distributionFlowAddress);
        (fromUIndexData, pdpIndex, actualFlowRate) = fromUIndexData
            .shift_flow2b(pdpIndex, flowRateDelta, t);
        finalFlowRate = int96(FlowRate.unwrap(actualFlowRate));
    }

    // test view function conditions where net flow rate makes sense given pending distribution
    // balance of the pool will capture pending flow rate

    function createPool(
        address admin,
        ISuperfluidToken token
    ) external override returns (ISuperTokenPool pool) {
        pool = ISuperTokenPool(
            address(
                SuperTokenPoolDeployerLibrary.deploy(
                    address(superTokenPoolBeacon),
                    admin,
                    GeneralDistributionAgreementV1(address(this)),
                    token
                )
            )
        );
        _setPoolAgreementState(token, address(pool));

        emit PoolCreated(token, admin, pool);
    }

    // @note This is same as approveSubscription
    function connectPool(
        ISuperTokenPool pool,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
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
        ISuperfluidToken token = pool.superToken();
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);
        address msgSender = currentContext.msgSender;
        newCtx = ctx;
        if (doConnect) {
            if (!isMemberConnected(token, address(pool), msgSender)) {
                assert(
                    pool.operatorConnectMember(
                        uint32(block.timestamp),
                        msgSender,
                        true
                    )
                );

                uint32 poolSlotId = _findAndFillPoolConnectionsBitmap(
                    token,
                    msgSender,
                    bytes32(uint256(uint160(address(pool))))
                );

                token.updateAgreementData(
                    _getPoolMemberId(msgSender, pool),
                    _encodePoolMemberData(
                        PoolMemberData({
                            poolId: poolSlotId,
                            pool: address(pool)
                        })
                    )
                );
            }
        } else {
            if (isMemberConnected(token, address(pool), msgSender)) {
                assert(
                    pool.operatorConnectMember(
                        uint32(block.timestamp),
                        msgSender,
                        false
                    )
                );
                (, PoolMemberData memory poolMemberData) = _getPoolMemberData(
                    token,
                    msgSender,
                    pool
                );
                _clearPoolConnectionsBitmap(
                    token,
                    msgSender,
                    poolMemberData.poolId
                );
            }
        }

        emit PoolConnectionUpdated(token, msgSender, pool, doConnect);
    }

    function isMemberConnected(
        ISuperfluidToken token,
        address pool,
        address member
    ) public view override returns (bool) {
        (bool exist, ) = _getPoolMemberData(
            token,
            member,
            ISuperTokenPool(pool)
        );
        return exist;
    }

    function absorbParticlesFromPool(
        ISuperfluidToken token,
        address[] calldata accounts,
        BasicParticle[] calldata ps
    ) public returns (bool) {
        if (_isPool(token, msg.sender) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }
        assert(accounts.length == ps.length);

        bytes memory eff = abi.encode(token);
        for (uint i = 0; i < accounts.length; i++) {
            BasicParticle memory accountParticle = _getUIndex(eff, accounts[i]);

            // update account particle
            _setUIndex(eff, accounts[i], accountParticle.mappend(ps[i]));
        }
        return true;
    }

    function distribute(
        ISuperfluidToken token,
        ISuperTokenPool pool,
        uint256 requestedAmount,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        if (_isPool(token, address(pool)) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }

        if (requestedAmount < 0) {
            revert GDA_NO_NEGATIVE_DISTRIBUTION();
        }

        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);
        newCtx = ctx;

        (, Value actualAmount) = _doDistribute(
            abi.encode(token),
            currentContext.msgSender,
            address(pool),
            Value.wrap(requestedAmount.toInt256())
        );

        emit InstantDistributionUpdated(
            token,
            pool,
            currentContext.msgSender,
            uint32(block.timestamp),
            requestedAmount,
            uint256(Value.unwrap(actualAmount)) // upcast from int256 -> uint256 is safe
        );
    }

    function distributeFlow(
        ISuperfluidToken token,
        ISuperTokenPool to,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        if (_isPool(token, address(to)) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }
        if (requestedFlowRate < 0) {
            revert GDA_NO_NEGATIVE_FLOW_RATE();
        }

        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);

        newCtx = ctx;

        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 distributionFlowAddress = _getDistributionFlowId(
            currentContext.msgSender,
            to
        );

        // @note it would be nice to have oldflowRate returned from _doDistributeFlow
        BasicParticle memory fromUIndexData = _getUIndex(
            abi.encode(token),
            currentContext.msgSender
        );
        FlowRate oldFlowRate = fromUIndexData._flow_rate.inv();

        (, FlowRate actualFlowRate) = _doDistributeFlow(
            abi.encode(token),
            currentContext.msgSender,
            address(to),
            distributionFlowAddress,
            FlowRate.wrap(requestedFlowRate),
            t
        );

        {
            _adjustBuffer(
                abi.encode(token),
                currentContext.msgSender,
                distributionFlowAddress,
                oldFlowRate,
                actualFlowRate
            );
        }

        {
            (int256 availableBalance, , ) = token.realtimeBalanceOf(
                currentContext.msgSender,
                currentContext.timestamp
            );

            if (availableBalance < 0) revert GDA_INSUFFICIENT_BALANCE();
        }

        {
            emit FlowDistributionUpdated(
                token,
                to,
                currentContext.msgSender,
                uint32(block.timestamp),
                int96(FlowRate.unwrap(oldFlowRate)),
                int96(FlowRate.unwrap(actualFlowRate))
            );
        }
    }

    function _adjustBuffer(
        bytes memory eff,
        address from,
        bytes32 flowHash,
        FlowRate oldFlowRate,
        FlowRate newFlowRate
    ) internal returns (bytes memory) {
        address token = abi.decode(eff, (address));
        // not using oldFlowRate in this model
        // surprising effect: reducing flow rate may require more buffer when liquidation_period adjusted upward
        ISuperfluidGovernance gov = ISuperfluidGovernance(
            ISuperfluid(_host).getGovernance()
        );
        uint256 pppConfig = gov.getConfigAsUint256(
            ISuperfluid(_host),
            ISuperfluidToken(token),
            CFAV1_PPP_CONFIG_KEY
        );
        (uint256 liquidationPeriod, ) = SuperfluidGovernanceConfigs
            .decodePPPConfig(pppConfig);

        (
            ,
            FlowDistributionData memory flowDistributionData
        ) = _getFlowDistributionData(ISuperfluidToken(token), flowHash);

        // @note downcasting from uint256 -> uint32 for liquidation period
        Value newBufferAmount = newFlowRate.mul(
            Time.wrap(uint32(liquidationPeriod))
        );
        Value bufferDelta = newBufferAmount -
            Value.wrap(int256(uint256(flowDistributionData.buffer)));

        FlowRate flowRateDelta = newFlowRate - oldFlowRate;
        eff = _doShift(eff, from, address(this), bufferDelta);

        {
            bytes32[] memory data = _encodeFlowDistributionData(
                FlowDistributionData({
                    flowRate: int96(FlowRate.unwrap(newFlowRate)),
                    buffer: uint96(uint256(Value.unwrap(newBufferAmount)))
                })
            );

            ISuperfluidToken(token).updateAgreementData(flowHash, data);
        }

        {
            (
                ,
                FlowDistributionData memory distributorAgreementState
            ) = _getDistributorAgreementState(ISuperfluidToken(token), from);

            _setDistributorAgreementState(
                ISuperfluidToken(token),
                from,
                FlowDistributionData({
                    flowRate: distributorAgreementState.flowRate +
                        int96(FlowRate.unwrap(flowRateDelta)),
                    buffer: distributorAgreementState.buffer +
                        uint256(Value.unwrap(bufferDelta))
                })
            );
        }
        return eff;
    }

    // # Universal Index operations
    //
    // Universal Index packing:
    //
    // -------- ------------------ ------------------ ------------------
    // WORD 1: |     flowRate     |     settledAt    |       free       |
    // -------- ------------------ ------------------ ------------------
    //         |        96b       |       32b        |        128b      |
    // -------- ------------------ ------------------ ------------------
    // WORD 2: |                      settledValue                      |
    // -------- ------------------ ------------------ ------------------
    //         |                          256b                          |
    // -------- ------------------ ------------------ ------------------

    function _getUniversalIndexId(
        address account
    ) internal view returns (bytes32) {
        return keccak256(abi.encode(block.chainid, "universalIndex", account));
    }

    function _encodeUniversalIndex(
        BasicParticle memory p
    ) internal pure returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] = bytes32(
            (uint256(int256(FlowRate.unwrap(p._flow_rate))) << 160) |
                (uint256(Time.unwrap(p._settled_at)) << 96)
        );
        data[1] = bytes32(uint256(Value.unwrap(p._settled_value)));
    }

    function _decodeUniversalIndexData(
        bytes32[] memory data
    ) internal pure returns (bool exists, BasicParticle memory particle) {
        uint256 wordA = uint256(data[0]);
        uint256 wordB = uint256(data[1]);

        exists = wordA > 0;

        if (exists) {
            particle = BasicParticle({
                _flow_rate: FlowRate.wrap(
                    int96(
                        int256(wordA >> 160) & int256(uint256(type(uint96).max))
                    )
                ),
                _settled_at: Time.wrap(uint32(wordA >> 96)),
                _settled_value: Value.wrap(int256(wordB))
            });
        }
    }

    // TokenMonad virtual functions
    function _getUIndex(
        bytes memory eff,
        address owner
    ) internal view override returns (BasicParticle memory uIndex) {
        address token = abi.decode(eff, (address));
        bytes32[] memory data = ISuperfluidToken(token).getAgreementData(
            address(this),
            _getUniversalIndexId(owner),
            2
        );
        (, uIndex) = _decodeUniversalIndexData(data);
    }

    function _setUIndex(
        bytes memory eff,
        address owner,
        BasicParticle memory p
    ) internal override returns (bytes memory) {
        address token = abi.decode(eff, (address));
        ISuperfluidToken(token).updateAgreementData(
            _getUniversalIndexId(owner),
            _encodeUniversalIndex(p)
        );

        emit UniversalIndexUpdated(
            ISuperfluidToken(token),
            owner,
            Time.unwrap(p._settled_at),
            Value.unwrap(p._settled_value),
            int96(FlowRate.unwrap(p._flow_rate))
        );

        return eff;
    }

    function _getPDPIndex(
        bytes memory eff,
        address pool
    ) internal view override returns (PDPoolIndex memory) {
        return SuperTokenPool(pool).getIndex();
    }

    function _setPDPIndex(
        bytes memory eff,
        address pool,
        PDPoolIndex memory p
    ) internal override returns (bytes memory) {
        assert(SuperTokenPool(pool).operatorSetIndex(p));

        return eff;
    }

    function _getFlowRate(
        bytes memory eff,
        bytes32 distributionFlowId
    ) internal view override returns (FlowRate) {
        address token = abi.decode(eff, (address));
        (, FlowDistributionData memory data) = _getFlowDistributionData(
            ISuperfluidToken(token),
            distributionFlowId
        );
        return FlowRate.wrap(data.flowRate);
    }

    function _setFlowInfo(
        bytes memory eff,
        bytes32 flowHash,
        address from,
        address to,
        FlowRate newFlowRate,
        FlowRate flowRateDelta
    ) internal override returns (bytes memory) {
        // @note it would be nice to have int96 as part of this interface for buffer
        address token = abi.decode(eff, (address));
        (
            ,
            FlowDistributionData memory flowDistributionData
        ) = _getFlowDistributionData(ISuperfluidToken(token), flowHash);

        bytes32[] memory data = _encodeFlowDistributionData(
            FlowDistributionData({
                flowRate: int96(FlowRate.unwrap(newFlowRate)),
                buffer: flowDistributionData.buffer
            })
        );

        ISuperfluidToken(token).updateAgreementData(flowHash, data);

        return eff;
    }

    // Pool data packing:
    // -------- ---------- -------------
    // WORD A: | reserved | poolAddress |
    // -------- ---------- -------------
    //         |    96    |     160     |
    // -------- ---------- -------------
    function _encodePoolExistenceData(
        address pool
    ) internal pure returns (bytes32[] memory data) {
        data = new bytes32[](1);
        data[0] = bytes32(uint256(uint160(pool)));
    }

    function _decodePoolExistenceData(
        uint256 data
    ) internal pure returns (bool exist, address pool) {
        exist = data > 0;
        if (exist) {
            pool = address(uint160(data));
        }
    }

    function _isPool(
        ISuperfluidToken token,
        address pool
    ) internal view virtual returns (bool exists) {
        exists = _getPoolAgreementState(token, pool);
    }

    function _setPoolAgreementState(
        ISuperfluidToken token,
        address pool
    ) internal {
        bytes32[] memory data = _encodePoolExistenceData(pool);
        token.updateAgreementStateSlot(
            pool,
            _POOL_EXISTENCE_STATE_SLOT_ID,
            data
        );
    }

    function _getPoolAgreementState(
        ISuperfluidToken token,
        address pool
    ) internal view returns (bool exist) {
        bytes32[] memory data = token.getAgreementStateSlot(
            address(this),
            pool,
            0,
            1
        );
        (exist, ) = _decodePoolExistenceData(uint256(data[0]));
    }

    // Distributor Agreement State packing:
    // -------- ---------- ---------- --------
    // WORD A: | reserved | flowRate | buffer |
    // -------- ---------- ---------- --------
    //         |    64    |    96    |   96   |
    // -------- ---------- ---------- --------

    function _getDistributorAgreementState(
        ISuperfluidToken token,
        address distributor
    )
        internal
        view
        returns (bool exist, FlowDistributionData memory flowDistributionData)
    {
        bytes32[] memory data = token.getAgreementStateSlot(
            address(this),
            distributor,
            _DISTRIBUTOR_AGREEMENT_STATE_SLOT_ID,
            1
        );
        (exist, flowDistributionData) = _decodeFlowDistributionData(
            uint256(data[0])
        );
    }

    function _setDistributorAgreementState(
        ISuperfluidToken token,
        address distributor,
        FlowDistributionData memory flowDistributionData
    ) internal {
        bytes32[] memory data = _encodeFlowDistributionData(
            flowDistributionData
        );
        token.updateAgreementStateSlot(
            distributor,
            _DISTRIBUTOR_AGREEMENT_STATE_SLOT_ID,
            data
        );
    }

    // FlowDistributionData data packing:
    // -------- ---------- ---------- --------
    // WORD A: | reserved | flowRate | buffer |
    // -------- ---------- ---------- --------
    //         |    64    |    96    |   96   |
    // -------- ---------- ---------- --------

    function _getDistributionFlowId(
        address from,
        ISuperTokenPool pool
    ) internal view returns (bytes32) {
        return
            keccak256(
                abi.encode(
                    block.chainid,
                    "distributionFlow",
                    from,
                    address(pool)
                )
            );
    }

    function _encodeFlowDistributionData(
        FlowDistributionData memory flowDistributionData
    ) internal pure returns (bytes32[] memory data) {
        data = new bytes32[](1);
        data[0] = bytes32(
            (uint256(uint96(flowDistributionData.flowRate)) << 96) |
                uint256(flowDistributionData.buffer)
        );
    }

    function _decodeFlowDistributionData(
        uint256 data
    )
        internal
        pure
        returns (bool exist, FlowDistributionData memory flowDistributionData)
    {
        exist = data > 0;
        if (exist) {
            flowDistributionData.buffer = uint96(
                data & uint256(type(uint96).max)
            );
            flowDistributionData.flowRate = int96(int256(data >> 96));
        }
    }

    function _getFlowDistributionData(
        ISuperfluidToken token,
        bytes32 distributionFlowId
    )
        internal
        view
        returns (bool exist, FlowDistributionData memory flowDistributionData)
    {
        bytes32[] memory data = token.getAgreementData(
            address(this),
            distributionFlowId,
            1
        );

        (exist, flowDistributionData) = _decodeFlowDistributionData(
            uint256(data[0])
        );
    }

    function _getFlowDistributionData(
        ISuperfluidToken token,
        address from,
        ISuperTokenPool pool
    )
        internal
        view
        returns (bool exist, FlowDistributionData memory flowDistributionData)
    {
        bytes32[] memory data = token.getAgreementData(
            address(this),
            _getDistributionFlowId(from, pool),
            1
        );

        (exist, flowDistributionData) = _decodeFlowDistributionData(
            uint256(data[0])
        );
    }

    // PoolMemberData data packing:
    // -------- ---------- -------- -------------
    // WORD A: | reserved | poolId | poolAddress |
    // -------- ---------- -------- -------------
    //         |    64    |   32   |     160     |
    // -------- ---------- -------- -------------

    function _getPoolMemberId(
        address poolMember,
        ISuperTokenPool pool
    ) internal view returns (bytes32) {
        return
            keccak256(
                abi.encode(
                    block.chainid,
                    "poolMember",
                    poolMember,
                    address(pool)
                )
            );
    }

    function _encodePoolMemberData(
        PoolMemberData memory poolMemberData
    ) internal pure returns (bytes32[] memory data) {
        data = new bytes32[](1);
        data[0] = bytes32(
            (uint256(uint32(poolMemberData.poolId)) << 160) |
                uint256(uint160(poolMemberData.pool))
        );
    }

    function _decodePoolMemberData(
        uint256 data
    ) internal pure returns (bool exist, PoolMemberData memory poolMemberData) {
        exist = data > 0;
        if (exist) {
            poolMemberData.pool = address(
                uint160(data & uint256(type(uint160).max))
            );
            poolMemberData.poolId = uint32(data >> 160);
        }
    }

    function _getPoolMemberData(
        ISuperfluidToken token,
        address poolMember,
        ISuperTokenPool pool
    ) internal view returns (bool exist, PoolMemberData memory poolMemberData) {
        bytes32[] memory data = token.getAgreementData(
            address(this),
            _getPoolMemberId(poolMember, pool),
            1
        );

        (exist, poolMemberData) = _decodePoolMemberData(uint256(data[0]));
    }

    // SlotsBitmap Pool Data:
    function _findAndFillPoolConnectionsBitmap(
        ISuperfluidToken token,
        address poolMember,
        bytes32 poolId
    ) private returns (uint32 slotId) {
        return
            SlotsBitmapLibrary.findEmptySlotAndFill(
                token,
                poolMember,
                _POOL_SUBS_BITMAP_STATE_SLOT_ID,
                _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START,
                poolId
            );
    }

    function _clearPoolConnectionsBitmap(
        ISuperfluidToken token,
        address poolMember,
        uint32 slotId
    ) private {
        SlotsBitmapLibrary.clearSlot(
            token,
            poolMember,
            _POOL_SUBS_BITMAP_STATE_SLOT_ID,
            slotId
        );
    }

    function _listPoolConnectionIds(
        ISuperfluidToken token,
        address subscriber
    ) private view returns (uint32[] memory slotIds, bytes32[] memory pidList) {
        (slotIds, pidList) = SlotsBitmapLibrary.listData(
            token,
            subscriber,
            _POOL_SUBS_BITMAP_STATE_SLOT_ID,
            _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START
        );
    }
}
