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
 *
 * Universal Index Data
 * slotId           = _UNIVERSAL_INDEX_STATE_SLOT_ID or 0
 * msg.sender       = address of GDAv1
 * account          = context.msgSender
 * Universal Index Data stores a Basic Particle for an account as well as the total buffer and
 * whether the account is a pool or not.
 *
 * SlotsBitmap Data
 * slotId           = _POOL_SUBS_BITMAP_STATE_SLOT_ID or 1
 * msg.sender       = address of GDAv1
 * account          = context.msgSender
 * Slots Bitmap Data Slot stores a bitmap of the slots that are "enabled" for a pool member.
 *
 * Pool Connections Data Slot Id Start
 * slotId (start)   = _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START or 1 << 128 or 340282366920938463463374607431768211456
 * msg.sender       = address of GDAv1
 * account          = context.msgSender
 * Pool Connections Data Slot Id Start indicates the starting slot for where we begin to store the pools that a
 * pool member is a part of.
 *
 *
 * Agreement Data
 * NOTE The Agreement Data slot is calculated with the following function:
 * keccak256(abi.encode("AgreementData", agreementClass, agreementId))
 * agreementClass       = address of GDAv1
 * agreementId          = DistributionFlowId | PoolMemberId
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
    using SafeCast for int256;
    using SemanticMoney for BasicParticle;

    address public constant SLOTS_BITMAP_LIBRARY_ADDRESS =
        address(SlotsBitmapLibrary);

    address public constant SUPER_TOKEN_POOL_DEPLOYER_ADDRESS =
        address(SuperTokenPoolDeployerLibrary);

    /// @dev Universal Index state slot id for storing universal index data
    uint256 private constant _UNIVERSAL_INDEX_STATE_SLOT_ID = 0;
    /// @dev Pool member state slot id for storing subs bitmap
    uint256 private constant _POOL_SUBS_BITMAP_STATE_SLOT_ID = 1;
    /// @dev Pool member state slot id starting point for pool connections
    uint256 private constant _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START =
        1 << 128;
    /// @dev CFAv1 PPP Config Key
    bytes32 private constant CFAV1_PPP_CONFIG_KEY =
        keccak256(
            "org.superfluid-finance.agreements.ConstantFlowAgreement.v1.PPPConfiguration"
        );

    bytes32 private constant SUPERTOKEN_MINIMUM_DEPOSIT_KEY =
        keccak256("org.superfluid-finance.superfluid.superTokenMinimumDeposit");

    struct UniversalIndexData {
        int96 flowRate;
        uint32 settledAt;
        uint256 totalBuffer;
        bool isPool;
        int256 settledValue;
    }

    struct FlowDistributionData {
        uint32 lastUpdated;
        int96 flowRate;
        uint256 buffer; // stored as uint96
    }

    struct PoolMemberData {
        address pool;
        uint32 poolID; // the slot id in the pool's subs bitmap
    }

    struct _StackVars_Liquidation {
        ISuperfluidToken token;
        int256 availableBalance;
        address sender;
        bytes32 distributionFlowID;
        int256 signedTotalGDADeposit;
        address liquidator;
    }

    IBeacon public superTokenPoolBeacon;

    constructor(ISuperfluid host) AgreementBase(address(host)) {}

    function initialize(IBeacon superTokenPoolBeacon_) external initializer {
        superTokenPoolBeacon = superTokenPoolBeacon_;
    }

    function realtimeBalanceVectorAt(
        ISuperfluidToken token,
        address account,
        uint256 time
    ) public view override returns (int256 available, int256 buffer) {
        UniversalIndexData memory universalIndexData = _getUIndexData(
            abi.encode(token),
            account
        );

        BasicParticle memory uIndexParticle = _getBasicParticleFromUIndex(
            universalIndexData
        );

        available = Value.unwrap(uIndexParticle.rtb(Time.wrap(uint32(time))));
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

        buffer = int256(universalIndexData.totalBuffer);
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
        bytes32 distributionFlowID = _getFlowDistributionID(from, to);
        (, FlowDistributionData memory data) = _getFlowDistributionData(
            token,
            distributionFlowID
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
        bytes32 distributionFlowID = _getFlowDistributionID(
            from,
            address(to)
        );

        BasicParticle memory fromUIndexData = _getUIndex(
            abi.encode(token),
            from
        );

        PDPoolIndex memory pdpIndex = _getPDPIndex("", address(to));

        FlowRate actualFlowRate;
        FlowRate flowRateDelta = FlowRate.wrap(requestedFlowRate) -
            _getFlowRate(abi.encode(token), distributionFlowID);
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
                    token
                )
            )
        );

        // @note We utilize the storage slot for Universal Index State
        // to store whether an account is a pool or not
        bytes32[] memory data = new bytes32[](1);
        data[0] = bytes32(uint256(1));
        token.updateAgreementStateSlot(
            address(pool),
            _UNIVERSAL_INDEX_STATE_SLOT_ID,
            data
        );

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

                uint32 poolSlotID = _findAndFillPoolConnectionsBitmap(
                    token,
                    msgSender,
                    bytes32(uint256(uint160(address(pool))))
                );

                token.updateAgreementData(
                    _getPoolMemberId(msgSender, pool),
                    _encodePoolMemberData(
                        PoolMemberData({
                            poolID: poolSlotID,
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
                token.terminateAgreement(_getPoolMemberId(msgSender, pool), 1);

                _clearPoolConnectionsBitmap(
                    token,
                    msgSender,
                    poolMemberData.poolID
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
            // update account particle
            _setUIndex(
                eff,
                accounts[i],
                _getUIndex(eff, accounts[i]).mappend(ps[i])
            );
        }
        return true;
    }

    function distribute(
        ISuperfluidToken token,
        address from,
        ISuperTokenPool pool,
        uint256 requestedAmount,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);

        newCtx = ctx;

        if (_isPool(token, address(pool)) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }

        if (requestedAmount < 0) {
            revert GDA_NO_NEGATIVE_DISTRIBUTION();
        }

        if (from != currentContext.msgSender) {
            revert GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED();
        }

        if (from != pool.admin()) {
            revert GDA_ONLY_ADMIN();
        }

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
        address from,
        ISuperTokenPool pool,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        if (_isPool(token, address(pool)) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }
        if (requestedFlowRate < 0) {
            revert GDA_NO_NEGATIVE_FLOW_RATE();
        }
        if (from != pool.admin()) {
            revert GDA_ONLY_ADMIN();
        }

        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);

        newCtx = ctx;

        bytes32 distributionFlowID = _getFlowDistributionID(
            from,
            address(pool)
        );

        // @note it would be nice to have oldflowRate returned from _doDistributeFlow
        UniversalIndexData memory fromUIndexData = _getUIndexData(
            abi.encode(token),
            from
        );

        BasicParticle memory basicParticle = _getBasicParticleFromUIndex(
            fromUIndexData
        );
        FlowRate oldFlowRate = basicParticle._flow_rate.inv();

        (, FlowRate actualFlowRate, FlowRate newDistributionFlowRate) = _doDistributeFlow(
            abi.encode(token),
            from,
            address(pool),
            distributionFlowID,
            FlowRate.wrap(requestedFlowRate),
            Time.wrap(uint32(block.timestamp))
        );

        {
            _adjustBuffer(
                abi.encode(token),
                from,
                distributionFlowID,
                oldFlowRate,
                actualFlowRate
            );
        }

        {
            (int256 availableBalance, , ) = token.realtimeBalanceOf(
                from,
                currentContext.timestamp
            );
            // distribute flow on behalf of someone else
            if (from != currentContext.msgSender) {
                if (requestedFlowRate > 0) {
                    // @note no ACL support for now
                    // revert if trying to distribute on behalf of others
                    revert GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED();
                } else {
                    // _StackVars_Liquidation used to handle good ol' stack too deep
                    _StackVars_Liquidation memory liquidationData;
                    {
                        liquidationData.token = token;
                        liquidationData.sender = from;
                        liquidationData.liquidator = currentContext.msgSender;
                        liquidationData
                            .distributionFlowID = distributionFlowID;
                        liquidationData.signedTotalGDADeposit = fromUIndexData
                            .totalBuffer
                            .toInt256();
                        liquidationData.availableBalance = availableBalance;
                    }
                    // closing stream on behalf of someone else: liquidation case
                    if (availableBalance < 0) {
                        _makeLiquidationPayouts(liquidationData);
                    } else {
                        revert GDA_NON_CRITICAL_SENDER();
                    }
                }
            } else {
                // from and msg.sender are the same
                if (requestedFlowRate > 0 && availableBalance < 0) {
                    revert GDA_INSUFFICIENT_BALANCE();
                }
            }
        }

        {
            emit FlowDistributionUpdated(
                token,
                pool,
                currentContext.msgSender,
                from,
                uint32(block.timestamp),
                int96(FlowRate.unwrap(oldFlowRate)),
                int96(FlowRate.unwrap(actualFlowRate))
            );
        }
    }

    function _makeLiquidationPayouts(
        _StackVars_Liquidation memory data
    ) internal {
        (
            ,
            FlowDistributionData memory flowDistributionData
        ) = _getFlowDistributionData(
                ISuperfluidToken(data.token),
                data.distributionFlowID
            );
        int256 signedSingleDeposit = flowDistributionData.buffer.toInt256();

        bytes memory liquidationTypeData;
        bool isCurrentlyPatricianPeriod;

        {
            (
                uint256 liquidationPeriod,
                uint256 patricianPeriod
            ) = _decode3PsData(data.token);
            isCurrentlyPatricianPeriod = _isPatricianPeriod(
                data.availableBalance,
                data.signedTotalGDADeposit,
                liquidationPeriod,
                patricianPeriod
            );
        }

        int256 totalRewardLeft = data.availableBalance +
            data.signedTotalGDADeposit;

        // critical case
        if (totalRewardLeft >= 0) {
            int256 rewardAmount = (signedSingleDeposit * totalRewardLeft) /
                data.signedTotalGDADeposit;
            liquidationTypeData = abi.encode(
                1,
                isCurrentlyPatricianPeriod ? 0 : 1
            );
            data.token.makeLiquidationPayoutsV2(
                data.distributionFlowID,
                liquidationTypeData,
                data.liquidator,
                isCurrentlyPatricianPeriod,
                data.sender,
                rewardAmount.toUint256(),
                rewardAmount * -1
            );
        } else {
            int256 rewardAmount = signedSingleDeposit;
            // bailout case
            data.token.makeLiquidationPayoutsV2(
                data.distributionFlowID,
                abi.encode(1, 2),
                data.liquidator,
                false,
                data.sender,
                rewardAmount.toUint256(),
                totalRewardLeft * -1
            );
        }
    }

    function _adjustBuffer(
        bytes memory eff,
        address from,
        bytes32 flowHash,
        FlowRate, // oldFlowRate,
        FlowRate newFlowRate
    ) internal returns (bytes memory) {
        address token = abi.decode(eff, (address));
        // not using oldFlowRate in this model
        // surprising effect: reducing flow rate may require more buffer when liquidation_period adjusted upward
        ISuperfluidGovernance gov = ISuperfluidGovernance(
            ISuperfluid(_host).getGovernance()
        );
        uint256 minimumDeposit = gov.getConfigAsUint256(
            ISuperfluid(msg.sender),
            ISuperfluidToken(token),
            SUPERTOKEN_MINIMUM_DEPOSIT_KEY
        );

        (uint256 liquidationPeriod, ) = _decode3PsData(ISuperfluidToken(token));

        (
            ,
            FlowDistributionData memory flowDistributionData
        ) = _getFlowDistributionData(ISuperfluidToken(token), flowHash);

        // @note downcasting from uint256 -> uint32 for liquidation period
        Value newBufferAmount = newFlowRate.mul(
            Time.wrap(uint32(liquidationPeriod))
        );

        if (
            Value.unwrap(newBufferAmount).toUint256() < minimumDeposit &&
            FlowRate.unwrap(newFlowRate) > 0
        ) {
            newBufferAmount = Value.wrap(minimumDeposit.toInt256());
        }

        Value bufferDelta = newBufferAmount -
            Value.wrap(int256(uint256(flowDistributionData.buffer)));

        eff = _doShift(eff, from, address(this), bufferDelta);

        {
            bytes32[] memory data = _encodeFlowDistributionData(
                FlowDistributionData({
                    lastUpdated: uint32(block.timestamp),
                    flowRate: int96(FlowRate.unwrap(newFlowRate)),
                    buffer: uint96(uint256(Value.unwrap(newBufferAmount)))
                })
            );

            ISuperfluidToken(token).updateAgreementData(flowHash, data);
        }

        UniversalIndexData memory universalIndexData = _getUIndexData(
            eff,
            from
        );
        universalIndexData.totalBuffer += uint96(
            uint256(Value.unwrap(bufferDelta))
        );
        ISuperfluidToken(token).updateAgreementStateSlot(
            from,
            _UNIVERSAL_INDEX_STATE_SLOT_ID,
            _encodeUniversalIndexData(universalIndexData)
        );

        return eff;
    }

    function _decode3PsData(
        ISuperfluidToken token
    )
        internal
        view
        returns (uint256 liquidationPeriod, uint256 patricianPeriod)
    {
        ISuperfluidGovernance gov = ISuperfluidGovernance(
            ISuperfluid(_host).getGovernance()
        );
        uint256 pppConfig = gov.getConfigAsUint256(
            ISuperfluid(_host),
            token,
            CFAV1_PPP_CONFIG_KEY
        );
        (liquidationPeriod, patricianPeriod) = SuperfluidGovernanceConfigs
            .decodePPPConfig(pppConfig);
    }

    function _isPatricianPeriod(
        int256 availableBalance,
        int256 signedTotalGDADeposit,
        uint256 liquidationPeriod,
        uint256 patricianPeriod
    ) internal pure returns (bool) {
        if (signedTotalGDADeposit == 0) {
            return false;
        }

        int256 totalRewardLeft = availableBalance + signedTotalGDADeposit;
        int256 totalGDAOutFlowrate = signedTotalGDADeposit /
            int256(liquidationPeriod);
        // divisor cannot be zero with existing outflow
        return
            totalRewardLeft / totalGDAOutFlowrate >
            int256(liquidationPeriod - patricianPeriod);
    }

    // # Universal Index operations
    //
    // Universal Index packing:
    // store buffer (96) and one bit to specify is pool in free
    // -------- ------------------ ------------------ ------------------ ------------------
    // WORD 1: |     flowRate     |     settledAt    |    totalBuffer   |      isPool      |
    // -------- ------------------ ------------------ ------------------ ------------------
    //         |        96b       |       32b        |       96b        |        32b       |
    // -------- ------------------ ------------------ ------------------ ------------------
    // WORD 2: |                                settledValue                               |
    // -------- ------------------ ------------------ ------------------ ------------------
    //         |                                    256b                                   |
    // -------- ------------------ ------------------ ------------------ ------------------

    function _encodeUniversalIndexData(
        BasicParticle memory p,
        uint256 buffer,
        bool isPool_
    ) internal pure returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] = bytes32(
            (uint256(int256(FlowRate.unwrap(p._flow_rate))) << 160) |
                (uint256(Time.unwrap(p._settled_at)) << 128) |
                (buffer << 32) |
                (isPool_ ? 1 : 0)
        );
        data[1] = bytes32(uint256(Value.unwrap(p._settled_value)));
    }

    function _encodeUniversalIndexData(
        UniversalIndexData memory uIndexData
    ) internal pure returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] = bytes32(
            (uint256(int256(uIndexData.flowRate)) << 160) |
                (uint256(uIndexData.settledAt) << 128) |
                (uint256(uIndexData.totalBuffer) << 32) |
                (uIndexData.isPool ? 1 : 0)
        );
        data[1] = bytes32(uint256(uIndexData.settledValue));
    }

    function _decodeUniversalIndexData(
        bytes32[] memory data
    )
        internal
        pure
        returns (bool exists, UniversalIndexData memory universalIndexData)
    {
        uint256 a = uint256(data[0]);
        uint256 b = uint256(data[1]);

        exists = a > 0 || b > 0;

        if (exists) {
            universalIndexData.flowRate = int96(
                int256(a >> 160) & int256(uint256(type(uint96).max))
            );
            universalIndexData.settledAt = uint32(
                uint256(a >> 128) & uint256(type(uint32).max)
            );
            universalIndexData.totalBuffer =
                uint256(a >> 32) &
                uint256(type(uint96).max);
            universalIndexData.isPool = ((a << 224) >> 224) & 1 == 1;
            universalIndexData.settledValue = int256(b);
        }
    }

    function _getUIndexData(
        bytes memory eff,
        address owner
    ) internal view returns (UniversalIndexData memory universalIndexData) {
        address token = abi.decode(eff, (address));
        bytes32[] memory data = ISuperfluidToken(token).getAgreementStateSlot(
            address(this),
            owner,
            _UNIVERSAL_INDEX_STATE_SLOT_ID,
            2
        );
        (, universalIndexData) = _decodeUniversalIndexData(data);
    }

    function _getBasicParticleFromUIndex(
        UniversalIndexData memory universalIndexData
    ) internal pure returns (BasicParticle memory particle) {
        particle._flow_rate = FlowRate.wrap(universalIndexData.flowRate);
        particle._settled_at = Time.wrap(universalIndexData.settledAt);
        particle._settled_value = Value.wrap(universalIndexData.settledValue);
    }

    // TokenMonad virtual functions
    function _getUIndex(
        bytes memory eff,
        address owner
    ) internal view override returns (BasicParticle memory uIndex) {
        address token = abi.decode(eff, (address));
        bytes32[] memory data = ISuperfluidToken(token).getAgreementStateSlot(
            address(this),
            owner,
            _UNIVERSAL_INDEX_STATE_SLOT_ID,
            2
        );
        (
            ,
            UniversalIndexData memory universalIndexData
        ) = _decodeUniversalIndexData(data);
        uIndex = _getBasicParticleFromUIndex(universalIndexData);
    }

    function _setUIndex(
        bytes memory eff,
        address owner,
        BasicParticle memory p
    ) internal override returns (bytes memory) {
        address token = abi.decode(eff, (address));
        // TODO see if this can be optimized, seems unnecessary to re-retrieve all the data
        // from storage to ensure totalBuffer and isPool isn't overriden
        UniversalIndexData memory universalIndexData = _getUIndexData(
            eff,
            owner
        );
        ISuperfluidToken(token).updateAgreementStateSlot(
            owner,
            _UNIVERSAL_INDEX_STATE_SLOT_ID,
            _encodeUniversalIndexData(
                p,
                universalIndexData.totalBuffer,
                universalIndexData.isPool
            )
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
        bytes memory, // eff,
        address pool
    ) internal view override returns (PDPoolIndex memory) {
        SuperTokenPool.PoolIndexData memory data = SuperTokenPool(pool)
            .getIndex();
        return SuperTokenPool(pool).getPDPoolIndexFromPoolIndexData(data);
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
        bytes32 distributionFlowID
    ) internal view override returns (FlowRate) {
        address token = abi.decode(eff, (address));
        (, FlowDistributionData memory data) = _getFlowDistributionData(
            ISuperfluidToken(token),
            distributionFlowID
        );
        return FlowRate.wrap(data.flowRate);
    }

    function _setFlowInfo(
        bytes memory eff,
        bytes32 flowHash,
        address, // from,
        address, // to,
        FlowRate newFlowRate,
        FlowRate // flowRateDelta
    ) internal override returns (bytes memory) {
        address token = abi.decode(eff, (address));
        (
            ,
            FlowDistributionData memory flowDistributionData
        ) = _getFlowDistributionData(ISuperfluidToken(token), flowHash);

        bytes32[] memory data = _encodeFlowDistributionData(
            FlowDistributionData({
                lastUpdated: uint32(block.timestamp),
                flowRate: int96(FlowRate.unwrap(newFlowRate)),
                buffer: flowDistributionData.buffer
            })
        );

        ISuperfluidToken(token).updateAgreementData(flowHash, data);

        return eff;
    }

    function isPool(
        ISuperfluidToken token,
        address account
    ) external view override returns (bool) {
        return _isPool(token, account);
    }

    function _isPool(
        ISuperfluidToken token,
        address account
    ) internal view returns (bool exists) {
        // @note see createPool, we retrieve the isPool bit from
        // UniversalIndex for this pool to determine whether the account
        // is a pool
        bytes32[] memory slotData = token.getAgreementStateSlot(
            address(this),
            account,
            _UNIVERSAL_INDEX_STATE_SLOT_ID,
            1
        );
        exists = ((uint256(slotData[0]) << 224) >> 224) & 1 == 1;
    }

    // FlowDistributionData data packing:
    // -------- ---------- ------------- ---------- --------
    // WORD A: | reserved | lastUpdated | flowRate | buffer |
    // -------- ---------- ------------- ---------- --------
    //         |    32    |      32     |    96    |   96   |
    // -------- ---------- ------------- ---------- --------

    function _getFlowDistributionID(
        address from,
        address to
    ) internal view returns (bytes32) {
        return
            keccak256(abi.encode(block.chainid, "distributionFlow", from, to));
    }

    function _encodeFlowDistributionData(
        FlowDistributionData memory flowDistributionData
    ) internal pure returns (bytes32[] memory data) {
        data = new bytes32[](1);
        data[0] = bytes32(
            (uint256(uint32(flowDistributionData.lastUpdated)) << 192) |
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
            flowDistributionData.lastUpdated = uint32(
                (data >> 192) & uint256(type(uint32).max)
            );
            flowDistributionData.flowRate = int96(int256(data >> 96));
            flowDistributionData.buffer = uint96(
                data & uint256(type(uint96).max)
            );
        }
    }

    function _getFlowDistributionData(
        ISuperfluidToken token,
        bytes32 distributionFlowID
    )
        internal
        view
        returns (bool exist, FlowDistributionData memory flowDistributionData)
    {
        bytes32[] memory data = token.getAgreementData(
            address(this),
            distributionFlowID,
            1
        );

        (exist, flowDistributionData) = _decodeFlowDistributionData(
            uint256(data[0])
        );
    }

    // PoolMemberData data packing:
    // -------- ---------- -------- -------------
    // WORD A: | reserved | poolID | poolAddress |
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
            (uint256(uint32(poolMemberData.poolID)) << 160) |
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
            poolMemberData.poolID = uint32(data >> 160);
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
        bytes32 poolID
    ) private returns (uint32 slotId) {
        return
            SlotsBitmapLibrary.findEmptySlotAndFill(
                token,
                poolMember,
                _POOL_SUBS_BITMAP_STATE_SLOT_ID,
                _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START,
                poolID
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
