// SPDX-License-Identifier: AGPLv3
// solhint-disable not-rely-on-time
pragma solidity 0.8.19;

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import { IBeacon } from "@openzeppelin/contracts/proxy/beacon/UpgradeableBeacon.sol";
import {
    BasicParticle,
    PDPoolIndex,
    SemanticMoney,
    Value,
    Time,
    FlowRate
} from "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import { ISuperfluid, ISuperfluidGovernance } from "../../interfaces/superfluid/ISuperfluid.sol";
import { SuperfluidPool } from "./SuperfluidPool.sol";
import { SuperfluidPoolDeployerLibrary } from "../../libs/SuperfluidPoolDeployerLibrary.sol";
import { IGeneralDistributionAgreementV1 } from "../../interfaces/agreements/gdav1/IGeneralDistributionAgreementV1.sol";
import { ISuperfluidToken } from "../../interfaces/superfluid/ISuperfluidToken.sol";
import { IConstantOutflowNFT } from "../../interfaces/superfluid/IConstantOutflowNFT.sol";
import { ISuperToken } from "../../interfaces/superfluid/ISuperToken.sol";
import { IPoolAdminNFT } from "../../interfaces/agreements/gdav1/IPoolAdminNFT.sol";
import { ISuperfluidPool } from "../../interfaces/agreements/gdav1/ISuperfluidPool.sol";
import { SlotsBitmapLibrary } from "../../libs/SlotsBitmapLibrary.sol";
import { SolvencyHelperLibrary } from "../../libs/SolvencyHelperLibrary.sol";
import { SafeGasLibrary } from "../../libs/SafeGasLibrary.sol";
import { AgreementBase } from "../AgreementBase.sol";
import { AgreementLibrary } from "../AgreementLibrary.sol";
import { DelegatableTokenMonad } from "./DelegatableTokenMonad.sol";

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
contract GeneralDistributionAgreementV1 is AgreementBase, IGeneralDistributionAgreementV1 {
    using SafeCast for uint256;
    using SafeCast for int256;
    using SemanticMoney for BasicParticle;

    address public constant SLOTS_BITMAP_LIBRARY_ADDRESS = address(SlotsBitmapLibrary);

    address public constant SUPERFLUID_POOL_DEPLOYER_ADDRESS = address(SuperfluidPoolDeployerLibrary);

    /// @dev Universal Index state slot id for storing universal index data
    uint256 private constant _UNIVERSAL_INDEX_STATE_SLOT_ID = 0;
    /// @dev Pool member state slot id for storing subs bitmap
    uint256 private constant _POOL_SUBS_BITMAP_STATE_SLOT_ID = 1;
    /// @dev Pool member state slot id starting point for pool connections
    uint256 private constant _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START = 1 << 128;
    /// @dev CFAv1 PPP Config Key
    bytes32 private constant CFAV1_PPP_CONFIG_KEY =
        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1.PPPConfiguration");

    bytes32 private constant SUPERTOKEN_MINIMUM_DEPOSIT_KEY =
        keccak256("org.superfluid-finance.superfluid.superTokenMinimumDeposit");

    address public immutable delegatableTokenMonad;

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

    struct StackVarsLiquidation {
        ISuperfluidToken token;
        int256 availableBalance;
        address sender;
        bytes32 distributionFlowHash;
        int256 signedTotalGDADeposit;
        address liquidator;
    }

    IBeacon public superfluidPoolBeacon;

    constructor(ISuperfluid host, address delegatableTokenMonad_) AgreementBase(address(host)) {
        delegatableTokenMonad = delegatableTokenMonad_;
    }

    function initialize(IBeacon superfluidPoolBeacon_) external initializer {
        superfluidPoolBeacon = superfluidPoolBeacon_;
    }

    function realtimeBalanceVectorAt(ISuperfluidToken token, address account, uint256 time)
        public
        view
        returns (int256 available, int256 fromPools, int256 buffer)
    {
        UniversalIndexData memory universalIndexData = _getUIndexData(abi.encode(token), account);

        if (isPool(token, account)) {
            available = ISuperfluidPool(account).getDisconnectedBalance(uint32(time));
        } else {
            available = Value.unwrap(_getBasicParticleFromUIndex(universalIndexData).rtb(Time.wrap(uint32(time))));
        }

        {
            (uint32[] memory slotIds, bytes32[] memory pidList) = _listPoolConnectionIds(token, account);
            for (uint256 i = 0; i < slotIds.length; ++i) {
                address pool = address(uint160(uint256(pidList[i])));
                (bool exist, PoolMemberData memory poolMemberData) =
                    _getPoolMemberData(token, account, ISuperfluidPool(pool));
                assert(exist);
                assert(poolMemberData.pool == pool);
                fromPools = fromPools + ISuperfluidPool(pool).getClaimable(account, uint32(time));
            }
        }

        buffer = universalIndexData.totalBuffer.toInt256();
    }

    function realtimeBalanceOf(ISuperfluidToken token, address account, uint256 time)
        public
        view
        override
        returns (int256 rtb, uint256 buf, uint256 owedBuffer)
    {
        (int256 available, int256 fromPools, int256 buffer) = realtimeBalanceVectorAt(token, account, time);
        rtb = available + fromPools;

        buf = uint256(buffer); // upcasting to uint256 is safe
        owedBuffer = 0;
    }

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOfNow(ISuperfluidToken token, address account)
        external
        view
        returns (int256 availableBalance, uint256 buffer, uint256 owedBuffer, uint256 timestamp)
    {
        (availableBalance, buffer, owedBuffer) = realtimeBalanceOf(token, account, block.timestamp);
        timestamp = block.timestamp;
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function getNetFlow(ISuperfluidToken token, address account) external view override returns (int96 netFlowRate) {
        netFlowRate = int256(FlowRate.unwrap(_getUIndex(abi.encode(token), account).flow_rate())).toInt96();

        if (isPool(token, account)) {
            netFlowRate += ISuperfluidPool(account).getTotalDisconnectedFlowRate();
        }

        {
            (uint32[] memory slotIds, bytes32[] memory pidList) = _listPoolConnectionIds(token, account);
            for (uint256 i = 0; i < slotIds.length; ++i) {
                ISuperfluidPool pool = ISuperfluidPool(address(uint160(uint256(pidList[i]))));
                netFlowRate += pool.getMemberFlowRate(account);
            }
        }
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function getFlowRate(ISuperfluidToken token, address from, ISuperfluidPool to)
        external
        view
        override
        returns (int96)
    {
        (, FlowDistributionData memory data) = _getFlowDistributionData(token, _getFlowDistributionHash(from, to));
        return data.flowRate;
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function estimateFlowDistributionActualFlowRate(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool to,
        int96 requestedFlowRate
    ) external view override returns (int96 actualFlowRate, int96 totalDistributionFlowRate) {
        bytes memory eff = abi.encode(token);
        bytes32 distributionFlowHash = _getFlowDistributionHash(from, to);

        BasicParticle memory fromUIndexData = _getUIndex(eff, from);

        PDPoolIndex memory pdpIndex = _getPDPIndex("", address(to));

        FlowRate oldFlowRate = _getFlowRate(eff, distributionFlowHash);
        FlowRate oldDistributionFlowRate = pdpIndex.flow_rate();
        FlowRate newDistributionFlowRate;
        FlowRate flowRateDelta = FlowRate.wrap(requestedFlowRate) - oldFlowRate;
        FlowRate currentAdjustmentFlowRate = FlowRate.wrap(getPoolAdjustmentFlowRate(address(to)));

        Time t = Time.wrap(uint32(block.timestamp));
        (fromUIndexData, pdpIndex, newDistributionFlowRate) =
            fromUIndexData.shift_flow2b(pdpIndex, flowRateDelta + currentAdjustmentFlowRate, t);

        actualFlowRate = int256(
            FlowRate.unwrap(
                oldFlowRate + (newDistributionFlowRate - oldDistributionFlowRate) - currentAdjustmentFlowRate
            )
        ).toInt96();
        totalDistributionFlowRate = int256(FlowRate.unwrap(newDistributionFlowRate)).toInt96();

        if (actualFlowRate < 0) {
            actualFlowRate = 0;
        }
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function estimateDistributionActualAmount(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool to,
        uint256 requestedAmount
    ) external view override returns (uint256 actualAmount) {
        bytes memory eff = abi.encode(token);

        (,, Value actualDistributionAmount) =
            _getUIndex(eff, from).shift2b(_getPDPIndex("", address(to)), Value.wrap(requestedAmount.toInt256()));

        actualAmount = uint256(Value.unwrap(actualDistributionAmount));
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function createPool(ISuperfluidToken token, address admin) external override returns (ISuperfluidPool pool) {
        // @note ensure if token and admin are the same that nothing funky happens with echidna
        if (admin == address(0)) revert GDA_NO_ZERO_ADDRESS_ADMIN();
        if (isPool(token, admin)) revert GDA_ADMIN_CANNOT_BE_POOL();

        pool =
            ISuperfluidPool(address(SuperfluidPoolDeployerLibrary.deploy(address(superfluidPoolBeacon), admin, token)));

        // @note We utilize the storage slot for Universal Index State
        // to store whether an account is a pool or not
        bytes32[] memory data = new bytes32[](1);
        data[0] = bytes32(uint256(1));
        token.updateAgreementStateSlot(address(pool), _UNIVERSAL_INDEX_STATE_SLOT_ID, data);

        IPoolAdminNFT poolAdminNFT = IPoolAdminNFT(_getPoolAdminNFTAddress(token));

        if (address(poolAdminNFT) != address(0)) {
            uint256 gasLeftBefore = gasleft();
            // solhint-disable-next-line no-empty-blocks
            try poolAdminNFT.mint(address(pool)) { }
            catch {
                SafeGasLibrary._revertWhenOutOfGas(gasLeftBefore);
            }
        }

        emit PoolCreated(token, admin, pool);
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function updateMemberUnits(ISuperfluidPool pool, address memberAddress, uint128 newUnits, bytes calldata ctx)
        external
        override
        returns (bytes memory newCtx)
    {
        // Only the admin can update member units here
        if (AgreementLibrary.authorizeTokenAccess(pool.superToken(), ctx).msgSender != pool.admin()) {
            revert GDA_NOT_POOL_ADMIN();
        }
        newCtx = ctx;

        pool.updateMemberUnits(memberAddress, newUnits);
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function claimAll(ISuperfluidPool pool, address memberAddress, bytes calldata ctx)
        external
        override
        returns (bytes memory newCtx)
    {
        AgreementLibrary.authorizeTokenAccess(pool.superToken(), ctx);
        newCtx = ctx;

        pool.claimAll(memberAddress);
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function connectPool(ISuperfluidPool pool, bytes calldata ctx) external override returns (bytes memory newCtx) {
        return connectPool(pool, true, ctx);
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function disconnectPool(ISuperfluidPool pool, bytes calldata ctx) external override returns (bytes memory newCtx) {
        return connectPool(pool, false, ctx);
    }

    // @note setPoolConnection function naming
    function connectPool(ISuperfluidPool pool, bool doConnect, bytes calldata ctx)
        public
        returns (bytes memory newCtx)
    {
        ISuperfluidToken token = pool.superToken();
        ISuperfluid.Context memory currentContext = AgreementLibrary.authorizeTokenAccess(token, ctx);
        address msgSender = currentContext.msgSender;
        newCtx = ctx;
        if (doConnect) {
            if (!isMemberConnected(token, address(pool), msgSender)) {
                assert(SuperfluidPool(address(pool)).operatorConnectMember(msgSender, true, uint32(block.timestamp)));

                // malicious token can reenter here
                // external call to untrusted contract
                // what sort of boundary can we trust

                // encodePoolMemberData
                bytes32[] memory data = new bytes32[](1);
                data[0] = bytes32(
                    (
                        uint256(
                            uint32(
                                _findAndFillPoolConnectionsBitmap(
                                    token, msgSender, bytes32(uint256(uint160(address(pool))))
                                )
                            )
                        ) << 160
                    ) | uint256(uint160(address(pool)))
                );

                token.createAgreement(_getPoolMemberHash(msgSender, pool), data);
            }
        } else {
            if (isMemberConnected(token, address(pool), msgSender)) {
                assert(SuperfluidPool(address(pool)).operatorConnectMember(msgSender, false, uint32(block.timestamp)));
                (, PoolMemberData memory poolMemberData) = _getPoolMemberData(token, msgSender, pool);
                token.terminateAgreement(_getPoolMemberHash(msgSender, pool), 1);

                SlotsBitmapLibrary.clearSlot(token, msgSender, _POOL_SUBS_BITMAP_STATE_SLOT_ID, poolMemberData.poolID);
            }
        }

        emit PoolConnectionUpdated(token, pool, msgSender, doConnect, currentContext.userData);
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function isMemberConnected(ISuperfluidToken token, address pool, address member)
        public
        view
        override
        returns (bool)
    {
        (bool exist,) = _getPoolMemberData(token, member, ISuperfluidPool(pool));
        return exist;
    }

    function isMemberConnected(ISuperfluidPool pool, address member) public view override returns (bool) {
        return isMemberConnected(pool.superToken(), address(pool), member);
    }

    function appendIndexUpdateByPool(ISuperfluidToken token, BasicParticle memory p, Time t) external returns (bool) {
        if (isPool(token, msg.sender) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }
        bytes memory eff = abi.encode(token);
        delegatableTokenMonad.delegatecall(
            abi.encodeCall(
                DelegatableTokenMonad(delegatableTokenMonad).setUIndex,
                (eff, msg.sender, _getUIndex(eff, msg.sender).mappend(p))
            )
        );
        _setPoolAdjustmentFlowRate(eff, msg.sender, true, /* doShift? */ p.flow_rate(), t);
        return true;
    }

    function poolSettleClaim(ISuperfluidToken superToken, address claimRecipient, int256 amount)
        external
        returns (bool)
    {
        if (isPool(superToken, msg.sender) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }

        delegatableTokenMonad.delegatecall(
            abi.encodeCall(
                DelegatableTokenMonad(delegatableTokenMonad).doShift,
                (abi.encode(superToken), msg.sender, claimRecipient, Value.wrap(amount))
            )
        );
        return true;
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function distribute(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool pool,
        uint256 requestedAmount,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary.authorizeTokenAccess(token, ctx);

        newCtx = ctx;

        if (isPool(token, address(pool)) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }

        if (from != currentContext.msgSender) {
            revert GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED();
        }

        (, bytes memory data) = delegatableTokenMonad.delegatecall(
            abi.encodeCall(
                DelegatableTokenMonad(delegatableTokenMonad).doDistributeViaPool,
                (abi.encode(token), currentContext.msgSender, address(pool), Value.wrap(requestedAmount.toInt256()))
            )
        );
        (, Value actualAmount) = abi.decode(data, (bytes, Value));

        if (token.isAccountCriticalNow(from)) {
            revert GDA_INSUFFICIENT_BALANCE();
        }

        // TODO: tokens are moving from sender => pool, including a transfer event makes sense here
        // trigger from the supertoken contract - @note this is possible since solc 0.8.21

        emit InstantDistributionUpdated(
            token,
            pool,
            from,
            currentContext.msgSender,
            requestedAmount,
            uint256(Value.unwrap(actualAmount)), // upcast from int256 -> uint256 is safe
            currentContext.userData
        );
    }

    struct StackThings {
        ISuperfluid.Context currentContext;
        bytes32 distributionFlowHash;
        FlowRate oldFlowRate;
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function distributeFlow(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool pool,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        if (isPool(token, address(pool)) == false) {
            revert GDA_ONLY_SUPER_TOKEN_POOL();
        }
        if (requestedFlowRate < 0) {
            revert GDA_NO_NEGATIVE_FLOW_RATE();
        }

        StackThings memory stackThings;
        {
            stackThings.currentContext = AgreementLibrary.authorizeTokenAccess(token, ctx);
            stackThings.distributionFlowHash = _getFlowDistributionHash(from, pool);
            stackThings.oldFlowRate = _getFlowRate(abi.encode(token), stackThings.distributionFlowHash);
        }
        newCtx = ctx;
        bytes memory delegateCallData;
        {
            (, bytes memory data) = delegatableTokenMonad.delegatecall(
                abi.encodeCall(
                    DelegatableTokenMonad(delegatableTokenMonad).doDistributeFlowViaPool,
                    (
                        abi.encode(token),
                        from,
                        address(pool),
                        stackThings.distributionFlowHash,
                        FlowRate.wrap(requestedFlowRate),
                        Time.wrap(uint32(block.timestamp))
                    )
                )
            );
            delegateCallData = data;
        }

        (, FlowRate actualFlowRate, FlowRate newDistributionFlowRate) =
            abi.decode(delegateCallData, (bytes, FlowRate, FlowRate));

        // handle distribute flow on behalf of someone else
        // @note move to internal maybe
        {
            if (from != stackThings.currentContext.msgSender) {
                if (requestedFlowRate > 0) {
                    // @note no ACL support for now
                    // revert if trying to distribute on behalf of others
                    revert GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED();
                } else {
                    // liquidation case, requestedFlowRate == 0
                    (int256 availableBalance,,) = token.realtimeBalanceOf(from, stackThings.currentContext.timestamp);
                    // StackVarsLiquidation used to handle good ol' stack too deep
                    StackVarsLiquidation memory liquidationData;
                    {
                        liquidationData.token = token;
                        liquidationData.sender = from;
                        liquidationData.liquidator = stackThings.currentContext.msgSender;
                        liquidationData.distributionFlowHash = stackThings.distributionFlowHash;
                        liquidationData.signedTotalGDADeposit =
                            _getUIndexData(abi.encode(token), from).totalBuffer.toInt256();
                        liquidationData.availableBalance = availableBalance;
                    }
                    // closing stream on behalf of someone else: liquidation case
                    if (availableBalance < 0) {
                        _makeLiquidationPayouts(liquidationData);
                    } else {
                        revert GDA_NON_CRITICAL_SENDER();
                    }
                }
            }
        }

        {
            _adjustBuffer(
                abi.encode(token),
                address(pool),
                from,
                stackThings.distributionFlowHash,
                stackThings.oldFlowRate,
                actualFlowRate
            );
        }

        // ensure sender has enough balance to execute transaction
        if (from == stackThings.currentContext.msgSender) {
            (int256 availableBalance,,) = token.realtimeBalanceOf(from, stackThings.currentContext.timestamp);
            // if from == msg.sender
            if (requestedFlowRate > 0 && availableBalance < 0) {
                revert GDA_INSUFFICIENT_BALANCE();
            }
        }

        // handleFlowNFT() - mint/burn FlowNFT to flow distributor
        {
            address constantOutflowNFTAddress = _getConstantOutflowNFTAddress(token);

            if (constantOutflowNFTAddress != address(0)) {
                uint256 gasLeftBefore;
                // create flow (mint)
                if (requestedFlowRate > 0 && FlowRate.unwrap(stackThings.oldFlowRate) == 0) {
                    gasLeftBefore = gasleft();
                    // solhint-disable-next-line no-empty-blocks
                    try IConstantOutflowNFT(constantOutflowNFTAddress).onCreate(token, from, address(pool)) { }
                    catch {
                        SafeGasLibrary._revertWhenOutOfGas(gasLeftBefore);
                    }
                }

                // update flow (update metadata)
                if (requestedFlowRate > 0 && FlowRate.unwrap(stackThings.oldFlowRate) > 0) {
                    gasLeftBefore = gasleft();
                    // solhint-disable-next-line no-empty-blocks
                    try IConstantOutflowNFT(constantOutflowNFTAddress).onUpdate(token, from, address(pool)) { }
                    catch {
                        SafeGasLibrary._revertWhenOutOfGas(gasLeftBefore);
                    }
                }

                // delete flow (burn)
                if (requestedFlowRate == 0) {
                    gasLeftBefore = gasleft();
                    // solhint-disable-next-line no-empty-blocks
                    try IConstantOutflowNFT(constantOutflowNFTAddress).onDelete(token, from, address(pool)) { }
                    catch {
                        SafeGasLibrary._revertWhenOutOfGas(gasLeftBefore);
                    }
                }
            }
        }

        {
            (address adjustmentFlowRecipient,, int96 adjustmentFlowRate) = getPoolAdjustmentFlowInfo(pool);

            emit FlowDistributionUpdated(
                token,
                pool,
                from,
                stackThings.currentContext.msgSender,
                int256(FlowRate.unwrap(stackThings.oldFlowRate)).toInt96(),
                int256(FlowRate.unwrap(actualFlowRate)).toInt96(),
                int256(FlowRate.unwrap(newDistributionFlowRate)).toInt96(),
                adjustmentFlowRecipient,
                adjustmentFlowRate,
                stackThings.currentContext.userData
            );
        }
    }

    /**
     * @notice Checks whether or not the NFT hook can be called.
     * @dev A staticcall, so `CONSTANT_OUTFLOW_NFT` must be a view otherwise the assumption is that it reverts
     * @param token the super token that is being streamed
     * @return constantOutflowNFTAddress the address returned by low level call
     */
    function _getConstantOutflowNFTAddress(ISuperfluidToken token)
        internal
        view
        returns (address constantOutflowNFTAddress)
    {
        // solhint-disable-next-line avoid-low-level-calls
        (bool success, bytes memory data) =
            address(token).staticcall(abi.encodeWithSelector(ISuperToken.CONSTANT_OUTFLOW_NFT.selector));

        if (success) {
            // @note We are aware this may revert if a Custom SuperToken's
            // CONSTANT_OUTFLOW_NFT does not return data that can be
            // decoded to an address. This would mean it was intentionally
            // done by the creator of the Custom SuperToken logic and is
            // fully expected to revert in that case as the author desired.
            constantOutflowNFTAddress = abi.decode(data, (address));
        }
    }

    function _getPoolAdminNFTAddress(ISuperfluidToken token) internal view returns (address poolAdminNFTAddress) {
        // solhint-disable-next-line avoid-low-level-calls
        (bool success, bytes memory data) =
            address(token).staticcall(abi.encodeWithSelector(ISuperToken.POOL_ADMIN_NFT.selector));

        if (success) {
            // @note We are aware this may revert if a Custom SuperToken's
            // POOL_ADMIN_NFT does not return data that can be
            // decoded to an address. This would mean it was intentionally
            // done by the creator of the Custom SuperToken logic and is
            // fully expected to revert in that case as the author desired.
            poolAdminNFTAddress = abi.decode(data, (address));
        }
    }

    function _makeLiquidationPayouts(StackVarsLiquidation memory data) internal {
        (, FlowDistributionData memory flowDistributionData) =
            _getFlowDistributionData(ISuperfluidToken(data.token), data.distributionFlowHash);
        int256 signedSingleDeposit = flowDistributionData.buffer.toInt256();

        bool isCurrentlyPatricianPeriod;

        {
            (uint256 liquidationPeriod, uint256 patricianPeriod) =
                SolvencyHelperLibrary.decode3PsData(ISuperfluid(_host), data.token);
            isCurrentlyPatricianPeriod = SolvencyHelperLibrary.isPatricianPeriod(
                data.availableBalance, data.signedTotalGDADeposit, liquidationPeriod, patricianPeriod
            );
        }

        int256 totalRewardLeft = data.availableBalance + data.signedTotalGDADeposit;

        // critical case
        if (totalRewardLeft >= 0) {
            int256 rewardAmount = (signedSingleDeposit * totalRewardLeft) / data.signedTotalGDADeposit;
            data.token.makeLiquidationPayoutsV2(
                data.distributionFlowHash,
                abi.encode(2, isCurrentlyPatricianPeriod ? 0 : 1), // liquidationTypeData
                data.liquidator,
                isCurrentlyPatricianPeriod,
                data.sender,
                rewardAmount.toUint256(),
                rewardAmount * -1
            );
        } else {
            // bailout case
            data.token.makeLiquidationPayoutsV2(
                data.distributionFlowHash,
                abi.encode(2, 2),
                data.liquidator,
                false,
                data.sender,
                signedSingleDeposit.toUint256(), // reward amount
                totalRewardLeft * -1
            );
        }
    }

    function _adjustBuffer(
        bytes memory eff,
        address pool,
        address from,
        bytes32 flowHash,
        FlowRate, // oldFlowRate,
        FlowRate newFlowRate
    ) internal returns (bytes memory) {
        address token = abi.decode(eff, (address));
        // not using oldFlowRate in this model
        // surprising effect: reducing flow rate may require more buffer when liquidation_period adjusted upward
        uint256 minimumDeposit = ISuperfluidGovernance(ISuperfluid(_host).getGovernance()).getConfigAsUint256(
            ISuperfluid(msg.sender), ISuperfluidToken(token), SUPERTOKEN_MINIMUM_DEPOSIT_KEY
        );

        (uint256 liquidationPeriod,) = SolvencyHelperLibrary.decode3PsData(ISuperfluid(_host), ISuperfluidToken(token));

        (, FlowDistributionData memory flowDistributionData) =
            _getFlowDistributionData(ISuperfluidToken(token), flowHash);

        // @note downcasting from uint256 -> uint32 for liquidation period
        Value newBufferAmount = newFlowRate.mul(Time.wrap(uint32(liquidationPeriod)));

        if (Value.unwrap(newBufferAmount).toUint256() < minimumDeposit && FlowRate.unwrap(newFlowRate) > 0) {
            newBufferAmount = Value.wrap(minimumDeposit.toInt256());
        }

        Value bufferDelta = newBufferAmount - Value.wrap(uint256(flowDistributionData.buffer).toInt256());

        bytes32[] memory encodedFlowDistributionData = new bytes32[](1);
        {
            // encodeFlowDistributionData
            encodedFlowDistributionData[0] = bytes32(
                (uint256(uint32(block.timestamp)) << 192) | (uint256(uint128(FlowRate.unwrap(newFlowRate))) << 96)
                    | uint256(Value.unwrap(newBufferAmount))
            );
        }
        ISuperfluidToken(token).updateAgreementData(flowHash, encodedFlowDistributionData);

        UniversalIndexData memory universalIndexData = _getUIndexData(eff, from);
        universalIndexData.totalBuffer =
        // new buffer
         (universalIndexData.totalBuffer.toInt256() + Value.unwrap(bufferDelta)).toUint256();

        bytes32[] memory encodedUniversalIndexData = new bytes32[](2);
        {
            // encodeUniversalIndexData
            encodedUniversalIndexData[0] = bytes32(
                (uint256(int256(universalIndexData.flowRate)) << 160) | (uint256(universalIndexData.settledAt) << 128)
                    | (uint256(universalIndexData.totalBuffer) << 32) | (universalIndexData.isPool ? 1 : 0)
            );
            encodedUniversalIndexData[1] = bytes32(uint256(universalIndexData.settledValue));
        }
        ISuperfluidToken(token).updateAgreementStateSlot(
            from, _UNIVERSAL_INDEX_STATE_SLOT_ID, encodedUniversalIndexData
        );

        {
            emit BufferAdjusted(
                ISuperfluidToken(token),
                ISuperfluidPool(pool),
                from,
                Value.unwrap(bufferDelta),
                Value.unwrap(newBufferAmount).toUint256(),
                universalIndexData.totalBuffer
            );
        }

        return eff;
    }

    // Solvency Related Getters
    function isPatricianPeriodNow(ISuperfluidToken token, address account)
        external
        view
        override
        returns (bool isCurrentlyPatricianPeriod, uint256 timestamp)
    {
        timestamp = ISuperfluid(_host).getNow();
        isCurrentlyPatricianPeriod = isPatricianPeriod(token, account, timestamp);
    }

    function isPatricianPeriod(ISuperfluidToken token, address account, uint256 timestamp)
        public
        view
        override
        returns (bool)
    {
        (int256 availableBalance,,) = token.realtimeBalanceOf(account, timestamp);
        if (availableBalance >= 0) {
            return true;
        }

        (uint256 liquidationPeriod, uint256 patricianPeriod) =
            SolvencyHelperLibrary.decode3PsData(ISuperfluid(_host), token);

        return SolvencyHelperLibrary.isPatricianPeriod(
            availableBalance,
            _getUIndexData(abi.encode(token), account).totalBuffer.toInt256(),
            liquidationPeriod,
            patricianPeriod
        );
    }

    // Hash Getters

    function _getPoolMemberHash(address poolMember, ISuperfluidPool pool) internal view returns (bytes32) {
        return keccak256(abi.encode(block.chainid, "poolMember", poolMember, address(pool)));
    }

    function _getFlowDistributionHash(address from, ISuperfluidPool to) internal view returns (bytes32) {
        return keccak256(abi.encode(block.chainid, "distributionFlow", from, to));
    }

    function _getPoolAdjustmentFlowHash(address from, address to) internal view returns (bytes32) {
        // this will never be in conflict with other flow has types
        return keccak256(abi.encode(block.chainid, "poolAdjustmentFlow", from, to));
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

    function _decodeUniversalIndexData(bytes32[] memory data)
        internal
        pure
        returns (bool exists, UniversalIndexData memory universalIndexData)
    {
        uint256 a = uint256(data[0]);
        uint256 b = uint256(data[1]);

        exists = a > 0 || b > 0;

        if (exists) {
            universalIndexData.flowRate = int96(int256(a >> 160) & int256(uint256(type(uint96).max)));
            universalIndexData.settledAt = uint32(uint256(a >> 128) & uint256(type(uint32).max));
            universalIndexData.totalBuffer = uint256(a >> 32) & uint256(type(uint96).max);
            universalIndexData.isPool = ((a << 224) >> 224) & 1 == 1;
            universalIndexData.settledValue = int256(b);
        }
    }

    function _getUIndexData(bytes memory eff, address owner)
        internal
        view
        returns (UniversalIndexData memory universalIndexData)
    {
        (, universalIndexData) = _decodeUniversalIndexData(
            ISuperfluidToken(abi.decode(eff, (address))).getAgreementStateSlot(
                address(this), owner, _UNIVERSAL_INDEX_STATE_SLOT_ID, 2
            )
        );
    }

    function _getBasicParticleFromUIndex(UniversalIndexData memory universalIndexData)
        internal
        pure
        returns (BasicParticle memory particle)
    {
        particle._flow_rate = FlowRate.wrap(universalIndexData.flowRate);
        particle._settled_at = Time.wrap(universalIndexData.settledAt);
        particle._settled_value = Value.wrap(universalIndexData.settledValue);
    }

    // TokenMonad virtual functions
    function _getUIndex(bytes memory eff, address owner) internal view returns (BasicParticle memory uIndex) {
        (, UniversalIndexData memory universalIndexData) = _decodeUniversalIndexData(
            ISuperfluidToken(abi.decode(eff, (address))).getAgreementStateSlot(
                address(this), owner, _UNIVERSAL_INDEX_STATE_SLOT_ID, 2
            )
        );
        uIndex = _getBasicParticleFromUIndex(universalIndexData);
    }

    function _getPDPIndex(
        bytes memory, // eff,
        address pool
    ) internal view returns (PDPoolIndex memory) {
        SuperfluidPool.PoolIndexData memory data = SuperfluidPool(pool).getIndex();
        return SuperfluidPool(pool).poolIndexDataToPDPoolIndex(data);
    }

    function _getFlowRate(bytes memory eff, bytes32 distributionFlowHash) internal view returns (FlowRate) {
        (, FlowDistributionData memory data) =
            _getFlowDistributionData(ISuperfluidToken(abi.decode(eff, (address))), distributionFlowHash);
        return FlowRate.wrap(data.flowRate);
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function getPoolAdjustmentFlowInfo(ISuperfluidPool pool)
        public
        view
        override
        returns (address adjustmentRecipient, bytes32 flowHash, int96 flowRate)
    {
        // pool admin is always the adjustment recipient
        adjustmentRecipient = pool.admin();
        flowHash = _getPoolAdjustmentFlowHash(address(pool), adjustmentRecipient);
        flowRate = int256(FlowRate.unwrap(_getFlowRate(abi.encode(pool.superToken()), flowHash))).toInt96();
    }

    function getPoolAdjustmentFlowRate(address pool) public view override returns (int96) {
        (,, int96 rawFlowRate) = getPoolAdjustmentFlowInfo(ISuperfluidPool(pool));
        return rawFlowRate;
    }

    function _setPoolAdjustmentFlowRate(bytes memory eff, address pool, bool doShiftFlow, FlowRate flowRate, Time t)
        internal
        returns (bytes memory)
    {
        // @note should this also always be
        address adjustmentRecipient = ISuperfluidPool(pool).admin();
        bytes32 adjustmentFlowHash = _getPoolAdjustmentFlowHash(pool, adjustmentRecipient);

        if (doShiftFlow) {
            flowRate = flowRate + _getFlowRate(eff, adjustmentFlowHash);
        }

        (, bytes memory data) = delegatableTokenMonad.delegatecall(
            abi.encodeCall(
                DelegatableTokenMonad(delegatableTokenMonad).doFlow,
                (eff, pool, adjustmentRecipient, adjustmentFlowHash, flowRate, t)
            )
        );

        return data;
    }

    /// @inheritdoc IGeneralDistributionAgreementV1
    function isPool(ISuperfluidToken token, address account) public view override returns (bool) {
        // @note see createPool, we retrieve the isPool bit from
        // UniversalIndex for this pool to determine whether the account
        // is a pool
        return (
            (uint256(token.getAgreementStateSlot(address(this), account, _UNIVERSAL_INDEX_STATE_SLOT_ID, 1)[0]) << 224)
                >> 224
        ) & 1 == 1;
    }

    // FlowDistributionData data packing:
    // -------- ---------- ------------- ---------- --------
    // WORD A: | reserved | lastUpdated | flowRate | buffer |
    // -------- ---------- ------------- ---------- --------
    //         |    32    |      32     |    96    |   96   |
    // -------- ---------- ------------- ---------- --------

    function _getFlowDistributionData(ISuperfluidToken token, bytes32 distributionFlowHash)
        internal
        view
        returns (bool exist, FlowDistributionData memory flowDistributionData)
    {
        uint256 data = uint256(token.getAgreementData(address(this), distributionFlowHash, 1)[0]);
        exist = data > 0;
        if (exist) {
            flowDistributionData.lastUpdated = uint32((data >> 192) & uint256(type(uint32).max));
            flowDistributionData.flowRate = int96(int256(data >> 96));
            flowDistributionData.buffer = uint96(data & uint256(type(uint96).max));
        }
    }

    // PoolMemberData data packing:
    // -------- ---------- -------- -------------
    // WORD A: | reserved | poolID | poolAddress |
    // -------- ---------- -------- -------------
    //         |    64    |   32   |     160     |
    // -------- ---------- -------- -------------

    function _getPoolMemberData(ISuperfluidToken token, address poolMember, ISuperfluidPool pool)
        internal
        view
        returns (bool exist, PoolMemberData memory poolMemberData)
    {
        uint256 data = uint256(token.getAgreementData(address(this), _getPoolMemberHash(poolMember, pool), 1)[0]);

        exist = data > 0;
        if (exist) {
            poolMemberData.pool = address(uint160(data & uint256(type(uint160).max)));
            poolMemberData.poolID = uint32(data >> 160);
        }
    }

    // SlotsBitmap Pool Data:
    function _findAndFillPoolConnectionsBitmap(ISuperfluidToken token, address poolMember, bytes32 poolID)
        private
        returns (uint32 slotId)
    {
        return SlotsBitmapLibrary.findEmptySlotAndFill(
            token, poolMember, _POOL_SUBS_BITMAP_STATE_SLOT_ID, _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START, poolID
        );
    }

    function _listPoolConnectionIds(ISuperfluidToken token, address subscriber)
        private
        view
        returns (uint32[] memory slotIds, bytes32[] memory pidList)
    {
        (slotIds, pidList) = SlotsBitmapLibrary.listData(
            token, subscriber, _POOL_SUBS_BITMAP_STATE_SLOT_ID, _POOL_CONNECTIONS_DATA_STATE_SLOT_ID_START
        );
    }
}
