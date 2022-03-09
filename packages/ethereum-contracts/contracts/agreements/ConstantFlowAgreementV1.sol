// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.12;

import {
    IConstantFlowAgreementV1,
    ISuperfluidToken
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperApp,
    SuperAppDefinitions,
    ContextDefinitions,
    SuperfluidGovernanceConfigs
} from "../interfaces/superfluid/ISuperfluid.sol";
import { AgreementBase } from "./AgreementBase.sol";

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import { Int96SafeMath } from "../libs/Int96SafeMath.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";


/**
 * @title ConstantFlowAgreementV1 contract
 * @author Superfluid
 * @dev Please read IConstantFlowAgreementV1 for implementation notes.
 * @dev For more technical notes, please visit protocol-monorepo wiki area.
 */
contract ConstantFlowAgreementV1 is
    AgreementBase,
    IConstantFlowAgreementV1
{

    bytes32 private constant CFAV1_PPP_CONFIG_KEY =
        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1.PPPConfiguration");

    bytes32 private constant SUPERTOKEN_MINIMUM_DEPOSIT_KEY =
        keccak256("org.superfluid-finance.superfluid.superTokenMinimumDeposit");

    using SafeCast for uint256;
    using SafeCast for int256;
    using Int96SafeMath for int96;

    struct FlowData {
        uint256 timestamp; // stored as uint32
        int96 flowRate; // stored also as int96
        uint256 deposit; // stored as int96 with lower 32 bits clipped to 0
        uint256 owedDeposit; // stored as int96 with lower 32 bits clipped to 0
    }

    struct FlowParams {
        bytes32 flowId;
        address sender;
        address receiver;
        int96 flowRate;
        bytes userData;
    }

    // solhint-disable-next-line no-empty-blocks
    constructor(ISuperfluid host) AgreementBase(address(host)) {}

    /**************************************************************************
     * ISuperAgreement interface
     *************************************************************************/

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
        (bool exist, FlowData memory state) = _getAccountFlowState(token, account);
        if(exist) {
            dynamicBalance = ((int256(time) - (int256(state.timestamp))) * state.flowRate);
            deposit = state.deposit;
            owedDeposit = state.owedDeposit;
        }
    }

    /**************************************************************************
     * IConstantFlowAgreementV1 interface
     *************************************************************************/

    /// @dev IConstantFlowAgreementV1.createFlow implementation
    function getMaximumFlowRateFromDeposit(
        ISuperfluidToken token,
        uint256 deposit)
        external view override
        returns (int96 flowRate)
    {
        require(deposit < 2**95, "CFA: deposit number too big");
        deposit = _clipDepositNumberRoundingDown(deposit);
        (uint256 liquidationPeriod, ) = _decode3PsData(token);
        uint256 flowrate1 = deposit / liquidationPeriod;

        // NOTE downcasting is safe as we constrain deposit to less than
        // 2 ** 95 so the resulting value flowRate1 will fit into int96
        return int96(int256(flowrate1));
    }

    function getDepositRequiredForFlowRate(
        ISuperfluidToken token,
        int96 flowRate)
        external view override
        returns (uint256 deposit)
    {
        require(flowRate >= 0, "CFA: not for negative flow rate");
        ISuperfluid host = ISuperfluid(token.getHost());
        ISuperfluidGovernance gov = ISuperfluidGovernance(host.getGovernance());
        uint256 minimumDeposit = gov.getConfigAsUint256(host, token, SUPERTOKEN_MINIMUM_DEPOSIT_KEY);
        uint256 pppConfig = gov.getConfigAsUint256(host, token, CFAV1_PPP_CONFIG_KEY);
        (uint256 liquidationPeriod, ) = SuperfluidGovernanceConfigs.decodePPPConfig(pppConfig);
        require(uint256(int256(flowRate))
            * liquidationPeriod <= uint256(int256(type(int96).max)), "CFA: flow rate too big");
        uint256 calculatedDeposit = _calculateDeposit(flowRate, liquidationPeriod);
        return calculatedDeposit < minimumDeposit && flowRate > 0 ? minimumDeposit : calculatedDeposit;
    }

    function isPatricianPeriodNow(
        ISuperfluidToken token, 
        address account)
        public view override
        returns (bool isCurrentlyPatricianPeriod, uint256 timestamp)
    {
        // solhint-disable-next-line not-rely-on-time
        timestamp = block.timestamp;
        isCurrentlyPatricianPeriod = isPatricianPeriod(token, account, timestamp);
    }

    function isPatricianPeriod(
        ISuperfluidToken token, 
        address account,
        uint256 timestamp)
        public view override
        returns (bool)
    {
        (int256 availableBalance, ,) = token.realtimeBalanceOf(account, timestamp);
        if (availableBalance >= 0) {
            return true;
        }

        (uint256 liquidationPeriod, uint256 patricianPeriod) = _decode3PsData(token);
        (,FlowData memory senderAccountState) = _getAccountFlowState(token, account);
        int256 signedTotalCFADeposit = senderAccountState.deposit.toInt256();

        return _isPatricianPeriod(
            availableBalance, 
            signedTotalCFADeposit, 
            liquidationPeriod, 
            patricianPeriod
        );
    }

    /// @dev IConstantFlowAgreementV1.createFlow implementation
    function createFlow(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        FlowParams memory flowParams;
        require(receiver != address(0), "CFA: receiver is zero");
        ISuperfluid.Context memory currentContext = AgreementLibrary.authorizeTokenAccess(token, ctx);
        flowParams.flowId = _generateFlowId(currentContext.msgSender, receiver);
        flowParams.sender = currentContext.msgSender;
        flowParams.receiver = receiver;
        flowParams.flowRate = flowRate;
        flowParams.userData = currentContext.userData;
        require(flowParams.sender != flowParams.receiver, "CFA: no self flow");
        require(flowParams.flowRate > 0, "CFA: invalid flow rate");
        (bool exist, FlowData memory oldFlowData) = _getAgreementData(token, flowParams.flowId);
        require(!exist, "CFA: flow already exist");

        if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver)))
        {
            newCtx = _changeFlowToApp(
                receiver,
                token, flowParams, oldFlowData,
                ctx, currentContext, FlowChangeType.CREATE_FLOW);
        } else {
            newCtx = _changeFlowToNonApp(
                token, flowParams, oldFlowData,
                ctx, currentContext);
        }

        _requireAvailableBalance(token, currentContext);
    }

    /// @dev IConstantFlowAgreementV1.updateFlow implementation
    function updateFlow(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        FlowParams memory flowParams;
        require(receiver != address(0), "CFA: receiver is zero");
        ISuperfluid.Context memory currentContext = AgreementLibrary.authorizeTokenAccess(token, ctx);
        flowParams.flowId = _generateFlowId(currentContext.msgSender, receiver);
        flowParams.sender = currentContext.msgSender;
        flowParams.receiver = receiver;
        flowParams.flowRate = flowRate;
        flowParams.userData = currentContext.userData;
        require(flowParams.sender != flowParams.receiver, "CFA: no self flow");
        require(flowParams.flowRate > 0, "CFA: invalid flow rate");
        (bool exist, FlowData memory oldFlowData) = _getAgreementData(token, flowParams.flowId);
        require(exist, "CFA: flow does not exist");

        if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver))) {
            newCtx = _changeFlowToApp(
                receiver,
                token, flowParams, oldFlowData,
                ctx, currentContext, FlowChangeType.UPDATE_FLOW);
        } else {
            newCtx = _changeFlowToNonApp(
                token, flowParams, oldFlowData,
                ctx, currentContext);
        }

        _requireAvailableBalance(token, currentContext);
    }

    /// @dev IConstantFlowAgreementV1.deleteFlow implementation
    function deleteFlow(
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        FlowParams memory flowParams;
        require(sender != address(0), "CFA: sender is zero");
        require(receiver != address(0), "CFA: receiver is zero");
        ISuperfluid.Context memory currentContext = AgreementLibrary.authorizeTokenAccess(token, ctx);
        flowParams.flowId = _generateFlowId(sender, receiver);
        flowParams.sender = sender;
        flowParams.receiver = receiver;
        flowParams.flowRate = 0;
        flowParams.userData = currentContext.userData;
        (bool exist, FlowData memory oldFlowData) = _getAgreementData(token, flowParams.flowId);
        require(exist, "CFA: flow does not exist");

        int256 availableBalance;
        (availableBalance,,) = token.realtimeBalanceOf(sender, currentContext.timestamp);

        // delete should only be called by sender or receiver
        // unless it is a liquidation (availale balance < 0)
        if (currentContext.msgSender != sender && currentContext.msgSender != receiver) {
            // liquidation should only for sender that is critical, unless sender or receiver is a jailed app
            if (!ISuperfluid(msg.sender).isAppJailed(ISuperApp(sender)) &&
                !ISuperfluid(msg.sender).isAppJailed(ISuperApp(receiver))) {
                require(availableBalance < 0, "CFA: sender account is not critical");
            }
        }

        if (availableBalance < 0) {
            _makeLiquidationPayouts(
                token,
                availableBalance,
                flowParams,
                oldFlowData,
                currentContext.msgSender);
        }

        newCtx = ctx;
        if (currentContext.msgSender == sender) {
            if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver))) {
                newCtx = _changeFlowToApp(
                    receiver,
                    token, flowParams, oldFlowData,
                    newCtx, currentContext, FlowChangeType.DELETE_FLOW);
            } else {
                newCtx = _changeFlowToNonApp(
                    token, flowParams, oldFlowData,
                    newCtx, currentContext);
            }
        } else if (currentContext.msgSender == receiver) {
            if (ISuperfluid(msg.sender).isApp(ISuperApp(sender))) {
                newCtx = _changeFlowToApp(
                    sender,
                    token, flowParams, oldFlowData,
                    newCtx, currentContext, FlowChangeType.DELETE_FLOW);
            } else if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver))) {
                newCtx = _changeFlowToApp(
                    address(0),
                    token, flowParams, oldFlowData,
                    newCtx, currentContext, FlowChangeType.DELETE_FLOW);
            } else {
                newCtx = _changeFlowToNonApp(
                    token, flowParams, oldFlowData,
                    newCtx, currentContext);
            }
        } else /* liquidations */ {
            // if the sender is an app, and becomes critical
            if (ISuperfluid(msg.sender).isApp(ISuperApp(sender))) {
                newCtx = ISuperfluid(msg.sender).jailApp(
                    newCtx,
                    ISuperApp(sender),
                    SuperAppDefinitions.APP_RULE_NO_CRITICAL_SENDER_ACCOUNT);
            }
            // always attempt to call receiver callback
            if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver))) {
                newCtx = _changeFlowToApp(
                    receiver,
                    token, flowParams, oldFlowData,
                    newCtx, currentContext, FlowChangeType.DELETE_FLOW);
            } else {
                newCtx = _changeFlowToNonApp(
                    token, flowParams, oldFlowData,
                    newCtx, currentContext);
            }
        }
    }

    /// @dev IConstantFlowAgreementV1.getFlow implementation
    function getFlow(
        ISuperfluidToken token,
        address sender,
        address receiver
    )
        external
        view
        override
        returns (
            uint256 timestamp,
            int96 flowRate,
            uint256 deposit,
            uint256 owedDeposit
        )
    {
        (, FlowData memory data) = _getAgreementData(
            token,
            _generateFlowId(sender, receiver));

        return(
            data.timestamp,
            data.flowRate,
            data.deposit,
            data.owedDeposit
        );
    }

    /// @dev IConstantFlowAgreementV1.getFlow implementation
    function getFlowByID(
        ISuperfluidToken token,
        bytes32 flowId
    )
        external
        view
        override
        returns(
            uint256 timestamp,
            int96 flowRate,
            uint256 deposit,
            uint256 owedDeposit
        )
    {
        (, FlowData memory data) = _getAgreementData(
            token,
            flowId
        );

        return (
            data.timestamp,
            data.flowRate,
            data.deposit,
            data.owedDeposit
        );
    }

    /// @dev IConstantFlowAgreementV1.getAccountFlowInfo implementation
    function getAccountFlowInfo(
        ISuperfluidToken token,
        address account
    )
        external view override
        returns (
            uint256 timestamp,
            int96 flowRate,
            uint256 deposit,
            uint256 owedDeposit)
    {
        (, FlowData memory state) = _getAccountFlowState(token, account);
        return (
            state.timestamp,
            state.flowRate,
            state.deposit,
            state.owedDeposit
        );
    }

    /// @dev IConstantFlowAgreementV1.getNetFlow implementation
    function getNetFlow(
        ISuperfluidToken token,
        address account
    )
        external view override
        returns (int96 flowRate)
    {
        (, FlowData memory state) = _getAccountFlowState(token, account);
        return state.flowRate;
    }

    /**************************************************************************
     * Internal State Functions
     *************************************************************************/

    enum FlowChangeType {
        CREATE_FLOW,
        UPDATE_FLOW,
        DELETE_FLOW
    }

    function _getAccountFlowState
    (
        ISuperfluidToken token,
        address account
    )
        private view
        returns(bool exist, FlowData memory)
    {
        bytes32[] memory data = token.getAgreementStateSlot(address(this), account, 0 /* slotId */, 1 /* length */);
        return _decodeFlowData(uint256(data[0]));
    }

    function _getAgreementData
    (
        ISuperfluidToken token,
        bytes32 dId
    )
        private view
        returns (bool exist, FlowData memory)
    {
        bytes32[] memory data = token.getAgreementData(address(this), dId, 1);
        return _decodeFlowData(uint256(data[0]));
    }

    function _updateAccountFlowState(
        ISuperfluidToken token,
        address account,
        int96 flowRateDelta,
        int256 depositDelta,
        int256 owedDepositDelta,
        uint256 currentTimestamp
    )
        private
        returns (int96 newNetFlowRate)
    {
        (, FlowData memory state) = _getAccountFlowState(token, account);
        int256 dynamicBalance = (currentTimestamp - state.timestamp).toInt256()
            * int256(state.flowRate);
        if (dynamicBalance != 0) {
            token.settleBalance(account, dynamicBalance);
        }
        state.flowRate = state.flowRate.add(flowRateDelta, "CFA: flowrate overflow");
        state.timestamp = currentTimestamp;
        state.deposit = (state.deposit.toInt256() + depositDelta).toUint256();
        state.owedDeposit = (state.owedDeposit.toInt256() + owedDepositDelta).toUint256();

        token.updateAgreementStateSlot(account, 0 /* slot id */, _encodeFlowData(state));

        return state.flowRate;
    }

    /**
     * @dev update a flow to a non-app receiver
     */
    function _changeFlowToNonApp(
        ISuperfluidToken token,
        FlowParams memory flowParams,
        FlowData memory oldFlowData,
        bytes memory ctx,
        ISuperfluid.Context memory currentContext
    )
        private
        returns (bytes memory newCtx)
    {
        // owed deposit should have been always zero, since an app should never become a non app
        assert(oldFlowData.owedDeposit == 0);

        // STEP 1: update the flow
        int256 depositDelta;
        FlowData memory newFlowData;
        (depositDelta,,newFlowData) = _changeFlow(
            currentContext.timestamp,
            currentContext.appAllowanceToken,
            token, flowParams, oldFlowData);

        // STEP 2: update app allowance used
        if (currentContext.appAllowanceToken == token) {
            newCtx = ISuperfluid(msg.sender).ctxUseAllowance(
                ctx,
                newFlowData.deposit, // allowanceWantedMore
                depositDelta // allowanceUsedDelta
            );
        } else {
            newCtx = ctx;
        }
    }

    /**
     * @dev change a flow to a app receiver
     */

    // Stack variables for updateFlowApp function, to avoid stack too deep issue
    // solhint-disable-next-line contract-name-camelcase
    struct _StackVars_changeFlowToApp {
        bytes cbdata;
        FlowData newFlowData;
        ISuperfluid.Context appContext;
    }
    function _changeFlowToApp(
        address appToCallback,
        ISuperfluidToken token,
        FlowParams memory flowParams,
        FlowData memory oldFlowData,
        bytes memory ctx,
        ISuperfluid.Context memory currentContext,
        FlowChangeType optype
    )
        private
        returns (bytes memory newCtx)
    {
        newCtx = ctx;
        // apply callbacks
        _StackVars_changeFlowToApp memory vars;

        // call callback
        if (appToCallback != address(0)) {
            AgreementLibrary.CallbackInputs memory cbStates = AgreementLibrary.createCallbackInputs(
                token,
                appToCallback,
                flowParams.flowId,
                abi.encode(flowParams.sender, flowParams.receiver)
            );

            // call the before callback
            if (optype == FlowChangeType.CREATE_FLOW) {
                cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP;
            } else if (optype == FlowChangeType.UPDATE_FLOW) {
                cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP;
            } else /* if (optype == FlowChangeType.DELETE_FLOW) */ {
                cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;
            }
            vars.cbdata = AgreementLibrary.callAppBeforeCallback(cbStates, ctx);

            (,cbStates.appAllowanceGranted,) = _changeFlow(
                    currentContext.timestamp,
                    currentContext.appAllowanceToken,
                    token, flowParams, oldFlowData);
            cbStates.appAllowanceGranted = cbStates.appAllowanceGranted * uint256(currentContext.appLevel + 1);
            cbStates.appAllowanceUsed = oldFlowData.owedDeposit.toInt256();
            // - each app level can at least "relay" the same amount of input flow rate to others
            // - each app level get a same amount of allowance

            if (optype == FlowChangeType.CREATE_FLOW) {
                cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP;
            } else if (optype == FlowChangeType.UPDATE_FLOW) {
                cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP;
            } else /* if (optype == FlowChangeType.DELETE_FLOW) */ {
                cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;
            }
            (vars.appContext,) = AgreementLibrary.callAppAfterCallback(cbStates, vars.cbdata, newCtx);

            // NB: the callback might update the same flow!!
            // reload the flow data
            (, vars.newFlowData) = _getAgreementData(token, flowParams.flowId);
        } else {
            (,,vars.newFlowData) = _changeFlow(
                    currentContext.timestamp,
                    currentContext.appAllowanceToken,
                    token, flowParams, oldFlowData);
        }

        // REVIEW the re-entrace assumptions from this point on

        // NOTE: vars.appContext.appAllowanceUsed will be adjusted by callAppAfterCallback
        // and its range will be [0, currentContext.appAllowance]
        {
            // clipping the allowance used amount before storing
            if (vars.appContext.appAllowanceUsed > 0) {
                // give more to the app
                vars.appContext.appAllowanceUsed =
                    _clipDepositNumber(vars.appContext.appAllowanceUsed.toUint256()).toInt256();
            }

            int256 appAllowanceDelta = vars.appContext.appAllowanceUsed
                - oldFlowData.owedDeposit.toInt256();

            // update flow data and account state with the allowance delta
            {
                vars.newFlowData.deposit = (vars.newFlowData.deposit.toInt256()
                    + appAllowanceDelta).toUint256();
                vars.newFlowData.owedDeposit = (vars.newFlowData.owedDeposit.toInt256()
                    + appAllowanceDelta).toUint256();
                token.updateAgreementData(flowParams.flowId, _encodeFlowData(vars.newFlowData));
                // update sender and receiver deposit (for sender) and owed deposit (for receiver)
                _updateAccountFlowState(
                    token,
                    flowParams.sender,
                    0, // flow rate delta
                    appAllowanceDelta, // deposit delta
                    0, // owed deposit delta
                    currentContext.timestamp
                );
                _updateAccountFlowState(
                    token,
                    flowParams.receiver,
                    0, // flow rate delta
                    0, // deposit delta
                    appAllowanceDelta, // owed deposit delta
                    currentContext.timestamp
                );
            }

            if (address(currentContext.appAllowanceToken) == address(0) ||
                currentContext.appAllowanceToken == token)
            {
                newCtx = ISuperfluid(msg.sender).ctxUseAllowance(
                    ctx,
                    vars.newFlowData.deposit, // allowanceWantedMore
                    appAllowanceDelta // allowanceUsedDelta
                );
            }

            // if receiver super app doesn't have enough available balance to give back app allowance
            // revert (non termination callbacks),
            // or take it from the sender and jail the app
            if (ISuperfluid(msg.sender).isApp(ISuperApp(flowParams.receiver))) {
                int256 availableBalance;
                (availableBalance,,) = token.realtimeBalanceOf(flowParams.receiver, currentContext.timestamp);
                if (availableBalance < 0) {
                    // app goes broke, send the app to jail
                    if (optype == FlowChangeType.DELETE_FLOW) {
                        newCtx = ISuperfluid(msg.sender).jailApp(
                            newCtx,
                            ISuperApp(flowParams.receiver),
                            SuperAppDefinitions.APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT);
                        // calculate user's damage
                        int256 userDamageAmount = AgreementLibrary.min(
                            // user will take the damage if the app is broke,
                            -availableBalance,
                            // but user's damage is limited to the amount of app allowance it gives to the app
                            AgreementLibrary.max(0, -appAllowanceDelta));
                        token.settleBalance(
                            flowParams.sender,
                            -userDamageAmount
                        );
                        token.settleBalance(
                            flowParams.receiver,
                            userDamageAmount
                        );
                    } else {
                        revert("CFA: APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT");
                    }
                }
            }
        }
    }

    /**
     * @dev change flow between sender and receiver with new flow rate
     *
     * NOTE:
     * - leaving owed deposit unchanged for later adjustment
     * - depositDelta output is always clipped (see _clipDepositNumber)
     */
    function _changeFlow(
        uint256 currentTimestamp,
        ISuperfluidToken appAllowanceToken,
        ISuperfluidToken token,
        FlowParams memory flowParams,
        FlowData memory oldFlowData
    )
        private
        returns (
            int256 depositDelta,
            uint256 appAllowanceBase,
            FlowData memory newFlowData
        )
    {
        { // enclosed block to avoid stack too deep error
            uint256 minimumDeposit;
            uint256 newDeposit;
            // STEP 1: calculate deposit required for the flow
            {
                (uint256 liquidationPeriod, ) = _decode3PsData(token);
                ISuperfluidGovernance gov = ISuperfluidGovernance(ISuperfluid(msg.sender).getGovernance());
                minimumDeposit = gov.getConfigAsUint256(
                    ISuperfluid(msg.sender), token, SUPERTOKEN_MINIMUM_DEPOSIT_KEY);
                // rounding up the number for app allowance too
                // CAVEAT:
                // - Now app could create a flow rate that is slightly higher than the incoming flow rate.
                // - The app may be jailed due to negative balance if it does this without its own balance.
                // Rule of thumbs:
                // - App can use app allowance to create a flow that has the same incoming flow rate
                // - But due to deposit clipping, there is no guarantee that the sum of the out going flow
                //   deposit can be covered by the allowance always.
                // - It is advisable for the app to check the allowance usages carefully, and if possible
                //   Always have some its own balances to cover the deposits.

                // preliminary calc of new deposit required, may be changed later in step 2.
                // used as a variable holding the new deposit amount in the meantime
                appAllowanceBase = _calculateDeposit(flowParams.flowRate, liquidationPeriod);
            }

            // STEP 2: apply minimum deposit rule and calculate deposit delta
            // preliminary calc depositDelta (minimum deposit rule not yet applied)
            depositDelta = appAllowanceBase.toInt256()
                - oldFlowData.deposit.toInt256()
                + oldFlowData.owedDeposit.toInt256();

            // preliminary calc newDeposit (minimum deposit rule not yet applied)
            newDeposit = (oldFlowData.deposit.toInt256() + depositDelta).toUint256();

            // calc depositDelta and newDeposit with minimum deposit rule applied
            if (newDeposit < minimumDeposit && flowParams.flowRate > 0) {
                depositDelta = minimumDeposit.toInt256()
                    - oldFlowData.deposit.toInt256()
                    + oldFlowData.owedDeposit.toInt256();
                newDeposit = minimumDeposit;
            }

            // allowance should be of the same token
            if (address(appAllowanceToken) != address(0) &&
                appAllowanceToken != token)
            {
                appAllowanceBase = 0;
            }

            // STEP 3: update current flow info
            newFlowData = FlowData(
                flowParams.flowRate > 0 ? currentTimestamp : 0,
                flowParams.flowRate,
                newDeposit,
                oldFlowData.owedDeposit // leaving it unchanged for later adjustment
            );
            token.updateAgreementData(flowParams.flowId, _encodeFlowData(newFlowData));
        }

        // STEP 4: update sender and receiver account flow state with the deltas
        int96 totalSenderFlowRate = _updateAccountFlowState(
            token,
            flowParams.sender,
            oldFlowData.flowRate.sub(flowParams.flowRate, "CFA: flowrate overflow"),
            depositDelta,
            0,
            currentTimestamp
        );
        int96 totalReceiverFlowRate = _updateAccountFlowState(
            token,
            flowParams.receiver,
            flowParams.flowRate.sub(oldFlowData.flowRate, "CFA: flowrate overflow"),
            0,
            0, // leaving owed deposit unchanged for later adjustment
            currentTimestamp
        );

        // STEP 5: emit the FlowUpdated Event
        emit FlowUpdated(
            token,
            flowParams.sender,
            flowParams.receiver,
            flowParams.flowRate,
            totalSenderFlowRate,
            totalReceiverFlowRate,
            flowParams.userData);
    }
    function _requireAvailableBalance(
        ISuperfluidToken token,
        ISuperfluid.Context memory currentContext
    )
        private view
    {
        // do not enforce balance checks during callbacks for the appAllowanceToken
        if (currentContext.callType != ContextDefinitions.CALL_INFO_CALL_TYPE_APP_CALLBACK ||
            currentContext.appAllowanceToken != token) {
            (int256 availableBalance,,) = token.realtimeBalanceOf(currentContext.msgSender, currentContext.timestamp);
            require(availableBalance >= 0, "CFA: not enough available balance");
        }
    }

    function _makeLiquidationPayouts(
        ISuperfluidToken token,
        int256 availableBalance,
        FlowParams memory flowParams,
        FlowData memory flowData,
        address liquidator
    )
        private
    {
        (,FlowData memory senderAccountState) = _getAccountFlowState(token, flowParams.sender);

        int256 signedSingleDeposit = flowData.deposit.toInt256();
        // TODO: GDA deposit should be considered here too
        int256 signedTotalCFADeposit = senderAccountState.deposit.toInt256();
        bytes memory liquidationTypeData;
        bool isCurrentlyPatricianPeriod;

        // Liquidation rules:
        //    - let Available Balance = AB (is negative)
        //    -     Agreement Single Deposit = SD
        //    -     Agreement Total Deposit = TD
        //    -     Total Reward Left = RL = AB + TD
        // #1 Can the total account deposit still cover the available balance deficit?
        int256 totalRewardLeft = availableBalance + signedTotalCFADeposit;
        
        // To retrieve patrician period
        // Note: curly brackets are to handle stack too deep overflow issue
        {
            (uint256 liquidationPeriod, uint256 patricianPeriod) = _decode3PsData(token);
            isCurrentlyPatricianPeriod = _isPatricianPeriod(
                availableBalance,
                signedTotalCFADeposit,
                liquidationPeriod,
                patricianPeriod
            );
        }

        // user is in a critical state
        if (totalRewardLeft >= 0) {
            // the liquidator is always whoever triggers the liquidation, but the
            // account which receives the reward will depend on the period (Patrician or Pleb)
            // #1.a.1 yes: then reward = (SD / TD) * RL
            int256 rewardAmount = signedSingleDeposit * totalRewardLeft / signedTotalCFADeposit;
            liquidationTypeData = abi.encode(1, isCurrentlyPatricianPeriod ? 0 : 1);
            token.makeLiquidationPayoutsV2(
                flowParams.flowId, // id
                liquidationTypeData, // (1 means "v1" of this encoding schema) - 0 or 1 for patrician or pleb
                liquidator, // liquidatorAddress
                
                // useDefaultRewardAccount: true in patrician period, else liquidator gets reward
                isCurrentlyPatricianPeriod,

                flowParams.sender, // targetAccount
                rewardAmount.toUint256(), // rewardAmount: remaining deposit of the flow to be liquidated
                rewardAmount * -1 // targetAccountBalanceDelta: amount deducted from the flow sender
            );
        } else {
            // #1.b.1 no: then the liquidator takes full amount of the single deposit
            int256 rewardAmount = signedSingleDeposit;
            liquidationTypeData = abi.encode(1, 2);
            token.makeLiquidationPayoutsV2(
                flowParams.flowId, // id
                liquidationTypeData, // (1 means "v1" of this encoding schema) - 2 for pirate/bailout period
                liquidator, // liquidatorAddress
                false, // useDefaultRewardAccount: out of patrician period, in pirate period, so always false
                flowParams.sender, // targetAccount
                rewardAmount.toUint256(), // rewardAmount: single deposit of flow
                totalRewardLeft * -1 // targetAccountBalanceDelta: amount to bring sender AB to 0
                // NOTE: bailoutAmount = rewardAmount + targetAccountBalanceDelta + paid by rewardAccount
            );
        }
    }

    /**************************************************************************
     * Deposit Calculation Pure Functions
     *************************************************************************/

    function _clipDepositNumberRoundingDown(uint256 deposit)
        internal pure
        returns(uint256)
    {
        return ((deposit >> 32)) << 32;
    }

    function _clipDepositNumber(uint256 deposit)
        internal pure
        returns(uint256)
    {
        // clipping the value, rounding up
        uint256 rounding = (deposit & type(uint32).max) > 0 ? 1 : 0;
        return ((deposit >> 32) + rounding) << 32;
    }

    function _calculateDeposit(
        int96 flowRate,
        uint256 liquidationPeriod
    )
        internal pure
        returns(uint256 deposit)
    {
        if (flowRate == 0) return 0;
        
        assert(liquidationPeriod <= uint256(int256(type(int96).max)));
        deposit = uint256(int256(flowRate.mul(int96(uint96(liquidationPeriod)), "CFA: deposit overflow")));
        return _clipDepositNumber(deposit);
    }

    /**************************************************************************
     * Flow Data Pure Functions
     *************************************************************************/

    function _generateFlowId(address sender, address receiver) private pure returns(bytes32 id) {
        return keccak256(abi.encode(sender, receiver));
    }

    //
    // Data packing:
    //
    // WORD A: | timestamp  | flowRate | deposit | owedDeposit |
    //         | 32b        | 96b      | 64      | 64          |
    //
    // NOTE:
    // - flowRate has 96 bits length
    // - deposit has 96 bits length too, but 32 bits are clipped-off when storing

    function _encodeFlowData
    (
        FlowData memory flowData
    )
        internal pure
        returns(bytes32[] memory data)
    {
        // enable these for debugging
        // assert(flowData.deposit & type(uint32).max == 0);
        // assert(flowData.owedDeposit & type(uint32).max == 0);
        data = new bytes32[](1);
        data[0] = bytes32(
            ((uint256(flowData.timestamp)) << 224) |
            ((uint256(uint96(flowData.flowRate)) << 128)) |
            (uint256(flowData.deposit) >> 32 << 64) |
            (uint256(flowData.owedDeposit) >> 32)
        );
    }

    function _decodeFlowData
    (
        uint256 wordA
    )
        internal pure
        returns(bool exist, FlowData memory flowData)
    {
        exist = wordA > 0;
        if (exist) {
            flowData.timestamp = uint32(wordA >> 224);
            // NOTE because we are upcasting from type(uint96).max to uint256 to int256, we do not need to use safecast
            flowData.flowRate = int96(int256(wordA >> 128) & int256(uint256(type(uint96).max)));
            flowData.deposit = ((wordA >> 64) & uint256(type(uint64).max)) << 32 /* recover clipped bits*/;
            flowData.owedDeposit = (wordA & uint256(type(uint64).max)) << 32 /* recover clipped bits*/;
        }
    }

    /**************************************************************************
     * 3P's Pure Functions
     *************************************************************************/

    //
    // Data packing:
    //
    // WORD A: |    reserved    | patricianPeriod | liquidationPeriod |
    //         |      192       |        32       |         32        |
    //
    // NOTE:
    // - liquidation period has 32 bits length
    // - patrician period also has 32 bits length

    function _decode3PsData(
        ISuperfluidToken token
    )
        internal view
        returns(uint256 liquidationPeriod, uint256 patricianPeriod)
    {
        ISuperfluid host = ISuperfluid(token.getHost());
        ISuperfluidGovernance gov = ISuperfluidGovernance(host.getGovernance());
        uint256 pppConfig = gov.getConfigAsUint256(host, token, CFAV1_PPP_CONFIG_KEY);
        (liquidationPeriod, patricianPeriod) = SuperfluidGovernanceConfigs.decodePPPConfig(pppConfig);
    }

    function _isPatricianPeriod(
        int256 availableBalance,
        int256 signedTotalCFADeposit,
        uint256 liquidationPeriod,
        uint256 patricianPeriod
    ) 
        internal pure 
        returns (bool) 
    {
        int256 totalRewardLeft = availableBalance + signedTotalCFADeposit;
        int256 totalCFAOutFlowrate = signedTotalCFADeposit / int256(liquidationPeriod);
        // divisor cannot be zero with existing outflow
        return totalRewardLeft / totalCFAOutFlowrate > int256(liquidationPeriod - patricianPeriod);
    }
}
