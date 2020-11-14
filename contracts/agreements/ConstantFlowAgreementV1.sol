// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.4;

import {
    IConstantFlowAgreementV1,
    ISuperfluidToken
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperApp
} from "../interfaces/superfluid/ISuperfluid.sol";
import { AgreementBase } from "./AgreementBase.sol";

import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/SafeCast.sol";
import { Int96SafeMath } from "../utils/Int96SafeMath.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";


contract ConstantFlowAgreementV1 is
    AgreementBase,
    IConstantFlowAgreementV1
{

    using SafeMath for uint256;
    using SafeCast for uint256;
    using SignedSafeMath for int256;
    using SafeCast for int256;
    using Int96SafeMath for int96;

    struct FlowData {
        uint256 timestamp;
        int96 flowRate;
        uint256 deposit;
        uint256 owedDeposit;
    }

    struct FlowParams {
        bytes32 flowId;
        address sender;
        address receiver;
        int96 flowRate;
    }

    /*
     * ISuperAgreement interface
     */

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
            dynamicBalance = ((int256(time).sub(int256(state.timestamp))).mul(state.flowRate));
            deposit = state.deposit;
            owedDeposit = state.owedDeposit;
        }
    }

    /// @dev IFlowAgreement.createFlow implementation
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
        uint256 currentTimestamp = block.timestamp;
        FlowParams memory flowParams;
        require(receiver != address(0), "CFA: receiver is zero");
        AgreementLibrary.Context memory currentContext = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx);
        flowParams.flowId = _generateFlowId(currentContext.msgSender, receiver);
        flowParams.sender = currentContext.msgSender;
        flowParams.receiver = receiver;
        flowParams.flowRate = flowRate;
        require(flowParams.sender != flowParams.receiver, "CFA: no self flow");
        require(flowParams.flowRate > 0, "CFA: invalid flow rate");
        (bool exist, FlowData memory oldFlowData) = _getAgreementData(token, flowParams.flowId);
        require(!exist, "CFA: flow already exist");

        if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver)))
        {
            newCtx = _changeFlowToApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext, FlowChangeType.CREATE_FLOW);
        } else {
            newCtx = _changeFlowToNonApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext);
        }

        _requireAvailableBalance(
            token,
            currentTimestamp,
            currentContext);
    }

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
        uint256 currentTimestamp = block.timestamp;
        FlowParams memory flowParams;
        require(receiver != address(0), "CFA: receiver is zero");
        AgreementLibrary.Context memory currentContext = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx);
        flowParams.flowId = _generateFlowId(currentContext.msgSender, receiver);
        flowParams.sender = currentContext.msgSender;
        flowParams.receiver = receiver;
        flowParams.flowRate = flowRate;
        require(flowParams.sender != flowParams.receiver, "CFA: no self flow");
        require(flowParams.flowRate > 0, "CFA: invalid flow rate");
        (bool exist, FlowData memory oldFlowData) = _getAgreementData(token, flowParams.flowId);
        require(exist, "CFA: flow does not exist");

        if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver))) {
            newCtx = _changeFlowToApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext, FlowChangeType.UPDATE_FLOW);
        } else {
            newCtx = _changeFlowToNonApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext);
        }

        _requireAvailableBalance(
            token,
            currentTimestamp,
            currentContext);
    }

    /// @dev IFlowAgreement.deleteFlow implementation
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
        uint256 currentTimestamp = block.timestamp;
        FlowParams memory flowParams;
        require(sender != address(0), "CFA: sender is zero");
        require(receiver != address(0), "CFA: receiver is zero");
        AgreementLibrary.Context memory currentContext = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx);
        flowParams.flowId = _generateFlowId(sender, receiver);
        flowParams.sender = sender;
        flowParams.receiver = receiver;
        flowParams.flowRate = 0;
        (bool exist, FlowData memory oldFlowData) = _getAgreementData(token, flowParams.flowId);
        require(exist, "CFA: flow does not exist");

        (int256 availableBalance,,) = token.realtimeBalanceOf(sender, currentTimestamp);

        // delete should only be called by sender or receiver
        // unless it is a liquidation (availale balance < 0)
        if (currentContext.msgSender != sender && currentContext.msgSender != receiver) {
            require(availableBalance < 0, "CFA: account is not critical");
        }

        if (availableBalance < 0) {
            _makeLiquidationPayouts(
                token,
                availableBalance,
                flowParams,
                oldFlowData,
                currentContext.msgSender);
        }

        if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver))) {
            newCtx = _changeFlowToApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext, FlowChangeType.DELETE_FLOW);
        } else {
            newCtx = _changeFlowToNonApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext);
        }

        // FIXME should only revert for an app
        /* _requireAvailableBalance(
            token,
            currentTimestamp,
            currentContext.msgSender,
            currentContext.allowance); */
    }

    /// @dev IFlowAgreement.getFlow implementation
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
            keccak256(abi.encodePacked(sender, receiver))
        );

        return(
            data.timestamp,
            data.flowRate,
            data.deposit,
            data.owedDeposit
        );
    }

    /// @dev IFlowAgreement.getFlow implementation
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

    /// @dev IFlowAgreement.getAccountFlowInfo implementation
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

    /// @dev IFlowAgreement.getNetFlow implementation
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

    /*
     * Internal State Functions
     */

    // Stack variables for updateFlowApp function, to avoid stack too deep issue
    // solhint-disable-next-line contract-name-camelcase
    struct _StackVars_updateFlowToApp {
        bytes cbdata;
        int256 depositDelta;
        FlowData newFlowData;
        int256 appAllowance;
        AgreementLibrary.Context appContext;
    }

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

    function _requireAvailableBalance(
        ISuperfluidToken token,
        uint256 currentTimestamp,
        AgreementLibrary.Context memory currentContext
    )
        private view
    {
        (int256 availableBalance,,) = token.realtimeBalanceOf(currentContext.msgSender, currentTimestamp);
        require(
            availableBalance
                .add(currentContext.allowance > 0 ?
                    // allowance (positive value) is given to the sender only after callback finishes
                    currentContext.allowance
                    // but allowance refund (negative value) is taken away from the sender immediately
                    : 0)
            >= 0,
            "CFA: not enough available balance");
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
        int256 dynamicBalance = currentTimestamp.sub(state.timestamp).toInt256()
            .mul(int256(state.flowRate));
        if (dynamicBalance != 0) {
            token.settleBalance(account, dynamicBalance);
        }
        state.flowRate = state.flowRate.add(flowRateDelta);
        state.timestamp = currentTimestamp;
        state.deposit = state.deposit.toInt256().add(depositDelta).toUint256();
        state.owedDeposit = state.owedDeposit.toInt256().add(owedDepositDelta).toUint256();

        token.updateAgreementStateSlot(account, 0 /* slot id */, _encodeFlowData(state));

        return state.flowRate;
    }

    /**
     * @dev update a flow to a non-app receiver
     */
    function _changeFlowToNonApp(
        uint256 currentTimestamp,
        ISuperfluidToken token,
        FlowParams memory flowParams,
        FlowData memory oldFlowData,
        bytes memory ctx,
        AgreementLibrary.Context memory currentContext
    )
        private
        returns (bytes memory newCtx)
    {
        // owed deposit should have been always zero, since an app should never become a non app
        assert(oldFlowData.owedDeposit == 0);

        // STEP 1: update the flow
        (int256 depositDelta,) = _changeFlow(
            currentTimestamp,
            token, flowParams, oldFlowData);

        // STEP 2: calculate allowance used delta, and account balance delta
        newCtx = AgreementLibrary.applyAllowanceUsedAndUpdate(
            token,
            flowParams.sender,
            currentContext.allowance,
            currentContext.allowanceUsed,
            depositDelta,
            ctx
        );
    }

    /**
     * @dev change a flow to a app receiver
     */
    function _changeFlowToApp(
        uint256 currentTimestamp,
        ISuperfluidToken token,
        FlowParams memory flowParams,
        FlowData memory oldFlowData,
        bytes memory ctx,
        AgreementLibrary.Context memory currentContext,
        FlowChangeType optype
    )
        private
        returns (bytes memory newCtx)
    {
        // apply callbacks
        _StackVars_updateFlowToApp memory vars;

        {
            if (optype == FlowChangeType.CREATE_FLOW) {
                (vars.cbdata, newCtx) = AgreementLibrary.beforeAgreementCreated(
                    ISuperfluid(msg.sender), token, ctx, address(this), flowParams.receiver, flowParams.flowId
                );
            } else if (optype == FlowChangeType.UPDATE_FLOW) {
                (vars.cbdata, newCtx) = AgreementLibrary.beforeAgreementUpdated(
                    ISuperfluid(msg.sender), token, ctx, address(this), flowParams.receiver, flowParams.flowId
                );
            } else /* if (optype == FlowChangeType.DELETE_FLOW) */ {
                (vars.cbdata, newCtx) = AgreementLibrary.beforeAgreementTerminated(
                    ISuperfluid(msg.sender), token, ctx, address(this), flowParams.receiver, flowParams.flowId
                );
            }

            (vars.depositDelta, vars.newFlowData) = _changeFlow(
                    currentTimestamp,
                    token, flowParams, oldFlowData);

            // set allowance for the next callback
            // multiplied the allowance with app level
            if (vars.depositDelta > 0) {
                // give app full allowance
                vars.appAllowance = vars.depositDelta
                    .mul(int256(currentContext.appLevel));
            } else if (vars.depositDelta < 0 && oldFlowData.owedDeposit > 0) {
                // ask for refund up to amount of owed deposit given prio
                vars.appAllowance = AgreementLibrary.max(
                        vars.depositDelta,
                        oldFlowData.owedDeposit.toInt256().mul(-1)
                    ).mul(int256(currentContext.appLevel));
            }

            vars.appAllowance = vars.appAllowance;

            if (optype == FlowChangeType.CREATE_FLOW) {
                (vars.appContext, newCtx) = AgreementLibrary.afterAgreementCreated(
                    ISuperfluid(msg.sender),
                    token,
                    newCtx,
                    address(this),
                    flowParams.receiver,
                    flowParams.flowId,
                    vars.cbdata,
                    vars.appAllowance
                );
            } else if (optype == FlowChangeType.UPDATE_FLOW) {
                (vars.appContext, newCtx) = AgreementLibrary.afterAgreementUpdated(
                    ISuperfluid(msg.sender),
                    token,
                    newCtx,
                    address(this),
                    flowParams.receiver,
                    flowParams.flowId,
                    vars.cbdata,
                    vars.appAllowance
                );
            } else /* if (optype == FlowChangeType.DELETE_FLOW) */ {
                (vars.appContext, newCtx) = AgreementLibrary.afterAgreementTerminated(
                    ISuperfluid(msg.sender),
                    token,
                    newCtx,
                    address(this),
                    flowParams.receiver,
                    flowParams.flowId,
                    vars.cbdata,
                    vars.appAllowance
                );
            }
        }

        if (vars.appContext.allowanceUsed != 0) {
            // update owed deposit of the flow
            vars.newFlowData.deposit = vars.newFlowData.deposit.toInt256()
                    .add(vars.appContext.allowanceUsed)
                    .toUint256();
            vars.newFlowData.owedDeposit = vars.newFlowData.owedDeposit.toInt256()
                    .add(vars.appContext.allowanceUsed)
                    .toUint256();
            token.updateAgreementData(flowParams.flowId, _encodeFlowData(vars.newFlowData));

            // update sender and receiver deposit (for sender) and owed deposit (for receiver)
            _updateAccountFlowState(
                token,
                flowParams.sender,
                0, // flow rate delta
                vars.appContext.allowanceUsed, // deposit delta
                0, // owed deposit delta
                currentTimestamp
            );
            _updateAccountFlowState(
                token,
                flowParams.receiver,
                0, // flow rate delta
                0, // deposit delta
                vars.appContext.allowanceUsed, // owed deposit delta
                currentTimestamp
            );
        }

        newCtx = AgreementLibrary.applyAllowanceUsedAndUpdate(
            token,
            flowParams.sender,
            currentContext.allowance,
            currentContext.allowanceUsed,
            vars.appContext.allowanceUsed,
            newCtx
        );
    }

    /**
     * @dev change flow between sender and receiver with new flow rate
     *
     * NOTE:
     * - leaving owed deposit unchanged for later adjustment
     */
    function _changeFlow(
        uint256 currentTimestamp,
        ISuperfluidToken token,
        FlowParams memory flowParams,
        FlowData memory oldFlowData
    )
        private
        returns (
            int256 depositDelta,
            FlowData memory newFlowData
        )
    {
        { // ecnlosed block to avoid stack too deep error
            uint256 oldDeposit;
            uint256 newDeposit;

            // STEP 1: calculate old and new deposit required for the flow
            ISuperfluidGovernance gov = AgreementLibrary.getGovernance();
            uint256 liquidationPeriod = gov.getLiquidationPeriod(token);
            oldDeposit = _calculateDeposit(oldFlowData.flowRate, liquidationPeriod);
            newDeposit = _calculateDeposit(flowParams.flowRate, liquidationPeriod);

            // STEP 2: calculate deposit delta
            depositDelta = newDeposit.toInt256().sub(oldDeposit.toInt256());

            // STEP 3: update current flow info
            newFlowData = FlowData(
                flowParams.flowRate > 0 ? currentTimestamp : 0,
                flowParams.flowRate,
                oldFlowData.deposit.toInt256().add(depositDelta).toUint256(),
                oldFlowData.owedDeposit // leaving it unchanged for later adjustment
            );
            token.updateAgreementData(flowParams.flowId, _encodeFlowData(newFlowData));
        }

        // STEP 4: update sender and receiver account flow state with the deltas
        int96 totalSenderFlowRate = _updateAccountFlowState(
            token,
            flowParams.sender,
            oldFlowData.flowRate.sub(flowParams.flowRate),
            depositDelta,
            0,
            currentTimestamp
        );
        int96 totalReceiverFlowRate = _updateAccountFlowState(
            token,
            flowParams.receiver,
            flowParams.flowRate.sub(oldFlowData.flowRate),
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
            totalReceiverFlowRate);
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
        int256 signedTotalDeposit = senderAccountState.deposit.toInt256();

        // Liquidation rules:
        //    - let Available Balance = AB (is negative)
        //    -     Agreement Single Deposit = SD
        //    -     Agreement Total Deposit = TD
        //    -     Total Reward Left = RL = AB + TD
        // #1 Can the total account deposit can still cover the available balance deficit?
        int256 totalRewardLeft = availableBalance.add(signedTotalDeposit);
        if (totalRewardLeft >= 0) {
            // #1.a.1 yes: then reward = (SD / TD) * RL
            int256 rewardAmount = signedSingleDeposit.mul(totalRewardLeft).div(signedTotalDeposit);
            token.makeLiquidationPayouts(
                flowParams.flowId,
                liquidator,
                flowParams.sender,
                rewardAmount.toUint256(),
                0
            );
        } else {
            // #1.b.1 no: then the liquidator takes full amount of the single deposit
            int256 rewardAmount = signedSingleDeposit;
            token.makeLiquidationPayouts(
                flowParams.flowId,
                liquidator,
                flowParams.sender,
                rewardAmount.toUint256() /* rewardAmount */,
                totalRewardLeft.mul(-1).toUint256() /* bailoutAmount */
            );
        }
    }

    /**************************************************************************
     * Deposit Calculation Pure Functions
     *************************************************************************/

    function _calculateDeposit(
        int96 flowRate,
        uint256 liquidationPeriod
    )
        internal pure
        returns(uint256 deposit)
    {
        if (flowRate == 0) return 0;
        assert(liquidationPeriod <= uint256(type(int96).max));
        deposit = uint256(flowRate.mul(int96(uint96(liquidationPeriod))));
        // clipping the value, and make sure the minimal deposit is not ZERO after clipping
        deposit = deposit >> 32;
        if (deposit == 0) return 1 << 32;
        else return deposit << 32;
    }

    /**************************************************************************
     * Flow Data Pure Functions
     *************************************************************************/

    function _generateFlowId(address sender, address receiver) private pure returns(bytes32 id) {
        return keccak256(abi.encodePacked(sender, receiver));
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
            flowData.flowRate = int96((wordA >> 128) & uint256(type(uint96).max));
            flowData.deposit = ((wordA >> 64) & uint256(type(uint64).max)) << 32 /* recover clipped bits*/;
            flowData.owedDeposit = (wordA & uint256(type(uint64).max)) << 32 /* recover clipped bits*/;
        }
    }

}
