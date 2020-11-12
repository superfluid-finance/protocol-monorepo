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
    using AgreementLibrary for AgreementLibrary.Context;

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
            newCtx = _updateFlowToApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext, true /* doCreate */);
        } else {
            newCtx = _updateFlowToNonApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext);
        }

        _requireAvailableBalance(
            token,
            currentTimestamp,
            currentContext.msgSender,
            currentContext.allowance);
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
            newCtx = _updateFlowToApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext, false /* doCreate */);
        } else {
            newCtx = _updateFlowToNonApp(
                currentTimestamp,
                token, flowParams, oldFlowData,
                ctx, currentContext);
        }

        _requireAvailableBalance(
            token,
            currentTimestamp,
            currentContext.msgSender,
            currentContext.allowance);
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
        require(sender != address(0), "CFA: sender is zero");
        require(receiver != address(0), "CFA: receiver is zero");
        bytes32 flowId = _generateFlowId(sender, receiver);
        (bool exist, FlowData memory flowData) = _getAgreementData(token, flowId);
        require(exist, "CFA: flow does not exist");
        (int256 availableBalance,,) = token.realtimeBalanceOf(sender, block.timestamp);

        address msgSender = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx).msgSender;

        // delete should only be called by sender or receiver
        // unless it is a liquidation (availale balance < 0)
        if (msgSender != sender && msgSender != receiver) {
            require(availableBalance < 0, "CFA: account is not critical");
        }

        bytes memory cbdata;
        (cbdata, newCtx) = AgreementLibrary.beforeAgreementTerminated(
            ISuperfluid(msg.sender),
            token,
            ctx, // FIXME calculate allowance
            address(this),
            receiver,
            flowId
        );
        // TODO: Decode return cbdata before calling the next step

        if (availableBalance < 0) {
            _liquidateAgreement(token, availableBalance, flowId, flowData, sender, msgSender);
        }

        _terminateAgreement(token, flowId, flowData, sender, receiver);

        (, newCtx) = AgreementLibrary.afterAgreementTerminated(
            ISuperfluid(msg.sender),
            token,
            newCtx,
            address(this),
            receiver,
            flowId,
            cbdata,
            0
        );
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
        int256 owedDepositDelta;
        FlowData newFlowData;
        int256 appAllowance;
        AgreementLibrary.Context appContext;
        int accountAllowanceUsedDelta;
        int accountBalanceDelta;
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
        address account,
        int256 currentAllowance
    )
        private view
    {
        (int256 availableBalance,,) = token.realtimeBalanceOf(account, currentTimestamp);
        require(availableBalance.add(currentAllowance) >= 0, "CFA: not enough available balance");
    }

    function _applyAllowanceUsedAndUpdate(
        ISuperfluidToken token,
        address account,
        int256 currentAllowance,
        int256 currentAllowanceUsed,
        int256 allowanceUsed,
        bytes memory ctx
    )
        private
        returns (bytes memory newCtx)
    {
        (int accountAllowanceUsedDelta, int accountBalanceDelta) = _applyAllowanceUsed(
            currentAllowance,
            currentAllowanceUsed,
            allowanceUsed
        );

        if (accountAllowanceUsedDelta != 0) {
            newCtx = ISuperfluid(msg.sender).ctxUpdateAllowanceUsed(
                ctx,
                currentAllowanceUsed.add(accountAllowanceUsedDelta));
        } else {
            newCtx = ctx;
        }

        if (accountBalanceDelta != 0) {
            token.settleBalance(account, accountBalanceDelta);
        }
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

        newNetFlowRate = state.flowRate;

        token.updateAgreementStateSlot(account, 0 /* slot id */, _encodeFlowData(state));

        return state.flowRate;
    }

    /**
     * @dev update a flow to a non-app receiver
     */
    function _updateFlowToNonApp(
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
        // STEP 1: update the flow
        (int256 depositDelta, int256 owedDepositDelta,) = _updateFlow(
            currentTimestamp,
            token, flowParams, oldFlowData);
        // owed deposit should have been always zero, since an app should never become a non app
        assert(owedDepositDelta == 0);

        // STEP 2: calculate allowance used delta, and account balance delta
        newCtx = _applyAllowanceUsedAndUpdate(
            token,
            flowParams.sender,
            currentContext.allowance,
            currentContext.allowanceUsed,
            depositDelta,
            ctx
        );
    }

    /**
     * @dev update a flow to a app receiver
     */
    function _updateFlowToApp(
        uint256 currentTimestamp,
        ISuperfluidToken token,
        FlowParams memory flowParams,
        FlowData memory oldFlowData,
        bytes memory ctx,
        AgreementLibrary.Context memory currentContext,
        bool toCreate
    )
        private
        returns (bytes memory newCtx)
    {
        // apply callbacks
        _StackVars_updateFlowToApp memory vars;
        {
            if (toCreate) {
                (vars.cbdata, newCtx) = AgreementLibrary.beforeAgreementCreated(
                    ISuperfluid(msg.sender), token, ctx, address(this), flowParams.receiver, flowParams.flowId
                );
            } else {
                (vars.cbdata, newCtx) = AgreementLibrary.beforeAgreementUpdated(
                    ISuperfluid(msg.sender), token, ctx, address(this), flowParams.receiver, flowParams.flowId
                );
            }

            (vars.depositDelta, vars.owedDepositDelta, vars.newFlowData) = _updateFlow(
                    currentTimestamp,
                    token, flowParams, oldFlowData);

            // set allowance for the next callback
            vars.appAllowance = int256(currentContext.appLevel).mul(vars.depositDelta);
            if (toCreate) {
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
            } else {
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
            }
        }

        {
            // update owed deposit of the flow
            int owedDepositDelta = vars.newFlowData.owedDeposit.toInt256();
            vars.newFlowData.deposit = vars.newFlowData.deposit.toInt256()
                    .add(vars.appContext.allowanceUsed)
                    .toUint256();
            vars.newFlowData.owedDeposit = vars.appContext.allowanceUsed > 0 ?
                vars.appContext.allowanceUsed.toUint256() : 0;
            owedDepositDelta = vars.newFlowData.owedDeposit.toInt256().sub(owedDepositDelta);
            // update sender and receiver deposit (for sender) and owed deposit (for receiver)
            if (owedDepositDelta != 0) {
                _updateAccountFlowState(
                    token,
                    flowParams.sender,
                    0, // flow rate delta
                    owedDepositDelta, // deposit delta
                    0, // owed deposit delta
                    currentTimestamp
                );
                _updateAccountFlowState(
                    token,
                    flowParams.receiver,
                    0, // flow rate delta
                    0, // deposit delta
                    owedDepositDelta, // owed deposit delta
                    currentTimestamp
                );
            }
            token.updateAgreementData(flowParams.flowId, _encodeFlowData(vars.newFlowData));
        }

        newCtx = _applyAllowanceUsedAndUpdate(
            token,
            flowParams.sender,
            currentContext.allowance,
            currentContext.allowanceUsed,
            vars.appContext.allowanceUsed,
            newCtx
        );
    }

    /**
     * @dev update flow between sender and receiver with new flow rate
     *
     * NOTE:
     * - owed deposit of the new flow data is reset to 0
     */
    function _updateFlow(
        uint256 currentTimestamp,
        ISuperfluidToken token,
        FlowParams memory flowParams,
        FlowData memory oldFlowData
    )
        private
        returns (
            int256 depositDelta,
            int256 owedDepositDelta,
            FlowData memory newFlowData
        )
    {
        uint256 newDeposit;

        // STEP 1: calculate new deposit required for the flow
        {
            ISuperfluidGovernance gov = AgreementLibrary.getGovernance();
            uint256 liquidationPeriod = gov.getLiquidationPeriod(token);
            newDeposit = _calculateDeposit(flowParams.flowRate, liquidationPeriod);
        }

        // STEP 2: calculate deposit and owewd deposit deltas
        depositDelta = newDeposit.toInt256().sub(oldFlowData.deposit.toInt256());
        owedDepositDelta = oldFlowData.owedDeposit.toInt256().mul(-1);

        // STEP 3: update current flow info with owed deposit reset to 0
        newFlowData = FlowData(
            currentTimestamp,
            flowParams.flowRate,
            newDeposit,
            0
        );
        token.updateAgreementData(flowParams.flowId, _encodeFlowData(newFlowData));

        // STEP 4: update sender and receiver account flow state with the deltas
        int96 totalSenderFlowRate = _updateAccountFlowState(
            token,
            flowParams.sender,
            oldFlowData.flowRate.sub(flowParams.flowRate),
            depositDelta,
            owedDepositDelta,
            currentTimestamp
        );
        int96 totalReceiverFlowRate = _updateAccountFlowState(
            token,
            flowParams.receiver,
            flowParams.flowRate.sub(oldFlowData.flowRate),
            0,
            0,
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

    function _terminateAgreement(
        ISuperfluidToken token,
        bytes32 flowId,
        FlowData memory flowData,
        address sender,
        address receiver
    )
        private
    {
        int96 totalSenderFlowRate = _updateAccountFlowState(
            token,
            sender,
            flowData.flowRate,
            flowData.deposit.toInt256().mul(-1),
            flowData.owedDeposit.toInt256().mul(-1),
            block.timestamp
        );
        int96 totalReceiverFlowRate = _updateAccountFlowState(
            token,
            receiver,
            flowData.flowRate.mul(-1),
            0,
            0,
            block.timestamp
        );
        token.terminateAgreement(flowId, 1);

        emit FlowUpdated(
            token,
            sender,
            receiver,
            0,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _liquidateAgreement(
        ISuperfluidToken token,
        int256 availableBalance,
        bytes32 flowId,
        FlowData memory flowData,
        address sender,
        address liquidator
    )
        private
    {
        (,FlowData memory senderAccountState) = _getAccountFlowState(token, sender);

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
            token.liquidateAgreement(
                flowId,
                liquidator,
                sender,
                rewardAmount.toUint256(),
                0
            );
        } else {
            // #1.b.1 no: then the liquidator takes full amount of the single deposit
            int256 rewardAmount = signedSingleDeposit;
            token.liquidateAgreement(
                flowId,
                liquidator,
                sender,
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
        assert(liquidationPeriod <= uint256(type(int96).max));
        deposit = uint256(flowRate.mul(int96(uint96(liquidationPeriod))));
        // clipping the value, and make sure the minimal deposit is not ZERO after clipping
        deposit = deposit >> 32;
        if (deposit == 0) return 1 << 32;
        else return deposit << 32;
    }

    // Apppy "allowance used" to "current allowance left" value to get:
    // - account allowance used delta,
    // - and account balance delta
    //
    // NOTES:
    // - currentAllowanceLeft can be nagative - as allowance refunds requested
    // - allowanceUsed can be negative - as allowance refunds
    function _applyAllowanceUsed(
        int256 currentAllowance,
        int256 currentAllowanceUsed,
        int256 allowanceUsed
    )
        internal pure
        returns (
            int256 accountAllowanceUsedDelta,
            int256 accountBalanceDelta
        )
    {
        int256 currentAllowanceLeft = currentAllowance.sub(currentAllowanceUsed);
        if (currentAllowanceLeft > 0) {
            // use the allowance
            if (allowanceUsed > 0) {
                // use up to the current context allowance amount
                if (currentAllowanceLeft > allowanceUsed) {
                    // use up to what app used,
                    accountAllowanceUsedDelta = allowanceUsed;
                } else {
                    // free up to the current context allowance amount
                    accountAllowanceUsedDelta = currentAllowanceLeft;
                    // rest paid by sender
                    accountBalanceDelta = allowanceUsed.sub(currentAllowanceLeft);
                }
            }
        } else if (currentAllowanceLeft < 0) {
            // refund the allowance
            if (allowanceUsed < 0) {
                // always refund everything
                accountAllowanceUsedDelta = currentAllowanceLeft;
                if (currentAllowanceLeft < allowanceUsed) {
                    // take into account refund paid by the app
                    accountBalanceDelta = accountAllowanceUsedDelta.sub(allowanceUsed);
                } // else app refund more than needed
            }
        }
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
