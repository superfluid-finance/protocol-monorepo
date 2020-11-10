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
}
from "../interfaces/superfluid/ISuperfluid.sol";
import { AgreementBase } from "./AgreementBase.sol";

import { Math } from "@openzeppelin/contracts/math/Math.sol";
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
        (bool exist, FlowData memory state) = _getAccountState(token, account);
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
        require(receiver != address(0), "CFA: receiver is zero");
        AgreementLibrary.Context memory currentContext = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx);
        bytes32 flowId = _generateId(currentContext.msgSender, receiver);
        require(currentContext.msgSender != receiver, "CFA: no self flow");
        require(flowRate > 0, "CFA: invalid flow rate");
        require(!_flowExists(token, flowId), "CFA: flow already exist");

        if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver)))
        {
            newCtx = _updateFlowToApp(
                token, flowId, currentContext.msgSender, receiver, flowRate,
                ctx, currentContext, true);
        } else {
            _updateFlowToNonApp(token, flowId, currentContext.msgSender, receiver, flowRate,
                ctx, currentContext);
            newCtx = ctx;
        }

        require(!token.isAccountCriticalNow(currentContext.msgSender), "CFA: not enough available balance");
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
        require(receiver != address(0), "CFA: receiver is zero");
        AgreementLibrary.Context memory currentContext = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx);
        bytes32 flowId = _generateId(currentContext.msgSender, receiver);
        require(currentContext.msgSender != receiver, "CFA: no self flow");
        require(flowRate > 0, "CFA: invalid flow rate");
        require(_flowExists(token, flowId), "CFA: flow does not exist");

        if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver))) {
            newCtx = _updateFlowToApp(
                token, flowId, currentContext.msgSender, receiver, flowRate,
                ctx, currentContext, false);
        } else {
            _updateFlowToNonApp(token, flowId, currentContext.msgSender, receiver, flowRate,
                ctx, currentContext);
            newCtx = ctx;
        }

        require(!token.isAccountCriticalNow(currentContext.msgSender), "CFA: not enough available balance");
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
        bytes32 flowId = _generateId(sender, receiver);
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

        newCtx = AgreementLibrary.afterAgreementTerminated(
            ISuperfluid(msg.sender),
            token,
            newCtx,
            address(this),
            receiver,
            flowId,
            cbdata
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
        (, FlowData memory state) = _getAccountState(token, account);
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
        (, FlowData memory state) = _getAccountState(token, account);
        return state.flowRate;
    }

    /*
     * Internal Functions
     */
    // solhint-disable-next-line contract-name-camelcase
    struct _States_updateAccountState {
        bytes cbdata;
        int256 depositDelta;
        int256 owedDepositDelta;
        FlowData newFlowData;
        int256 appAllowance;
        AgreementLibrary.Context appContext;
    }

    // solhint-disable-next-line contract-name-camelcase
    struct _States_updateFlows {
        uint256 newDeposit;
        uint256 newOwedDeposit;
        FlowData oldFlowData;
    }

    function _calculateOwedDeposit(
        int256 currentAllowance,
        int256 allowanceWanted)
        private
        returns (uint256 owedDeposit)
    {
        // allowance wanted could be negative due to refund
        if (allowanceWanted < 0) return 0;
        // current allowance negative means refund is demanded instead
        if (currentAllowance < 0) return 0;
        if (currentAllowance > allowanceWanted) {
            // pay the entire allowance wanted with current allowance
            return allowanceWanted.toUint256();
        } else {
            // use up to current allowance as owed deposit
            return currentAllowance.toUint256();
        }
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

    function _applyAllowanceUsedAndUpdate(
        ISuperfluidToken token,
        address account,
        int256 currentAllowance,
        int256 currentAllowanceUsed,
        int256 allowanceUsed,
        bytes memory ctx,
        bool doUpdateAccountState
    )
        private returns (bytes memory newCtx)
    {
        (int accountAllowanceUsedDelta, int accountBalanceDelta) = _applyAllowanceUsed(
            currentAllowance,
            currentAllowanceUsed,
            allowanceUsed
        );
        if (accountAllowanceUsedDelta != 0) {
            newCtx = ISuperfluid(msg.sender).ctxUpdateAllowanceUsed(ctx, accountAllowanceUsedDelta);
        } else {
            newCtx = ctx;
        }
        if (doUpdateAccountState && (allowanceUsed != 0 || accountAllowanceUsedDelta != 0)) {
            _updateAccountState(
                token,
                account,
                0, // flow rate delta
                allowanceUsed,
                accountAllowanceUsedDelta,
                false
            );
        }
        if (accountBalanceDelta != 0) {
            token.settleBalance(account, accountBalanceDelta);
        }
    }

    function _updateAccountState(
        ISuperfluidToken token,
        address account,
        int96 flowRateDelta,
        int256 depositDelta,
        int256 owedDepositDelta,
        bool settlement
    )
        private
        returns (int96 newNetFlowRate)
    {
        (bool exist, FlowData memory state) = _getAccountState(token, account);
        if(exist && settlement) {
            int256 dynamicBalance = block.timestamp.sub(state.timestamp).toInt256()
                .mul(int256(state.flowRate));
            token.settleBalance(account, dynamicBalance);
        }
        state.flowRate = state.flowRate.add(flowRateDelta);
        state.timestamp = block.timestamp;
        state.deposit = state.deposit.toInt256().add(depositDelta).toUint256();
        state.owedDeposit = state.owedDeposit.toInt256().add(owedDepositDelta).toUint256();

        token.updateAgreementStateSlot(account, 0 /* slot id */, _encodeFlowData(state));
        return state.flowRate;
    }

    function _updateFlowToNonApp(
        ISuperfluidToken token,
        bytes32 flowId,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory ctx,
        AgreementLibrary.Context memory currentContext
    )
        private
        returns (bytes memory newCtx)
    {
        (,int256 owedDepositDelta,) = _updateFlow(
            token, flowId, sender, receiver, flowRate,
            currentContext.allowance, true /* doApplyOwedDeposit */);
        newCtx = _applyAllowanceUsedAndUpdate(
            token,
            sender,
            currentContext.allowance,
            currentContext.allowanceUsed,
            owedDepositDelta,
            ctx,
            false /* doUpdateAccountState */
        );
    }

    function _updateFlowToApp(
        ISuperfluidToken token,
        bytes32 flowId,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory ctx,
        AgreementLibrary.Context memory currentContext,
        bool toCreate
    )
        private
        returns (bytes memory newCtx)
    {
        // apply callbacks
        _States_updateAccountState memory states;
        {
            if (toCreate) {
                (states.cbdata, newCtx) = AgreementLibrary.beforeAgreementCreated(
                    ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
                );
            } else {
                (states.cbdata, newCtx) = AgreementLibrary.beforeAgreementUpdated(
                    ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
                );
            }

            (states.depositDelta, states.owedDepositDelta, states.newFlowData) =
                _updateFlow(token, flowId, sender, receiver, flowRate,
                    currentContext.allowance, false /* doApplyOwedDeposit */);

            // set allowance for the next callback
            states.appAllowance = int256(currentContext.appLevel).mul(states.depositDelta);
            if (toCreate) {
                newCtx = AgreementLibrary.afterAgreementCreated(
                    ISuperfluid(msg.sender),
                    token,
                    ISuperfluid(msg.sender).ctxUpdateAllowance(newCtx, states.appAllowance),
                    address(this),
                    receiver,
                    flowId,
                    states.cbdata
                );
            } else {
                newCtx = AgreementLibrary.afterAgreementUpdated(
                    ISuperfluid(msg.sender),
                    token,
                    ISuperfluid(msg.sender).ctxUpdateAllowance(newCtx, states.appAllowance),
                    address(this),
                    receiver,
                    flowId,
                    states.cbdata
                );
            }

            // the callback will signal allowance used/refunded through the allowanceUsed transaction context
            // TODO move to the library code
            states.appContext = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), newCtx);
            // sanity check of appContext.allowanceUsed return value
            if (states.appAllowance > 0) {
                // app allowance can be used by the app
                // agreement must not refund allowance if not requested (allowance < 0)
                assert(states.appContext.allowanceUsed >= 0);
                // agreement must only use up to allowance given
                assert(states.appContext.allowanceUsed <= states.appAllowance);
                // pay for app allowance
            } else if (states.appAllowance < 0) {
                // app allowance must be refunded
                assert(states.appContext.allowanceUsed == states.appAllowance);
            } // trivial casae no action
        }

        // update owed deposit of current flow
        {
            // _updateFlow doApplyOwedDeposit is set to false,
            // hence owed deposit was reset to zero, allowing a new calculation here
            int256 depositAllowanceWanted = states.newFlowData.deposit.toInt256()
                .add(states.appContext.allowanceUsed);
            states.newFlowData.owedDeposit = _calculateOwedDeposit(
                currentContext.allowance, depositAllowanceWanted);
            token.updateAgreementData(flowId, _encodeFlowData(states.newFlowData));
        }

        newCtx = _applyAllowanceUsedAndUpdate(
            token,
            sender,
            currentContext.allowance,
            currentContext.allowanceUsed,
            states.appContext.allowanceUsed,
            newCtx,
            true /* doUpdateAccountState */);
    }

    function _updateFlow(
        ISuperfluidToken token,
        bytes32 flowId,
        address sender,
        address receiver,
        int96 flowRate,
        int256 currentAllowance,
        bool doApplyOwedDeposit
    )
        private
        returns (
            int256 depositDelta,
            int256 owedDepositDelta,
            FlowData memory newFlowData
        )
    {
        _States_updateFlows memory states;

        (, states.oldFlowData) = _getAgreementData(token, flowId);
        states.newDeposit = _calculateDeposit(token, flowRate);

        if (doApplyOwedDeposit) {
            states.newOwedDeposit = _calculateOwedDeposit(
                currentAllowance, states.newDeposit.toInt256());
        }

        // calculate deposit and owed deposit deltas
        /* require(flowRate == 277777777777777, "flowRate");
        require(states.oldFlowData.deposit == 0, "states.oldFlowData.deposit == 0");
        require(states.newDeposit == 999999997191651328, "states.newDeposit == 999999997191651328"); */
        depositDelta = states.newDeposit.toInt256()
            .sub(states.oldFlowData.deposit.toInt256());
        owedDepositDelta = states.newOwedDeposit.toInt256()
            .sub(states.oldFlowData.owedDeposit.toInt256());

        newFlowData = FlowData(
            block.timestamp,
            flowRate,
            states.newDeposit,
            doApplyOwedDeposit ? states.newOwedDeposit : 0
        );

        token.updateAgreementData(flowId, _encodeFlowData(newFlowData));

        int96 totalSenderFlowRate = _updateAccountState(
            token,
            sender,
            states.oldFlowData.flowRate.sub(flowRate),
            depositDelta,
            owedDepositDelta,
            true
        );
        int96 totalReceiverFlowRate = _updateAccountState(
            token,
            receiver,
            flowRate.sub(states.oldFlowData.flowRate),
            0,
            0,
            true
        );
        emit FlowUpdated(
            token,
            sender,
            receiver,
            flowRate,
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
        (,FlowData memory senderAccountState) = _getAccountState(token, sender);

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

    function _terminateAgreement(
        ISuperfluidToken token,
        bytes32 flowId,
        FlowData memory flowData,
        address sender,
        address receiver
    )
        private
    {
        int96 totalSenderFlowRate = _updateAccountState(
            token,
            sender,
            flowData.flowRate,
            flowData.deposit.toInt256().mul(-1),
            flowData.owedDeposit.toInt256().mul(-1),
            true
        );
        int96 totalReceiverFlowRate = _updateAccountState(
            token,
            receiver,
            flowData.flowRate.mul(-1),
            0,
            0,
            true
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

    function _generateId(address sender, address receiver) private pure returns(bytes32 id) {
        return keccak256(abi.encodePacked(sender, receiver));
    }

    function _flowExists(
        ISuperfluidToken token,
        bytes32 flowId
    )
        internal view
        returns(bool isNewFlow)
    {
        (bool exist,) = _getAgreementData(token, flowId);
        return exist;
    }

    function _calculateDeposit(ISuperfluidToken token, int96 flowRate)
        internal view
        returns(uint256 deposit)
    {
        ISuperfluidGovernance gov = AgreementLibrary.getGovernance();
        uint256 liquidationPeriod = gov.getLiquidationPeriod(token);
        assert(liquidationPeriod <= uint256(type(int96).max));
        deposit = uint256(flowRate.mul(int96(uint96(liquidationPeriod))));
        // clipping the value, and make sure the minimal deposit is not ZERO after clipping
        deposit = deposit >> 32;
        if (deposit == 0) return 1 << 32;
        else return deposit << 32;
    }


    // # Flow data operations
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
        private
        pure
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
        private
        pure
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

    function _getAccountState
    (
        ISuperfluidToken token,
        address account
    )
        private
        view
        returns(bool exist, FlowData memory state)
    {
        bytes32[] memory data = token.getAgreementStateSlot(address(this), account, 0 /* slotId */, 1 /* length */);
        return _decodeFlowData(uint256(data[0]));
    }

    function _getAgreementData
    (
        ISuperfluidToken token,
        bytes32 dId
    )
        private
        view
        returns (bool exist, FlowData memory adata)
    {
        bytes32[] memory data = token.getAgreementData(address(this), dId, 1);
        return _decodeFlowData(uint256(data[0]));
    }
}
