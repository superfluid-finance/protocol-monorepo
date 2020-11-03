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
import { AgreementLibrary } from "./AgreementLibrary.sol";


contract ConstantFlowAgreementV1 is
    AgreementBase,
    IConstantFlowAgreementV1
{

    using SafeMath for uint256;
    using SafeCast for uint256;
    using SignedSafeMath for int256;
    using SafeCast for int256;
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
        AgreementLibrary.Context memory stcCtx = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx);
        bytes32 flowId = _generateId(stcCtx.msgSender, receiver);
        require(_isNewFlow(token, flowId), "Flow already exist");

        if(ISuperfluid(msg.sender).isApp(ISuperApp(receiver)) ||
           ISuperfluid(msg.sender).isApp(ISuperApp(stcCtx.msgSender)))
        {
            // TODO: Decode return cbdata before calling the next step
            bytes memory cbdata;
            (cbdata, newCtx) =
                AgreementLibrary.beforeAgreementCreated(
                    ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
            );
            (uint256 depositSpend, FlowData memory newData) =
                _updateFlow(token, flowId, stcCtx.msgSender, receiver, flowRate, false);

            newCtx = _ctxUpdateDeposit(
                ISuperfluid(msg.sender),
                newCtx,
                receiver,
                depositSpend);
            newCtx = AgreementLibrary.afterAgreementCreated(
                ISuperfluid(msg.sender),
                token,
                newCtx,
                address(this),
                receiver,
                flowId,
                cbdata
            );

            AgreementLibrary.Context memory stcNewCtx = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), newCtx);
            if(stcCtx.allowance == 0 && stcCtx.allowanceUsed == 0) {
                newData.deposit = stcNewCtx.allowanceUsed;
                newData.owedDeposit = 0;
            } else {
                newData.deposit = depositSpend;
                newData.owedDeposit = (stcCtx.allowance > depositSpend
                    ? depositSpend : depositSpend - stcCtx.allowance);
            }
            _updateDeposits(token, newData, stcNewCtx.msgSender, flowId);
            newCtx = AgreementLibrary.updateCtx(
                ISuperfluid(msg.sender),
                newCtx,
                stcNewCtx
            );
        } else {
            _updateFlow(token, flowId, stcCtx.msgSender, receiver, flowRate, true);
            newCtx = ctx;
        }
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
        // TODO meta-tx support
        // TODO: Decode return cbdata before calling the next step
        AgreementLibrary.Context memory stcCtx = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx);
        bytes32 flowId = _generateId(stcCtx.msgSender, receiver);
        require(!_isNewFlow(token, flowId), "Flow doesn't exist");
        //require(sender == msg.sender, "FlowAgreement: only sender can update its own flow");
        if (ISuperfluid(msg.sender).isApp(ISuperApp(receiver))) {
            bytes memory cbdata;
            (cbdata, newCtx) =
                AgreementLibrary.beforeAgreementUpdated(
                ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
            );

            (uint256 depositSpend, FlowData memory newData) =
                _updateFlow(token, flowId, stcCtx.msgSender, receiver, flowRate, false);
            newCtx = _ctxUpdateDeposit(
                ISuperfluid(msg.sender),
                newCtx,
                receiver,
                depositSpend);
            newCtx = AgreementLibrary.afterAgreementUpdated(
                ISuperfluid(msg.sender),
                token,
                newCtx,
                address(this),
                receiver,
                flowId,
                cbdata
            );

            AgreementLibrary.Context memory stcNewCtx = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), newCtx);
            if(stcCtx.allowance == 0 && stcCtx.allowanceUsed == 0) {
                newData.deposit = stcNewCtx.allowanceUsed;
                newData.owedDeposit = 0;
            } else {
                newData.deposit = depositSpend;
                newData.owedDeposit = (stcCtx.allowance > depositSpend
                    ? depositSpend : depositSpend - stcCtx.allowance);
            }

            _updateDeposits(token, newData, stcNewCtx.msgSender, flowId);
            newCtx = AgreementLibrary.updateCtx(
                ISuperfluid(msg.sender),
                newCtx,
                stcNewCtx
            );
        } else {
            _updateFlow(token, flowId, stcCtx.msgSender, receiver, flowRate, true);
            newCtx = ctx;
        }
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
        bytes32 flowId = _generateId(sender, receiver);
        (bool exist, FlowData memory flowData) = _getAgreementData(token, flowId);
        require(exist, "FlowAgreement: flow does not exist");

        address msgSender = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), ctx).msgSender;
        bool isLiquidator = (msgSender != sender && msgSender != receiver);

        if (isLiquidator) {
            require(token.isAccountInsolvent(sender),
                    "FlowAgreement: account is solvent");
        }

        bytes memory cbdata;
        (cbdata, newCtx) =
            AgreementLibrary.beforeAgreementTerminated(
                ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
        );
        // TODO: Decode return cbdata before calling the next step

        if (isLiquidator) {
            _liquidateAgreement(token, flowId, flowData, sender, msgSender);
        }
        _terminateAgreement(token, flowId, flowData, sender, receiver);

        newCtx = _ctxUpdateDeposit(
            ISuperfluid(msg.sender),
            newCtx,
            receiver,
            0);
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

        return(
            data.timestamp,
            data.flowRate,
            data.deposit,
            data.owedDeposit
        );
    }

    /// @dev IFlowAgreement.getNetFlow implementation
    function getNetFlow(
        ISuperfluidToken token,
        address account
    )
        external
        view
        override
        returns (int96 flowRate)
    {
        (, FlowData memory state) = _getAccountState(token, account);
        return state.flowRate;
    }

    /*
     * Internal Functions
     */
    function _updateAccountState(
        ISuperfluidToken token,
        address account,
        int96 flowRate,
        uint256 deposit,
        uint256 owedDeposit,
        bool settlement
    )
        private
        returns (int96 newNetFlowRate)
    {
        (bool exist, FlowData memory state) = _getAccountState(token, account);
        if(exist && settlement) {
            int256 dynamicBalance =
                ((int256(block.timestamp).sub(int256(state.timestamp))).mul(state.flowRate));
            token.settleBalance(account, dynamicBalance);
        }
        state.flowRate += flowRate;
        state.timestamp = block.timestamp;
        state.deposit += deposit;
        state.owedDeposit += owedDeposit;

        token.updateAgreementStateSlot(account, 0, _encodeAccountState(state));
        return state.flowRate;
    }

    function _updateFlow(
        ISuperfluidToken token,
        bytes32 flowId,
        address sender,
        address receiver,
        int96 flowRate,
        bool chargeDeposit
    )
        private
        returns(uint256 allowanceUsed, FlowData memory newData)
    {
        require(sender != receiver, "FlowAgreement: self flow not allowed");
        require(flowRate != 0, "FlowAgreement: use delete flow");
        require(flowRate > 0, "FlowAgreement: negative flow rate not allowed");


        //bytes32 flowId = _generateId(sender, receiver);
        (, FlowData memory data) = _getAgreementData(token, flowId);
        allowanceUsed = _minimalDeposit(token, uint256(flowRate));
        if(chargeDeposit) {
            (int256 availabelBalance, ,) = token.realtimeBalanceOf(sender, block.timestamp);
            require(availabelBalance > int256(allowanceUsed), "CFA: not enough available balance");
        }

        newData = FlowData(
            block.timestamp,
            flowRate,
            chargeDeposit ? allowanceUsed : 0,
            0
        );

        token.updateAgreementData(flowId, _encodeAgreementData(newData));

        int96 totalSenderFlowRate = _updateAccountState(
            token,
            sender,
            -(flowRate - data.flowRate),
            chargeDeposit ? allowanceUsed : 0,
            0,
            true
        );
        int96 totalReceiverFlowRate = _updateAccountState(
            token,
            receiver,
            flowRate - data.flowRate,
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

        (int256 availableBalance,,) = token.realtimeBalanceOf(sender, block.timestamp);

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
            -flowData.deposit,
            -flowData.owedDeposit,
            true
        );
        int96 totalReceiverFlowRate = _updateAccountState(
            token,
            receiver,
            -flowData.flowRate,
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
        require(sender != address(0), "Sender is zero");
        require(receiver != address(0), "Receiver is zero");

        return keccak256(abi.encodePacked(sender, receiver));
    }

    function _isNewFlow(
        ISuperfluidToken token,
        bytes32 flowId
    )
        internal
        view
        returns(bool isNewFlow)
    {
        (bool exist, ) = _getAgreementData(token, flowId);
        return !exist;
    }

    function _minimalDeposit(ISuperfluidToken token, uint256 flowRate) internal view returns(uint256 deposit) {
        ISuperfluidGovernance gov = AgreementLibrary.getGovernance();
        uint256 liquidationPeriod = gov.getLiquidationPeriod(token);
        deposit = flowRate * liquidationPeriod;
    }

    function _updateDeposits(
        ISuperfluidToken token,
        FlowData memory data,
        address account,
        bytes32 flowId
    )
        internal
    {
        //update data deposit and save it
        token.updateAgreementData(flowId, _encodeAgreementData(data));
        (int256 availabelBalance, ,) = token.realtimeBalanceOf(account, block.timestamp);
        require(availabelBalance >= int256(data.owedDeposit - data.deposit), "CFA: not enough available balance");
        //update state
        _updateAccountState(
            token,
            account,
            0,
            data.deposit,
            data.owedDeposit,
            false
        );
    }

    function _encodeAccountState
    (
        FlowData memory astate
    )
        private
        pure
        returns(bytes32[] memory state)
    {
        state = new bytes32[](1);
        state[0] = bytes32(
            (uint256(astate.timestamp)) << 224 |
            (uint256(uint96(astate.flowRate)) << 128) |
            (uint256(astate.deposit)) <<  64 |
            (uint256(astate.owedDeposit))
        );
    }

    function _encodeAgreementData
    (
        FlowData memory adata
    )
        private
        pure
        returns (bytes32[] memory data)
    {
        data = new bytes32[](1);
        data[0] = bytes32(
            (uint256(adata.timestamp) << 224) |
            (uint256(uint96(adata.flowRate)) << 128) |
            (uint256(adata.deposit) <<  64) |
            (uint256(adata.owedDeposit))
        );
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
        bytes32[] memory data = token.getAgreementStateSlot(address(this), account, 0, 1);
        uint256 wordA = uint256(data[0]);
        exist = wordA > 0;
        if (exist) {
            state.timestamp = uint32(wordA >> 224);
            state.flowRate = int96((wordA >> 128) & uint96(int96(-1)));
            state.deposit = uint64((wordA >> 64) & uint64(int64(-1)));
            state.owedDeposit = uint64(wordA & uint64(int64(-1)));
        }
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
        uint256 wordA = uint256(data[0]);
        exist = wordA > 0;
        if (exist) {
            adata.timestamp = uint32(wordA >> 224);
            adata.flowRate = int96((wordA >> 128) & uint96(int96(-1)));
            adata.deposit = uint64(wordA >> 64 & uint64(-1));
            adata.owedDeposit = uint64(wordA & uint64(-1));
        }
    }

    function _ctxUpdateDeposit(
        ISuperfluid host,
        bytes memory ctx,
        address receiver,
        uint256 unitOfAllowance
    )
        private
        returns(bytes memory newCtx)
    {
        if (unitOfAllowance == 0) return ctx;
        AgreementLibrary.Context memory context = AgreementLibrary.decodeCtx(host, ctx);
        // TODO review this rule
        uint256 level = uint256(host.getAppLevel(ISuperApp(receiver)));
        context.allowanceUsed +=
            (unitOfAllowance > context.allowance ?
             unitOfAllowance - context.allowance :
             unitOfAllowance);

        context.allowance = (level * unitOfAllowance);
        newCtx = AgreementLibrary.updateCtx(host, ctx, context);
    }
}
