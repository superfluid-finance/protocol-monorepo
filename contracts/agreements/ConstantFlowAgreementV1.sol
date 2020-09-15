// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { IConstantFlowAgreementV1, ISuperToken } from "../interfaces/IConstantFlowAgreementV1.sol";
import { ISuperfluidGovernance } from "../interfaces/ISuperfluidGovernance.sol";
import { ISuperfluid } from "../interfaces/ISuperfluid.sol";
import { Math } from "@openzeppelin/contracts/math/Math.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";
import { ContextLibrary } from "../superfluid/ContextLibrary.sol";

contract ConstantFlowAgreementV1 is IConstantFlowAgreementV1 {

    using SignedSafeMath for int256;
    using SafeMath for uint256;
    using ContextLibrary for ContextLibrary.Context;

    /*
     * ISuperAgreement interface
     */
    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        ISuperToken /* token */,
        address /* account */,
        bytes calldata data,
        uint256 time
    )
        external
        pure
        override
        returns (int256 availabelBalance, int256 deposit, int256 owedDeposit)
    {
        uint256 startDate;
        int256 flowRate;

        (startDate, flowRate, deposit, owedDeposit) = _decodeFlow(data);
        availabelBalance = ((int256(time).sub(int256(startDate))).mul(flowRate));
    }

    /// @dev ISuperAgreement.touch implementation
    function touch(
        address /* account */,
        bytes memory currentData,
        uint256 timestamp
    )
        public
        pure
        override
        returns(bytes memory newData)
    {
        (, int256 cRate, int256 cDeposit, int256 cOwned) = _decodeFlow(currentData);
        return _encodeFlow(timestamp, cRate, cDeposit, cOwned);
    }

    /// @dev IFlowAgreement.createFlow implementation
    function createFlow(
        ISuperToken token,
        address receiver,
        int256 flowRate,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        // TODO: Decode return cbdata before calling the next step
        ContextLibrary.Context memory stcCtx = ContextLibrary.decode(ctx);
        bytes32 flowId = _generateId(stcCtx.msgSender, receiver);
        require(_isNewFlow(token, flowId), "Flow already exist");
        bytes memory cbdata;
        (cbdata, newCtx) =
            AgreementLibrary.beforeAgreementCreated(
                ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
        );
        (int256 depositSpend, ) = _updateFlow(token, stcCtx.msgSender, receiver, flowRate);
        newCtx = ContextLibrary.updateCtxDeposit(ISuperfluid(msg.sender), receiver, newCtx, depositSpend);
        newCtx = AgreementLibrary.afterAgreementCreated(
            ISuperfluid(msg.sender),
            token,
            newCtx,
            address(this),
            receiver,
            flowId,
            cbdata
        );

        ContextLibrary.Context memory stcNewCtx = ContextLibrary.decode(newCtx);
        if(stcCtx.allowance == 0 && stcCtx.allowanceUsed == 0) {
            _chargeDeposit(
                token,
                stcCtx.msgSender,
                flowId,
                stcNewCtx.allowanceUsed,
                0
            );
        } else {
            _chargeDeposit(
                token,
                stcNewCtx.msgSender,
                flowId,
                depositSpend,
                (stcCtx.allowance > depositSpend ? depositSpend : depositSpend - stcCtx.allowance)
            );
        }
        (newCtx, ) = ContextLibrary.encode(stcNewCtx);
    }

    function updateFlow(
        ISuperToken token,
        address receiver,
        int256 flowRate,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        // TODO meta-tx support
        // TODO: Decode return cbdata before calling the next step
        address sender = ContextLibrary.decode(ctx).msgSender;
        bytes32 flowId = _generateId(sender, receiver);
        //require(flowRate > 0, "use delete flow");
        require(!_isNewFlow(token, flowId), "Flow doesn't exist");
        //require(sender == msg.sender, "FlowAgreement: only sender can update its own flow");
        bytes memory cbdata;
        (cbdata, newCtx) =
            AgreementLibrary.beforeAgreementUpdated(
            ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
        );
        (int256 depositSpend, int256 oldDeposit) = _updateFlow(token, sender, receiver, flowRate);
        _updateDeposit(token, sender, depositSpend - oldDeposit, depositSpend);
        newCtx = AgreementLibrary.afterAgreementUpdated(
            ISuperfluid(msg.sender),
            token,
            ContextLibrary.updateCtxDeposit(ISuperfluid(msg.sender), receiver, newCtx, depositSpend),
            address(this),
            receiver,
            flowId,
            cbdata
        );
    }

    /// @dev IFlowAgreement.deleteFlow implementation
    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        // TODO: Decode return cbdata before calling the next step
        address msgSender = ContextLibrary.decode(ctx).msgSender;
        bytes32 flowId = _generateId(sender, receiver);
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
        _terminateAgreementData(token, sender, receiver, isLiquidator);
        newCtx = AgreementLibrary.afterAgreementTerminated(
            ISuperfluid(msg.sender),
            token,
            ContextLibrary.updateCtxDeposit(ISuperfluid(msg.sender), receiver, newCtx, 0),
            address(this),
            receiver,
            flowId,
            cbdata
        );
    }

    /// @dev IFlowAgreement.getFlow implementation
    function getFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        view
        override
        returns (int256 flowRate)
    {
        bytes memory data = token.getAgreementData(address(this), keccak256(abi.encodePacked(sender, receiver)));
        (, , , flowRate, , ) = _decodeData(data);
    }
    /// @dev IFlowAgreement.getNetFlow implementation
    function getFlow(
        ISuperToken token,
        bytes32 flowId
    )
        external
        view
        override
        returns(
            uint256 timestamp,
            address sender,
            address receiver,
            int256 flowRate,
            int256 deposit,
            int256 owedDeposit
        )
    {
        bytes memory data = token.getAgreementData(address(this), flowId);
        return _decodeData(data);
    }

    function getNetFlow(
        ISuperToken token,
        address account
    )
        external
        view
        override
        returns (int256 flowRate)
    {
        bytes memory state = token.getAgreementAccountState(address(this), account);
        (, flowRate, , ) = _decodeFlow(state);
    }

    /*
     * Internal Functions
     */

    function _updateAccountState(
        ISuperToken token,
        address account,
        int256 flowRate,
        int256 deposit,
        int256 owedDeposit
    )
        private
        returns(int256 newFlowRate)
    {
        bytes memory state = token.getAgreementAccountState(address(this), account);
        state = _composeState(
            state,
            flowRate,
            block.timestamp,
            deposit,
            owedDeposit
        );
        token.updateAgreementAccountState(account, state);
        (, newFlowRate, , ) = _decodeFlow(state);
    }

    function _updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        private
        returns(int256 allowanceUsed, int256 oldDeposit)
    {
        require(sender != receiver, "FlowAgreement: self flow not allowed");
        require(flowRate != 0, "FlowAgreement: use delete flow");
        require(flowRate > 0, "FlowAgreement: negative flow rate not allowed");
        bytes32 flowId = _generateId(sender, receiver);
        bytes memory oldFlowData = token.getAgreementData(address(this), flowId);
        int256 oldFlowRate;
        (, , , oldFlowRate, oldDeposit, ) = _decodeData(oldFlowData);
        allowanceUsed = _minimalDeposit(token, flowRate);
        bytes memory newFlowData = _encodeData(
            block.timestamp,
            sender,
            receiver,
            flowRate,
            0,
            0
        );
        token.createAgreement(flowId, newFlowData);
        int flowRateDelta = flowRate - oldFlowRate;
        int256 totalSenderFlowRate = _updateAccountState(token, sender, _mirrorFlowRate(flowRateDelta), 0, 0);
        int256 totalReceiverFlowRate = _updateAccountState(token, receiver, flowRateDelta, 0, 0);
        emit FlowUpdated(
            token,
            sender,
            receiver,
            flowRate,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _terminateAgreementData(
        ISuperToken token,
        address sender,
        address receiver,
        bool liquidation
    )
        private
        returns(int256 deposit, int256 owedDeposit)
    {
        bytes32 flowId = _generateId(sender, receiver);
        bytes memory flowData = token.getAgreementData(address(this), flowId);
        require(flowData.length > 0, "FlowAgreement: flow does not exist");
        int256 senderFlowRate;
        (, , , senderFlowRate, deposit, owedDeposit) = _decodeData(flowData);
        require(senderFlowRate > 0, "FlowAgreement: sender flow rate must be positive");

        int256 totalSenderFlowRate = _updateAccountState(token, sender, senderFlowRate, -deposit, -owedDeposit);
        int256 totalReceiverFlowRate = _updateAccountState(
            token,
            receiver,
            _mirrorFlowRate(senderFlowRate),
            -deposit,
            -owedDeposit
        );

        // Close this Agreement Data
        if (liquidation) {
            token.liquidateAgreement(
                msg.sender,
                flowId,
                sender,
                deposit
            );
        } else {
            token.terminateAgreement(flowId);
        }

        emit FlowUpdated(
            token,
            sender,
            receiver,
            0,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _mirrorFlowRate(int256 flowRate) private pure returns(int256 mirrorFlowRate) {
        return -1 * flowRate;
    }

    function _generateId(address sender, address receiver) private pure returns(bytes32 id) {
        require(sender != address(0), "Sender is zero");
        require(receiver != address(0), "Receiver is zero");

        return keccak256(abi.encodePacked(sender, receiver));
    }

    function _encodeData
    (
        uint256 timestamp,
        address sender,
        address receiver,
        int256 flowRate,
        int256 deposit,
        int256 owedDeposit
    )
        private
        pure
        returns(bytes memory data)
    {
        return abi.encode(timestamp, sender, receiver, flowRate, deposit, owedDeposit);
    }

    function _decodeData
    (
        bytes memory data
    )
        private
        pure
        returns
    (
        uint256 timestamp,
        address sender,
        address receiver,
        int256 flowRate,
        int256 deposit,
        int256 owedDeposit
    )
    {
        if (data.length == 0) return (0, address(0), address(0), 0, 0, 0);
        //require(data.length == 136, "FlowAgreement: invalid data");
        return abi.decode(data, (uint256, address, address, int256, int256, int256));
    }


    /// Encoders & Decoders
    /// @dev Encode the parameters into a bytes type.
    ///      Both data and state share the same data structure.
    function _encodeFlow
    (
        uint256 timestamp,
        int256 flowRate,
        int256 deposit,
        int256 owedDeposit
    )
        private
        pure
        returns (bytes memory flow)
    {
        return abi.encode(timestamp, flowRate, deposit, owedDeposit);
    }

    /// @dev Decode the parameter into the original types
    function _decodeFlow
    (
        bytes memory state
    )
        private
        pure
        returns
    (
        uint256 timestamp,
        int256 flowrate,
        int256 deposit,
        int256 owneddeposit
    )
    {
        if (state.length == 0) return (0, 0, 0, 0);
        require(state.length == 128, "FlowAgreement: invalid state");
        return abi.decode(state, (uint256, int256, int256, int256));
    }

    /// @notice Compose in one state the states passed as arguments.
    /// @dev Will add the two state and update the `timestamp` to block.timestamp
    /// @dev If end result is a FlowRate of zero then return String.Empty
    /// @param currentState Data of the actual agreement
    /// @param flowRate New value to update
    /// @param timestamp New time to update
    /// @return newAgreement New agreement data
    function _composeState
    (
        bytes memory currentState,
        int256 flowRate,
        uint256 timestamp,
        int256 deposit,
        int256 owedDeposit
    )
        private
        pure
        returns (bytes memory newAgreement)
    {
        (, int256 cRate, int256 cDeposit, int256 cOwned) = _decodeFlow(currentState);
        cRate = cRate.add(flowRate);
        cDeposit = cDeposit.add(deposit);
        cOwned = cOwned.add(owedDeposit);
        /*
        if (cRate == 0) {
            return "";
        }
        */
        return _encodeFlow(timestamp, cRate, cDeposit, cOwned);
    }

    function _isNewFlow(
        ISuperToken token,
        bytes32 flowId
    )
        internal
        view
        returns(bool isNewFlow)
    {
        bytes memory data = token.getAgreementData(address(this), flowId);
        return (data.length == 0);
    }

    function _minimalDeposit(ISuperToken token, int256 flowRate) internal view returns(int256 deposit) {
        ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernanceAddress());
        uint16 liquidationPeriod = gov.getLiquidationPeriod(token.getUnderlayingToken());
        deposit = flowRate * liquidationPeriod;
    }

    function _chargeDeposit(
        ISuperToken token,
        address account,
        bytes32 flowId,
        int256 charge,
        int256 maxAllowance
    )
        internal
    {
        token.chargeDeposit(
            account,
            flowId,
            charge,
            _updateDepositData(token, flowId, charge, maxAllowance),
            _updateDepositState(token, account, charge, maxAllowance)
        );
    }

    function  _updateDeposit(
        ISuperToken token,
        address account,
        int256 charge,
        int256 maxAllowance
    )
        internal
    {
        token.updateDeposit(
            account,
            _updateDepositState(token, account, charge, maxAllowance)
        );
    }

    function _updateDepositState(
        ISuperToken token,
        address account,
        int256 charge,
        int256 maxAllowance
    )
        internal
        view
        returns(bytes memory state)
    {
        (
            uint256 cTimestamp,
            int256 cFlowRate,
            int256 cDeposit,
            int256 cOwned
        ) = _decodeFlow(token.getAgreementAccountState(address(this), account));

        //cDeposit = cDeposit.add(charge);
        //cOwned = cOwned.add(maxAllowance);
        cDeposit += charge;
        cOwned += maxAllowance;
        return _encodeFlow(cTimestamp, cFlowRate, cDeposit ,cOwned);
    }

    function _updateDepositData(
        ISuperToken token,
        bytes32 flowId,
        int256 charge,
        int256 maxAllowance
    )
        internal
        view
        returns(bytes memory data)
    {
        (
            uint256 cTimestamp,
            address cSender,
            address cReceiver,
            int256 cFlowRate,
            int256 cDeposit,
            int256 cOwned
        ) = _decodeData(token.getAgreementData(address(this), flowId));

        cDeposit = charge;
        cOwned = maxAllowance;
        return _encodeData(
            cTimestamp,
            cSender,
            cReceiver,
            cFlowRate,
            cDeposit,
            cOwned
        );
    }
}
