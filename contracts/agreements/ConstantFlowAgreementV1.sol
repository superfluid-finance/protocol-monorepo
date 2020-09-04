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

    function agreementType() external override pure returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    }

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
        returns (int256 balance)
    {
        uint256 startDate;
        int256 flowRate;
        uint256 deposit;

        (startDate, flowRate, deposit) = _decodeFlow(data);
        return ((int256(time).sub(int256(startDate))).mul(flowRate).sub(int256(deposit)));
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
        (, int256 cRate, uint256 cDeposit) = _decodeFlow(currentData);
        return _encodeFlow(timestamp, cRate, cDeposit);
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
        (, address host) = token.getFramework();
        address sender = ContextLibrary.decode(ctx).msgSender;
        bytes32 flowId = _generateId(sender, receiver);
        require(_isNewFlow(token, flowId), "Flow already exist");
        bytes memory cbdata;
        (cbdata, newCtx) =
            AgreementLibrary.beforeAgreementCreated(
                ISuperfluid(host), token, ctx, address(this), receiver, flowId
        );
        _updateFlow(token, sender, receiver, flowRate);
        newCtx = AgreementLibrary.afterAgreementCreated(
            ISuperfluid(host),
            token,
            _updateCtxDeposit(host, receiver, newCtx),
            address(this),
            receiver,
            flowId,
            cbdata
        );
        //check initial credit
        //c
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
        (, address host) = token.getFramework();
        require(host == msg.sender, "Only Superfluid can interact with FlowAgreement");
        address sender = ContextLibrary.decode(ctx).msgSender;
        bytes32 flowId = _generateId(sender, receiver);
        //require(flowRate > 0, "use delete flow");
        require(!_isNewFlow(token, flowId), "Flow doesn't exist");
        //require(sender == msg.sender, "FlowAgreement: only sender can update its own flow");
        bytes memory cbdata;
        (cbdata, newCtx) =
            AgreementLibrary.beforeAgreementUpdated(
            ISuperfluid(host), token, ctx, address(this), receiver, flowId
        );
        _updateFlow(token, sender, receiver, flowRate);
        newCtx = AgreementLibrary.afterAgreementUpdated(
            ISuperfluid(host),
            token,
            _updateCtxDeposit(host, receiver, newCtx),
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
        (, address host) = token.getFramework();
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
                ISuperfluid(host), token, ctx, address(this), receiver, flowId
        );
        _terminateAgreementData(token, sender, receiver, isLiquidator);
        newCtx = AgreementLibrary.afterAgreementTerminated(
            ISuperfluid(host),
            token,
            _updateCtxDeposit(host, receiver, newCtx),
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
        (, , , flowRate, ) = _decodeData(data);
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
            uint256 deposit
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
        (, flowRate, ) = _decodeFlow(state);
    }

    /*
     * Internal Functions
     */

    function _updateAccountState(
        ISuperToken token,
        address account,
        int256 flowRate
    )
        private
        returns(int256 newFlowRate)
    {
        bytes memory state = token.getAgreementAccountState(address(this), account);
        state = _composeState(
            state,
            flowRate,
            block.timestamp,
            flowRate < 0 ? _minimalDeposit(token, flowRate) : 0
        );
        token.updateAgreementAccountState(account, state);
        (, newFlowRate, ) = _decodeFlow(state);
    }

    function _updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        private
    {
        require(sender != receiver, "FlowAgreement: self flow not allowed");
        require(flowRate != 0, "FlowAgreement: use delete flow");
        require(flowRate > 0, "FlowAgreement: negative flow rate not allowed");
        bytes32 flowId = _generateId(sender, receiver);
        bytes memory oldFlowData = token.getAgreementData(address(this), flowId);
        (, , , int256 oldFlowRate, ) = _decodeData(oldFlowData);
        bytes memory newFlowData = _encodeData(
            block.timestamp,
            sender,
            receiver,
            flowRate,
            _minimalDeposit(token, flowRate)
        );
        token.createAgreement(flowId, newFlowData);

        //(, int256 newFlowRate) = _decodeFlow(newFlowData);
        int flowRateDelta = flowRate - oldFlowRate;

        int256 totalSenderFlowRate = _updateAccountState(token, sender, _mirrorFlowRate(flowRateDelta));
        int256 totalReceiverFlowRate = _updateAccountState(token, receiver, flowRateDelta);
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
    {
        bytes32 flowId = _generateId(sender, receiver);
        bytes memory flowData = token.getAgreementData(address(this), flowId);
        require(flowData.length > 0, "FlowAgreement: flow does not exist");
        (, , , int256 senderFlowRate, ) = _decodeData(flowData);
        require(senderFlowRate > 0, "FlowAgreement: sender flow rate must be positive");

        int256 totalSenderFlowRate = _updateAccountState(token, sender, senderFlowRate);
        int256 totalReceiverFlowRate = _updateAccountState(token, receiver, _mirrorFlowRate(senderFlowRate));

        // note : calculate the deposit here and pass it to superToken, and emit the events.
        // Close this Agreement Data
        if (liquidation) {
            token.liquidateAgreement(
                msg.sender,
                flowId,
                sender,
                _minimalDeposit(token, senderFlowRate)
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

    function _generateId(address sender, address receiver) private pure returns(bytes32) {
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
        uint256 deposit
    )
        private
        pure
        returns(bytes memory)
    {
        return abi.encode(timestamp, sender, receiver, flowRate, deposit);
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
        uint256 deposit
    )
    {
        if (data.length == 0) return (0, address(0), address(0), 0, 0);
        //require(data.length == 104, "FlowAgreement: invalid data");
        return abi.decode(data, (uint256, address, address, int256, uint256));
    }


    /// Encoders & Decoders
    /// @dev Encode the parameters into a bytes type.
    ///      Both data and state share the same data structure.
    function _encodeFlow
    (
        uint256 timestamp,
        int256 flowRate,
        uint256 deposit
    )
        private
        pure
        returns (bytes memory)
    {
        return abi.encode(timestamp, flowRate, deposit);
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
        int256 flowRate,
        uint256 deposit
    )
    {
        if (state.length == 0) return (0, 0, 0);
        require(state.length == 96, "FlowAgreement: invalid state");
        return abi.decode(state, (uint256, int256, uint256));
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
        uint256 /*deposit*/
    )
        private
        pure
        returns (bytes memory newAgreement)
    {
        (, int256 cRate, uint256 cDeposit) = _decodeFlow(currentState);
        cRate = cRate.add(flowRate);
        //cDeposit = cDeposit.add(deposit);

        if (cRate == 0) {
            return "";
        }
        return _encodeFlow(timestamp, cRate, cDeposit);
    }

    function _isNewFlow(
        ISuperToken token,
        bytes32 flowId
    )
        internal
        view
        returns(bool)
    {
        bytes memory data = token.getAgreementData(address(this), flowId);
        return (data.length == 0);
    }

    function _minimalDeposit(ISuperToken token, int256 flowRate) internal view returns(uint256 deposit) {
        ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernanceAddress());
        uint16 minDeposit = gov.getMinimalDeposit(token.getUnderlayingToken());
        uint16 liquidationPeriod = gov.getLiquidationPeriod(token.getUnderlayingToken());
        deposit = Math.max(
            uint256(minDeposit),
            uint256(flowRate) * uint256(liquidationPeriod));
    }
    //this go to the library
    function _updateCtxDeposit(address host, address app, bytes memory ctx) internal returns(bytes memory newCtx) {
        if(ISuperfluid(host).isApp(app)) {
            return ISuperfluid(host).updateCtxDeposit(newCtx);
        }

        return ctx;
    }
}
