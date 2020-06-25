// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.6.6;

import { ISuperToken, IFlowAgreement } from "./interface/IFlowAgreement.sol";
import { ISuperfluidGovernance } from "./interface/ISuperfluidGovernance.sol";
import { Math } from "@openzeppelin/contracts/math/Math.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";

contract FlowAgreement is IFlowAgreement {

    using SignedSafeMath for int256;

    /*
     * ISuperAgreement interface
     */
    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
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

        (startDate, flowRate) = _decodeFlow(data);
        return (int256(time).sub(int256(startDate))).mul(flowRate);
    }

    /// @dev ISuperAgreement.touch implementation
    function touch(
        bytes memory currentData,
        uint256 timestamp
    )
        public
        pure
        override
        returns(bytes memory newData)
    {
        (, int256 cRate) = _decodeFlow(currentData);
        return _encodeFlow(timestamp, cRate);
    }

    /*
     *   IFlowAgreement interface
     */

    /// @dev IFlowAgreement.updateFlow implementation
    function updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        external
        override
    {
        // TODO meta-tx support
        require(sender == msg.sender, "FlowAgreement: only sender can update its own flow");
        _updateFlow(token, sender, receiver, flowRate);
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
        bytes32 flowId = _generateId(sender, receiver);
        bytes memory data = token.getAgreementData(address(this), flowId);
        (, flowRate) = _decodeFlow(data);
    }

    /// @dev IFlowAgreement.getNetFlow implementation
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
        (, flowRate) = _decodeFlow(state);
    }

    /// @dev IFlowAgreement.deleteFlow implementation
    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        override
    {
        bool isLiquidator = (msg.sender != sender && msg.sender != receiver);
        if (isLiquidator) {
            require(token.isAccountInsolvent(sender), "FlowAgreement: account is solvent");
        }

        _terminateAgreementData(token, sender, receiver, isLiquidator);
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
        state = _composeState(state, flowRate, block.timestamp);
        token.updateAgreementAccountState(account, state);
        (, newFlowRate) = _decodeFlow(state);
    }

    function _updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        private
    {
        require(flowRate != 0, "FlowAgreement: use delete flow");
        require(flowRate > 0, "FlowAgreement: negative flow rate not allowed");

        bytes32 flowId = _generateId(sender, receiver);
        bytes memory oldFlowData = token.getAgreementData(address(this), flowId);
        (, int256 oldFlowRate) = _decodeFlow(oldFlowData);
        bytes memory newFlowData = _encodeFlow(block.timestamp, flowRate);
        token.createAgreement(flowId, newFlowData);
        (, int256 newFlowRate) = _decodeFlow(newFlowData);
        int flowRateDelta = newFlowRate - oldFlowRate;

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
        (, int256 senderFlowRate) = _decodeFlow(flowData);
        require(senderFlowRate > 0, "FlowAgreement: sender flow rate must be positive");

        int256 totalSenderFlowRate = _updateAccountState(token, sender, senderFlowRate);
        int256 totalReceiverFlowRate = _updateAccountState(token, receiver, _mirrorFlowRate(senderFlowRate));

        // note : calculate the deposit here and pass it to superToken, and emit the events.
        // Close this Agreement Data
        if (liquidation) {
            ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernanceAddress());
            uint16 minDeposit = gov.getMinimalDeposit(token.getUnderlayingToken());
            uint16 liquidationPeriod = gov.getLiquidationPeriod(token.getUnderlayingToken());
            uint256 deposit = Math.max(
                uint256(minDeposit),
                uint256(senderFlowRate) * uint256(liquidationPeriod));

            token.liquidateAgreement(msg.sender, flowId, sender, deposit);
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
        return keccak256(abi.encodePacked(sender, receiver));
    }

    /// Encoders & Decoders
    /// @dev Encode the parameters into a bytes type.
    ///      Both data and state share the same data structure.
    function _encodeFlow
    (
        uint256 timestamp,
        int256 flowRate
    )
        private
        pure
        returns (bytes memory)
    {
        return abi.encodePacked(timestamp, flowRate);
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
        int256 flowRate
    )
    {
        if (state.length == 0) return (0, 0);
        require(state.length == 64, "FlowAgreement: invalid state");
        return abi.decode(state, (uint256, int256));
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
        uint256 timestamp
    )
        private
        pure
        returns (bytes memory newAgreement)
    {
        (, int cRate) = _decodeFlow(currentState);
        cRate = cRate.add(flowRate);

        if (cRate == 0) {
            return "";
        }
        return _encodeFlow(timestamp, cRate);
    }
}
