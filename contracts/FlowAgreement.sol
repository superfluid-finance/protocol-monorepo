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
        require(sender === msg.sender, "Only msg.sender can be sender");
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
        (bytes32 outFlowId, ) = _hashAccounts(sender, receiver);
        bytes memory data = token.getAgreementData(address(this), outFlowId);
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
        bool isLiquidation = (msg.sender != sender && msg.sender != receiver);
        if (isLiquidation) {
            require(token.isAccountInsolvent(sender), "Account is solvent");
        }

        _terminateAgreementData(token, sender, receiver, isLiquidation);
    }

    /*
     * Internal Functions
     */

    function _updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        private
    {
        require(flowRate != 0, "Invalid FlowRate, use deleteFlow function");
        bytes memory data = _encodeFlow(block.timestamp, flowRate);

        _updateAgreementData(token, sender, receiver, data);
        _updateAccountState(token, sender, receiver, flowRate);
    }

    function _updateAgreementData(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory additionalData
    )
        private
    {
        bytes32 flowId = _generateId(sender, receiver);
        /// aliace -> bob 10 / mo
        ///
        bytes memory senderData = token.getAgreementData(address(this), outFlowId);
        //bytes memory receiverData = token.getAgreementData(address(this), inFlowId);

        senderData = _composeData(senderData, _mirrorAgreementData(additionalData));
        //receiverData = _composeData(receiverData, additionalData);
        (, int256 flowRate) = _decodeFlow(senderData);

        require(flowRate <= 0, "Revert flow not allowed");
        token.createAgreement(outFlowId, senderData);
        //token.createAgreement(inFlowId, receiverData);
    }

    function _updateAccountState(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        private
    {
        int256 totalSenderFlowRate = _updateAccountState(token, sender, _mirrorFlowRate(flowRate));
        int256 totalReceiverFlowRate = _updateAccountState(token, receiver, flowRate);
        emit FlowUpdated(
            token,
            sender,
            receiver,
            flowRate,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _updateAccountState(
        ISuperToken token,
        address account,
        int256 flowRate
    )
        private
        returns(int256)
    {
        bytes memory state = token.getAgreementAccountState(address(this), account);
        state = _composeState(state, flowRate, block.timestamp);
        token.updateAgreementAccountState(account, state);
        (, int256 newFlowRate) = _decodeFlow(state);
        return newFlowRate;
    }

    function _terminateAgreementData(
        ISuperToken token,
        address sender,
        address receiver,
        bool liquidation
    )
        private
    {
        (bytes32 outFlowId, ) = _hashAccounts(sender, receiver);
        (int256 senderFlowRate, int256 totalSenderFlowRate) = _updateAccountStateWithData(
            token, sender, outFlowId, true
        );
        (, int256 totalReceiverFlowRate) = _updateAccountStateWithData(token, receiver, outFlowId, false);

        assert(senderFlowRate < 0);

        // note : calculate the deposit here and pass it to superToken, and emit the events.
        // Close this Agreement Data
        if (liquidation) {
            ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernanceAddress());
            uint16 minDeposit = gov.getMinimalDeposit(token.getUnderlayingToken());
            uint16 liquidationPeriod = gov.getLiquidationPeriod(token.getUnderlayingToken());
            uint256 deposit = Math.max(
                uint256(minDeposit),
                uint256(-senderFlowRate) * uint256(liquidationPeriod));

            token.liquidateAgreement(msg.sender, outFlowId, sender, deposit);
        } else {
            token.terminateAgreement(outFlowId);
        }

        emit FlowUpdated(
            token,
            sender,
            receiver,
            0,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _updateAccountStateWithData(
        ISuperToken token,
        address account,
        bytes32 id,
        bool invertFlow
    )
        private
        returns(int256 flowRate, int256 totalFlowRate)
    {

        bytes memory state = token.getAgreementAccountState(address(this), account);
        bytes memory data = token.getAgreementData(address(this), id);
        require(data.length > 0, "Account data not found");
        (, flowRate) = _decodeFlow(data);

        if (invertFlow) {
            state = _composeState(state, _mirrorFlowRate(flowRate), block.timestamp);
        } else {
            state = _composeState(state, flowRate, block.timestamp);
        }

        token.updateAgreementAccountState(account, state);

        (, totalFlowRate) = _decodeFlow(state);
    }

    function _mirrorFlowRate(int256 flowRate) private pure returns(int256 mirrorFlowRate) {
        return -1 * flowRate;
    }

    /// @dev mirrorState reverts the flow rate maintains the same timestamp
    function _mirrorAgreementData(
        bytes memory state
    )
        private
        pure
        returns(bytes memory mirror)
    {
        (uint256 startDate, int256 flowRate) = _decodeFlow(state);
        return _encodeFlow(startDate, _mirrorFlowRate(flowRate));
    }

    function _hashAccounts(address sender, address receiver) private pure returns(bytes32, bytes32) {
        return (keccak256(abi.encodePacked(sender, receiver)), keccak256(abi.encodePacked(receiver, sender)));
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
        require(state.length == 64, "invalid state size must be 64");
        return abi.decode(state, (uint256, int256));
    }

    /// @dev Compose in one state the states passed as arguments.
    ///      Will add the two state and update the `timestamp` to block.timestamp
    /// @param currentData the user actual data of agreement
    /// @param additionalData new state to be addeed to previous state
    /// @return newState composed
    function _composeData
    (
        bytes memory currentData,
        bytes memory additionalData
    )
        private
        pure
        returns (bytes memory newState)
    {
        int256 cRate;
        if (currentState.length != 0) {
            (, cRate) = _decodeFlow(currentState);

        }

        (uint256 aTimestamp, int256 aRate) = _decodeFlow(additionalState);
        int256 newRate = aRate == 0 ? 0 : cRate.add(aRate);
        if (newRate == 0) {
            return "";
        }

        return _encodeFlow(aTimestamp, newRate);
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

        require(flowRate != 0, "Invalid FlowRate");
        (, int cRate) = _decodeFlow(currentState);
        cRate = cRate.add(flowRate);

        if (cRate == 0) {
            return "";
        }
        return _encodeFlow(timestamp, cRate);
    }
}
