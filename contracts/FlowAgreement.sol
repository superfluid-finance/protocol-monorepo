/* solhint-disable not-rely-on-time */
pragma solidity 0.6.6;

import { ISuperToken, IFlowAgreement } from "./interface/IFlowAgreement.sol";

contract FlowAgreement is IFlowAgreement {

    /*
     * ISuperAgreement interface
     */
    /// @dev IsuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        bytes calldata data,
        uint256 time
    )
        external
        pure
        override
        returns (int256 balance)
    {
        uint256 _startDate;
        int256 _flowRate;

        (_startDate, _flowRate) = _decodeFlow(data);
        return int256(time - _startDate) * _flowRate;
    }

    /// @dev IsuperAgreement.touch implementation
    function touch(
        bytes memory currentData,
        uint256 timestamp
    )
        public
        pure
        override
        returns(bytes memory newData)
    {
        (, int256 _cRate) = _decodeFlow(currentData);
        return _encodeFlow(timestamp, _cRate);
    }

    /*
     *   FlowAgreement interface
     */

    function createFlow(
        ISuperToken token,
        address receiver,
        int256 flowRate
    )
        external
        override
    {
        //TODO FIX: direction of flow
        _updateFlow(token, msg.sender, receiver, flowRate);
    }

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
        (bytes32 _outFlowId, ) = _hashAccounts(sender, receiver);
        bytes memory data = token.getAgreementData(address(this), _outFlowId);
        (, flowRate) = _decodeFlow(data);
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
        (, flowRate) = _decodeFlow(state);
    }

    function updateFlow(
        ISuperToken token,
        address receiver,
        int256 flowRate
    )
        external
        override
    {
        _updateFlow(token, msg.sender, receiver, flowRate);
    }

    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        override
    {
        require(msg.sender == sender || msg.sender == receiver, "Not the sender or receiver");
        _terminateAgreementData(token, sender, receiver);
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
        bytes memory _data = _encodeFlow(block.timestamp, flowRate);

        _updateAgreementData(token, sender, receiver, _data);
        _updateAccountState(token, sender, receiver, flowRate);
    }

    function _updateAgreementData(
        ISuperToken token,
        address accountA,
        address accountB,
        bytes memory additionalData
    )
        private
    {
        (bytes32 _outFlowId, bytes32 _inFlowId) = _hashAccounts(accountA, accountB);
        bytes memory _senderData = token.getAgreementData(address(this), _outFlowId);
        bytes memory _receiverData = token.getAgreementData(address(this), _inFlowId);

        _senderData = _composeData(_senderData, _mirrorAgreementData(additionalData));
        _receiverData = _composeData(_receiverData, additionalData);

        token.createAgreement(_outFlowId, _senderData);
        token.createAgreement(_inFlowId, _receiverData);
    }

    function _updateAccountState(
        ISuperToken token,
        address accountA,
        address accountB,
        int256 flowRate
    )
        private
    {

        int256 _invFlowRate = _mirrorFlowRate(flowRate);
        require(_invFlowRate <= 0, "Flipping Flow");
        bytes memory _senderState = token.getAgreementAccountState(address(this), accountA);
        bytes memory _receiverState = token.getAgreementAccountState(address(this), accountB);

        _senderState = _composeState(_senderState, _invFlowRate, block.timestamp);
        _receiverState = _composeState(_receiverState, flowRate, block.timestamp);

        token.updateAgreementAccountState(accountA, _senderState);
        token.updateAgreementAccountState(accountB, _receiverState);

        (, int totalSenderFlowRate) = _decodeFlow(_senderState);
        (, int totalReceiverFlowRate) = _decodeFlow(_receiverState);
        emit FlowUpdated(
            token,
            accountA,
            accountB,
            flowRate,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _terminateAgreementData(
        ISuperToken token,
        address accountA,
        address accountB
    )
        private
    {
        (bytes32 _outFlowId, bytes32 _inFlowId) = _hashAccounts(accountA, accountB);

        bytes memory _senderState = token.getAgreementAccountState(address(this), accountA);
        bytes memory _receiverState = token.getAgreementAccountState(address(this), accountB);
        bytes memory _senderData = token.getAgreementData(address(this), _outFlowId);
        bytes memory _receiverData = token.getAgreementData(address(this), _inFlowId);


        (, int256 _senderFlowRate) = _decodeFlow(_senderData);
        _senderState = _composeState(_senderState, _mirrorFlowRate(_senderFlowRate), block.timestamp);
        token.updateAgreementAccountState(accountA, _senderState);

        (, int256 _receiverFlowRate) = _decodeFlow(_receiverData);
        _receiverState = _composeState(_receiverState, _mirrorFlowRate(_receiverFlowRate), block.timestamp);
        token.updateAgreementAccountState(accountB, _receiverState);


        //Close this Agreement Data
        token.terminateAgreement(_outFlowId);
        token.terminateAgreement(_inFlowId);

        (, int256 totalSenderFlowRate) = _decodeFlow(_senderState);
        (, int256 totalReceiverFlowRate) = _decodeFlow(_senderState);
        emit FlowUpdated(
            token,
            accountA,
            accountB,
            0,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _mirrorFlowRate(int256 flowRate) private pure returns(int256) {
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
        (uint256 _startDate, int256 _flowRate) = _decodeFlow(state);
        //TODO fix this
        require(_flowRate != 0, "is zero");

        return _encodeFlow(_startDate, (-1 * _flowRate));
    }

    function _hashAccounts(address accountA, address accountB) private pure returns(bytes32, bytes32) {
        return (keccak256(abi.encodePacked(accountA, accountB)), keccak256(abi.encodePacked(accountB, accountA)));
    }

    //Encoders & Decoders
    /// @dev Encode the parameters into a bytes type
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

    /// @notice Compose in one state the states passed as arguments.
    /// @dev Will add the two state and update the `timestamp` to block.timestamp
    /// @param currentState the user actual state of agreement
    /// @param additionalState new state to be addeed to previous state
    /// @return newState composed
    function _composeData
    (
        bytes memory currentState,
        bytes memory additionalState
    )
        private
        pure
        returns (bytes memory newState)
    {
        int256 _cRate;
        if (currentState.length != 0) {
            (, _cRate) = _decodeFlow(currentState);

        }

        (uint256 _aTimestamp, int256 _aRate) = _decodeFlow(additionalState);
        int256 _newRate = _aRate == 0 ? 0 : (_cRate + _aRate);
        if (_newRate == 0) {
            return "";
        }

        return _encodeFlow(_aTimestamp, _newRate);
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
        (, int _cRate) = _decodeFlow(currentState);
        _cRate += flowRate;

        if (_cRate == 0) {
            return "";
        }
        return _encodeFlow(timestamp, _cRate);
    }

}
