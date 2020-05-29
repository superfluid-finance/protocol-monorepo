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

        (_startDate, _flowRate, ,) = _decodeState(data);
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
        //(, int256 _cRate) = decodeFlow(currentData);
        //return encodeFlow(timestamp, _cRate);

        (, int256 _cRate, uint256 _ins, uint256 _outs) = _decodeState(currentData);
        return _encodeState(timestamp, _cRate, _ins, _outs);
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
        bytes memory data = token.getAgreementData(address(this), _hashAccounts(sender, receiver));
        (, flowRate) = _decodeFlow(data);
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
        address receiver
    )
        external
        override
    {
        _terminateAgreementData(token, msg.sender, receiver);
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
         //TODO FIX THIS
         require(_data.length == 64, "Encoded data wrong");

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
        bytes32 _ab = _hashAccounts(accountA, accountB);
        bytes32 _ba = _hashAccounts(accountB, accountA);
        bytes memory _currentSenderAgreementData = token.getAgreementData(address(this), _ab);
        bytes memory _currentReceiverAgreementData = token.getAgreementData(address(this), _ba);

        bytes memory _senderData = _composeData(_currentSenderAgreementData, _mirrorAgreementData(additionalData));
        bytes memory _receiverData = _composeData(_currentReceiverAgreementData, additionalData);

        token.createAgreement(_ab, _senderData);
        token.createAgreement(_ba, _receiverData);
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

        bytes memory _senderAccountState = token.getAgreementAccountState(address(this), accountA);
        bytes memory _receiverAccountState = token.getAgreementAccountState(address(this), accountB);

        bytes memory _senderNewAccountState;
        bytes memory _receiverNewAccountState;

        if (_senderAccountState.length > 0) {

            bool _changeCounters = (token.getAgreementData(
                address(this), _hashAccounts(accountA, accountB))
            ).length == 0;
            _senderNewAccountState = _composeState(_senderAccountState, _invFlowRate, block.timestamp, _changeCounters);

        } else {
            _senderNewAccountState = _encodeState(block.timestamp, _invFlowRate, 0, 1);
        }

        if (_receiverAccountState.length > 0) {
            bool _changeCounters = (token.getAgreementData(
                address(this), _hashAccounts(accountB, accountA))
            ).length == 0;
            _receiverNewAccountState = _composeState(_receiverAccountState, flowRate, block.timestamp, _changeCounters);
        } else {
            _receiverNewAccountState = _encodeState(block.timestamp, flowRate, 1, 0);
        }

        token.updateAgreementAccountState(accountA, _senderNewAccountState);
        token.updateAgreementAccountState(accountB, _receiverNewAccountState);

        (,int totalSenderFlowRate,,) = _decodeState(_senderNewAccountState);
        (,int totalReceiverFlowRate,,) = _decodeState(_receiverNewAccountState);
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

        //TODO RENAME _ab = outFlowId, ba = inFlowId
        bytes32 _ab = _hashAccounts(accountA, accountB);
        bytes32 _ba = _hashAccounts(accountB, accountA);

        bytes memory _currentSenderState = token.getAgreementAccountState(address(this), accountA);
        bytes memory _currentReceiverState = token.getAgreementAccountState(address(this), accountB);

        bytes memory _senderNewAccountState;
        bytes memory _receiverNewAccountState;

        if (_currentSenderState.length != 0) {

            (, int256 _stateFlowRate, uint256 _ins, uint256 _outs) = _decodeState(_currentSenderState);

            if (_ins == 0 && _outs - 1 == 0) {
                //last one, just closed it
                token.updateAgreementAccountState(accountA, "");
            } else {
                //We are still running something. Get the agreement value discount it from the state and save it
                bytes memory _userAgreement = token.getAgreementData(address(this), _ab);
                (, int256 _flowRate) = _decodeFlow(_userAgreement);

                int256 finalFlow = _flowRate - _stateFlowRate;
                _senderNewAccountState = _encodeState(block.timestamp, finalFlow, _ins, _outs - 1);
                token.updateAgreementAccountState(accountA, _senderNewAccountState);
            }
        }

        if (_currentReceiverState.length != 0) {

            (, int256 _stateFlowRate, uint256 _ins, uint256 _outs) = _decodeState(_currentReceiverState);

            if (_ins - 1 == 0 && _outs == 0) {

                //last one, just closed it
                token.updateAgreementAccountState(accountB, "");

            } else {
                //We are still running something. Get the agreement value discount it from the state and save it
                bytes memory _userAgreement = token.getAgreementData(address(this), _ba);
                (, int256 _flowRate) = _decodeFlow(_userAgreement);
                int256 finalFlow = _flowRate - _stateFlowRate;
                _receiverNewAccountState = _encodeState(block.timestamp, finalFlow, _ins - 1, _outs);
                token.updateAgreementAccountState(accountA, _receiverNewAccountState);
            }
        }

        //Close this Agreement Data
        token.terminateAgreement(_ab);
        token.terminateAgreement(_ba);

        (,int totalSenderFlowRate,,) = _decodeState(_senderNewAccountState);
        (,int totalReceiverFlowRate,,) = _decodeState(_receiverNewAccountState);
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

    function _hashAccounts(address accountA, address accountB) private pure returns(bytes32) {
        return keccak256(abi.encodePacked(accountA, accountB));
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
        require(state.length == 64, "invalid state size must be 64");
        return abi.decode(state, (uint256, int256));
    }

    /// @dev Encode the parameters into a state
    function _encodeState
    (
        uint256 timestamp,
        int256 flowRate,
        uint256 ins,
        uint256 outs
    )
        private
        pure
        returns (bytes memory)
    {
        return abi.encodePacked(timestamp, flowRate, ins, outs);
    }

    /// @dev Decode the state into the original types
    function _decodeState
    (
        bytes memory state
    )
        private
        pure
        returns
    (
        uint256 timestamp,
        int256 flowRate,
        uint256 ins,
        uint256 outs
    )
    {
        if (state.length == 0) return (0, 0, 0, 0);
        require(state.length == 128, "invalid agreement size must be 128");
        return abi.decode(state, (uint256, int256, uint256, uint256));
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
        return _encodeFlow(_aTimestamp, _newRate);
    }

    /// @notice Compose in one state the states passed as arguments.
    /// @dev Will add the two state and update the `timestamp` to block.timestamp
    /// @param currentState Data of the actual agreement
    /// @param flowRate New value to update
    /// @param timestamp New time to update
    /// @return newAgreement New agreement data
    function _composeState
    (
        bytes memory currentState,
        int256 flowRate,
        uint256 timestamp,
        bool updCounter

    )
        private
        pure
        returns (bytes memory newAgreement)
    {

        require(flowRate != 0, "Invalid FlowRate");
        int256 _cRate;
        uint256 _ins;
        uint256 _outs;

        if (currentState.length != 0) {
            (, _cRate, _ins, _outs) = _decodeState(currentState);
        }


        //If is a sender then we have a negative flow
        if (updCounter) {
            flowRate < 0 ? _outs += 1 : _ins += 1;
        }

        return _encodeState(timestamp, (_cRate + flowRate), _ins, _outs);
    }
}
