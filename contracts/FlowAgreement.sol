pragma solidity 0.6.6;

import "./interface/ISuperToken.sol";
import "./interface/ISuperAgreement.sol";

contract FlowAgreement is ISuperAgreement {

    function balanceOf(
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

        (_startDate, _flowRate) = decodeFlow(data);
        return int256(time - _startDate) * _flowRate;
    }


    /*
     *   Flow Functions
     */

    function getFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        view
        returns(bytes memory state)
    {
        return token.getAgreementData(address(this), _hashAccounts(sender, receiver));
    }

    function createFlow(
        ISuperToken token,
        address account,
        int256 flowRate
    )
        external
    {
        require(flowRate > 0, "Invalid FlowRate");
        updateFlow(token, account, flowRate);
    }

    function updateFlow(
        ISuperToken token,
        address account,
        int256 flowRate
    )
        public
    {
        require(flowRate != 0, "Invalid FlowRate, use deleteFlow function");
        bytes memory _data = encodeFlow(block.timestamp, flowRate);

        _updateAgreementData(token, msg.sender, account, _data);
        _updateAccountState(token, msg.sender, account, flowRate);
    }

    function deleteFlow(
        ISuperToken token,
        address account
    )
        external
    {
        _terminateAgreementData(token, msg.sender, account);
    }


   

    /*
     *  Helpers
     */

    /// @notice touch the timestamp of the current aggreement, update only the `timestamp`
    /// @param currentData Account data to be updated
    /// @return newData updated
    function touch(
        bytes memory currentData,
        uint256 timestamp
    )
        public
        pure
        override
        returns(bytes memory newData)
    {
        (, int256 _cRate) = decodeFlow(currentData);
        return encodeFlow(timestamp, _cRate);
    }



    /*
     * Internal Functions
     */

    function _updateAgreementData(
        ISuperToken token,
        address accountA,
        address accountB,
        bytes memory additionalData
    )
        internal
    {
        bytes32 _ab = _hashAccounts(accountA, accountB);
        bytes32 _ba = _hashAccounts(accountB, accountA);
        bytes memory _currentSenderAgreementData = token.getAgreementData(address(this), _ab);
        bytes memory _currentReceiverAgreementData = token.getAgreementData(address(this), _ba);

        bytes memory _senderData = composeData(_currentSenderAgreementData, mirrorAgreementData(additionalData));
        bytes memory _receiverData = composeData(_currentReceiverAgreementData, additionalData);

        token.createAgreement(address(this), _ab, _senderData);
        token.createAgreement(address(this), _ba, _receiverData);
    }

    function _updateAccountState(
        ISuperToken token,
        address accountA,
        address accountB,
        int256 flowRate
    )
        internal
    {

        int256 _invFlowRate = mirrorFlowRate(flowRate);

        bytes memory _senderAccountState = token.getAgreementAccountState(accountA);
        bytes memory _receiverAccountState = token.getAgreementAccountState(accountB);
        bytes memory _senderNewAccountState;
        bytes memory _receiverNewAAccountState;

        if (_senderAccountState.length != 0) {
            _senderNewAccountState = composeAgreementState(_senderAccountState, _invFlowRate, block.timestamp);
        } else {
            _senderNewAccountState = encodeAgreementState(block.timestamp, flowRate, 0, 1);
        }

        if(_receiverAccountState.length != 0) {
           _receiverNewAAccountState = composeAgreementState(_receiverAccountState, flowRate, block.timestamp);
        } else {
            _receiverNewAAccountState = encodeAgreementState(block.timestamp, flowRate, 1, 0);
        }

        token.updateAgreementAccountState(accountA, _senderNewAccountState);
        token.updateAgreementAccountState(accountB, _receiverNewAAccountState);
    }


    function _terminateAgreementData(
        ISuperToken token,
        address accountA,
        address accountB
    )
        internal
    {

        bytes32 _ab = _hashAccounts(accountA, accountB);
        bytes32 _ba = _hashAccounts(accountB, accountA);
        bytes memory _currentSenderState = token.getAgreementAccountState(accountA);
        bytes memory _currentReceiverState = token.getAgreementAccountState(accountB);


        if (_currentSenderState.length != 0) {
            (,,uint128 _ins, uint128 _outs) = decodeAgreementState(_currentSenderState);

            if(_ins == 0 && _outs - 1 == 0) {
                token.updateAgreementAccountState(accountA, "");
            } else {
                //We are still running something
                bytes memory _currentSenderAgreementData = token.getAgreementData(address(this), _ab);
                //notice that we don't inverse the data
                bytes memory _senderData = composeData(_currentSenderState, _currentSenderAgreementData);
                token.createAgreement(address(this), _ab, _senderData);
            }
        }

        if(_currentReceiverState.length != 0) {
            (,,uint128 _ins, uint128 _outs) = decodeAgreementState(_currentReceiverState);

            if(_ins - 1 == 0 && _outs == 0) {
                token.updateAgreementAccountState(accountB, "");
            } else {
                //We are still running something
                bytes memory _currentReceiverAgreementData = token.getAgreementData(address(this), _ba);
                bytes memory _receiverData = composeData(_currentReceiverState, _currentReceiverAgreementData);
                token.createAgreement(address(this), _ab, _receiverData);
            }
        }

        //Close this Agreement Data
        token.terminateAgreement(address(this), _ab);
        token.terminateAgreement(address(this), _ba);
    }

    function mirrorFlowRate(int256 flowRate) internal pure returns(int256) {
        return -1 * flowRate;
    }

    /// @dev mirrorState reverts the flow rate maintains the same timestamp
    function mirrorAgreementData(
        bytes memory state
    )
        internal
        pure
        returns(bytes memory mirror)
    {
        (uint256 _startDate, int256 _flowRate) = decodeFlow(state);
        return encodeFlow(_startDate, (-1 * _flowRate));
    }

     function _hashAccounts(address accountA, address accountB) internal pure returns(bytes32) {
        return keccak256(abi.encodePacked(accountA, accountB));
    }

    //Encoders & Decoders
    /// @dev Encode the parameters into a bytes type
    function encodeFlow
    (
        uint256 timestamp,
        int256 flowRate
    )
        public
        pure
        override
        returns (bytes memory)
    {
        return abi.encodePacked(timestamp, flowRate);
    }

    /// @dev Decode the parameter into the original types
    function decodeFlow
    (
        bytes memory state
    )
        public
        pure
        override
        returns
    (
        uint256 timestamp,
        int256 flowRate
    )
    {
        require(state.length == 64, "invalid state size");
        return abi.decode(state, (uint256, int256));
    }

    /// @dev Encode the parameters into a state
    function encodeAgreementState
    (
        uint256 timestamp,
        int256 flowRate,
        uint128 ins,
        uint128 outs
    )
        public
        pure
        returns (bytes memory)
    {
        return abi.encodePacked(timestamp, flowRate, ins, outs);
    }

    /// @dev Decode the state into the original types
    function decodeAgreementState
    (
        bytes memory state
    )
        public
        pure
        returns
    (
        uint256 timestamp,
        int256 flowRate,
        uint128 ins,
        uint128 outs
    )
    {
        require(state.length == 96, "invalid agreement size");
        return abi.decode(state, (uint256, int256, uint128, uint128));
    }

    /// @notice Compose in one state the states passed as arguments.
    /// @dev Will add the two state and update the `timestamp` to block.timestamp
    /// @param currentState the user actual state of agreement
    /// @param additionalState new state to be addeed to previous state
    /// @return newState composed
    function composeData
    (
        bytes memory currentState,
        bytes memory additionalState
    )
        internal
        pure
        returns (bytes memory newState)
    {
        int256 _cRate;
        if (currentState.length != 0) {
            (, _cRate) = decodeFlow(currentState);

        }

        (uint256 _aTimestamp, int256 _aRate) = decodeFlow(additionalState);
        int256 _newRate = _aRate == 0 ? 0 : (_cRate + _aRate);
        return encodeFlow(_aTimestamp, _newRate);
    }

    /// @notice Compose in one state the states passed as arguments.
    /// @dev Will add the two state and update the `timestamp` to block.timestamp
    /// @param currentState Data of the actual agreement
    /// @param flowRate New value to update
    /// @param timestamp New time to update
    /// @return newAgreement New agreement data
    function composeAgreementState
    (
        bytes memory currentState,
        int256 flowRate,
        uint256 timestamp
    )
        internal
        pure
        returns (bytes memory newAgreement)
    {

        require(flowRate != 0, "Invalid FlowRate");
        int256 _cRate;
        uint128 _ins;
        uint128 _outs;

        if (currentState.length != 0) {
            (, _cRate, _ins, _outs) = decodeAgreementState(currentState);
        }

        //If is a sender then we have a negative flow
        if(flowRate < 0) {
            _outs += 1;
        } else {
            _ins += 1;
        }

        return encodeAgreementState(timestamp, (_cRate + flowRate), _ins, _outs);
    }
}
