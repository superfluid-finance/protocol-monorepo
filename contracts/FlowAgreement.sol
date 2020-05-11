/* solhint-disable not-rely-on-time */
pragma solidity 0.6.6;

import { SuperAgreementBase } from "./SuperAgreementBase.sol";
import "./interface/ISuperToken.sol";

/**
 * @title Superfluid's flow agreement
 * @notice A realtime finance primitive that facilitate payments in streams.
 * @author Superfluid
 */
contract FlowAgreement is SuperAgreementBase {

    /// @notice Calculate the moving balance. This method don't calculate the real balance
    /// @dev Calculate balance based on the state and time
    /// @param state Bytes that save the agreement state
    /// @param time Time used to calculate balance. Normally is the block timestamp
    /// @return amount of the moving balance
    function balanceOf
    (
        bytes calldata state,
        uint256 time
    )
        external
        pure
        override
        returns (int256 amount)
    {
        uint256 _startDate;
        int256 _flowRate;

        (_startDate, _flowRate) = decodeFlow(state);
        return int256(time - _startDate) * _flowRate;
    }

    /// @notice Create a new flow between two users
    /// @dev This function will make a external call to register the agreement
    /// @param token Contract of the the SuperToken that will manage the agreement
    /// @param receiver Who is receiving SuperToken
    /// @param flowRate What is the periodicity of payments
    function createFlow
    (
        ISuperToken token,
        address receiver,
        int256 flowRate
    )
        external
    {
        updateFlow(token, receiver, flowRate);
    }

    /// @notice Update an flow between two users
    /// @dev This function will make a external call to register the agreement
    /// @param token Contract of the the SuperToken that will manage the agreement
    /// @param receiver Who is receiving SuperToken
    /// @param flowRate What is the updated periodicity of payments
    function updateFlow
    (
        ISuperToken token,
        address receiver,
        int256 flowRate
    )
        public
    {
        bytes memory _newState = encodeFlow(block.timestamp, flowRate);
        bool termination = flowRate == 0;
        updateState(token, msg.sender, receiver, termination, _newState);
    }

    /// @notice Delete an flow between two users
    /// @dev This function will make a external call to delete the agreement
    /// @param token Contract of the the SuperToken that will manage the agreement
    /// @param receiver Who is receiving SuperToken
    function deleteFlow(
        ISuperToken token,
        address receiver
    )
        external
    {
        updateFlow(token, receiver, 0);
    }

    /// @notice Gets the flow between two users
    /// @dev This function will make a external call to get the agreement data
    /// @param token Contract of the the SuperToken that will manage the agreement
    /// @param receiver Who is receiving SuperToken
    function getFlowRate(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        view
        returns(int256 flowRate)
    {
        (, int256 _flowRate) = decodeFlow(token.currentState(address(this), sender, receiver));
        return _flowRate;
    }

    /// @notice Gets the total in flow rate to the user
    /// @dev This function will make a external call to get the agreement data
    /// @param account to query
    function getTotalInFlowRate(
        ISuperToken token,
        address account
    )
        external
        view
        returns (int256 flowRate)
    {
        bytes memory data = token.getAgreementState(address(this), account);(
        (uint256, int256 flowRate) = decodeFlow(data);
    }

    /// @notice Gets the total out flow rate to the user
    /// @dev This function will make a external call to get the agreement data
    /// @param account to query
    /// @return total of out flow rate
    function getTotalOutFlowRate(
        ISuperToken token,
        address account
    )
        external
        view
        returns(int256)
    {
        //should check if token is approved
        (, int256 debitor) = token.getAccountRateFlows(account);
        return debitor;
    }

    /// @notice Compose in one state the states passed as arguments.
    /// @dev Will add the two state and update the `timestamp` to block.timestamp
    /// @param currentState the user actual state of agreement
    /// @param additionalState new state to be addeed to previous state
    /// @return newState composed
    function composeState
    (
        bytes memory currentState,
        bytes memory additionalState
    )
        internal
        pure
        override
        returns (bytes memory newState)
    {
        int256 _cRate;
        //We are not updating a new flow
        if (currentState.length != 0) {
            (, _cRate) = decodeFlow(currentState);

        }

        (uint256 _aTimestamp, int256 _aRate) = decodeFlow(additionalState);
        int256 _newRate = _aRate == 0 ? 0 : (_cRate + _aRate);
        return encodeFlow(_aTimestamp, _newRate);
    }

    /// @notice touch the timestamp of the current aggreement, update only the `timestamp`
    /// @param currentState is the user current state to be updated
    /// @return newState updated
    function touch(bytes memory currentState, uint256 timestamp) public pure override returns(bytes memory newState) {
        (, int256 _cRate) = decodeFlow(currentState);
        return encodeFlow(timestamp, _cRate);
    }

    /// @dev mirrorState reverts the flow rate maintains the same timestamp
    function mirrorState(
        bytes memory state
    )
        internal
        pure
        override
        returns(bytes memory mirror)
    {
        (uint256 _startDate, int256 _flowRate) = decodeFlow(state);
        return encodeFlow(_startDate, (-1 * _flowRate));
    }

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
}
