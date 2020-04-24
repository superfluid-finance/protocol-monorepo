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
        return int256(time - _startDaate) * _flowRate;
    }

    /// @notice Create a new flow between two users
    /// @dev This function will make a external call to register the agreement
    /// @param token Contract of the the SuperToken that will manage the agreement
    /// @param sender Who is sending SuperToken
    /// @param receiver Who is receiving SuperToken
    /// @param flowRate What is the periodicity of payments
    function createFlow
    (
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        external
    {
        bytes memory _newReceiverState = encodeFlow(block.timestamp, int256(flowRate));
        //Atention: External call
        token.updateState(sender, receiver, mirrorState(_newReceiverState), _newReceiverState);
    }

    function updateFlow
    (
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        public
    {
        bytes memory _newState = encodeFlow(block.timestamp, flowRate);
        updateState(token, sender, receiver, _newState);
    }

    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
    {
        updateFlow(token, sender, receiver, 0);
    }

    function getFlowRate(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        view
        returns(int256 flowRate)
    {
        (, int256 _flowRate) = decodeFlow(token.currentState(sender, receiver));
        return _flowRate;
    }

    function getTotalInFlowRate(
        ISuperToken token,
        address sender
    )
        external
        view
        returns(int256)
    {
        //should check if token is approved
        (int256 creditor, ) = token.getAccountRateFlows(sender);
        return creditor;
    }

    function getTotalOutFlowRate(
        ISuperToken token,
        address sender
    )
        external
        view
        returns(int256)
    {
        //should check if token is  approved
        (, int256 debitor) = token.getAccountRateFlows(sender);
        return debitor;
    }

    function updateAccount(
        bytes memory newState
    )
        public
        pure
        override
        returns(int256 flowRate)
    {

        int256 _flowRate;
        (, _flowRate) = decodeFlow(newState);

        return _flowRate;
    }

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
        (, int256 _cRate) = decodeFlow(currentState);
        (uint256 _aTimestamp, int256 _aRate) = decodeFlow(additionalState);

        int256 _newRate = _aRate == 0 ? 0 : (_cRate + _aRate);
        return encodeFlow(_aTimestamp, _newRate);
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
        internal
        pure
        returns (bytes memory)
    {
        return abi.encodePacked(timestamp, flowRate);
    }

    /// @dev Decode the parameter into the original types
    function decodeFlow
    (
        bytes memory state
    )
        internal
        pure
        returns
    (
        uint256,
        int256
    )
    {
        require(state.length == 64, "invalid state");
        return abi.decode(state, (uint256, int256));
    }
}
