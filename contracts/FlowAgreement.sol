/* solhint-disable not-rely-on-time */
pragma solidity 0.6.6;

import { SuperAgreementBase } from "./SuperAgreementBase.sol";
import "./ISuperToken.sol";

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
        uint256 _type;
        uint256 _startDate;
        int256 _flowRate;
        int256 _balance;

        (_type, _startDate, _flowRate) = decodeFlow(state);

        //if (_type == 0) {
        _balance = int256(time - _startDate) * _flowRate;
        //}
        // awaiting for others types of flows

        return _balance;
    }

    /// @notice Create a new flow between two users
    /// @dev This function will make a external call to register the agreement
    /// @param token Contract of the the SuperToken that will manage the agreement
    /// @param sender Who is sending SuperToken
    /// @param receiver Who is receiving SuperToken
    /// @param flowType What type of flow should this agreement implement
    /// @param flowRate What is the periodicity of payments
    function createFlow
    (
        ISuperToken token,
        address sender,
        address receiver,
        uint256 flowType,
        int256 flowRate
    )
    external
    {
        bytes memory senderNewFlow = encodeFlow(flowType, block.timestamp, -flowRate);
        bytes memory receiverNewFlow = encodeFlow(flowType, block.timestamp, flowRate);

        token.updateState(sender, senderNewFlow);
        token.updateState(receiver, receiverNewFlow);
    }

    function composeState
    (
        bytes memory,/* currentState */
        bytes memory additionalState
    )
    internal
    pure
    override
    returns (bytes memory newState)
    {
        return additionalState;
        //(,,int256 cRate) = currentState.length >= 4 ? decodeFlow(currentState) : 0;
        //(,,int256 aRate) = decodeFlow(additionalState);
        //int256 newRate = cRate + aRate;
        //newState = encodeFlow(newRate);
    }

    /// @dev Encode the parameters into a bytes type
    function encodeFlow
    (
        uint256 flowtype,
        uint256 timestamp,
        int256 flowRate
    )
    internal
    pure
    returns (bytes memory)
    {
        return abi.encodePacked(flowtype, timestamp, flowRate);
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
        uint256,
        int256
    )
    {
        return abi.decode(state, (uint256, uint256, int256));
    }
}
