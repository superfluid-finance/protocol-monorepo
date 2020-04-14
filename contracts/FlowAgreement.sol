pragma solidity 0.6.6;

import { SuperAgreementBase } from "./SuperAgreementBase.sol";
import "./ISuperToken.sol";

/**
 * @title Superfluid's flow agreement
 * @notice A realtime finance primitive that facilitate payments in streams.
 * @author Superfluid
 */
contract FlowAgreement is SuperAgreementBase {

    enum FlowRateType {
        FLOW_PER_SECOND,
        FLOW_PER_MONTH
    }

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
        FlowRateType _type;
        uint256 _startDate;
        int256 _flowRate;
        (_type, _startDate, _flowRate) = decodeFlow(state);

        int256 _balance;

        if(_type == FlowRateType.FLOW_PER_SECOND) {
            _balance = int256(time - _startDate) * _flowRate;
        }
        // awaiting for others types of flows

        return _balance;
    }

    function createFlow
    (
        ISuperToken token,
        address sender,
        address receiver,
        FlowAgreement.FlowRateType flowType,
        int256 flowRate
    )
    external
    {
        bytes memory senderNewFlow = encodeFlow(flowType, block.timestamp, flowRate);
        bytes memory receiverNewFlow = encodeFlow(flowType, block.timestamp, -flowRate);

        token.updateState(sender, senderNewFlow);
        token.updateState(receiver, receiverNewFlow);
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
        //(,,int256 cRate) = currentState.length >= 4 ? decodeFlow(currentState) : 0;
        //(,,int256 aRate) = decodeFlow(additionalState);
        //int256 newRate = cRate + aRate;
        //newState = encodeFlow(newRate);
    }

    function encodeFlow
    (
        FlowRateType flowtype,
        uint256 timestamp,
        int256 flowRate
    )
    internal
    pure
    returns (bytes memory)
    {
        return abi.encodePacked(flowtype, timestamp, flowRate);
    }

    function decodeFlow
    (
        bytes memory state
    )
    internal
    pure
    returns
    (
        FlowRateType,
        uint256,
        int256
    )
    {
        return abi.decode(state, (FlowRateType, uint256, int256));
    }
}
