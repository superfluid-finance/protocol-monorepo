pragma solidity 0.5.17;

import { SuperAgreementBase } from "./SuperAgreementBase.sol";

/**
 * @title Superfluid's flow agreement
 * @notice 
 * @author Superfluid
 */
contract FlowAgreement is SuperAgreementBase {

    enum FlowRateType {
        FLOW_PER_BLOCK,
        FLOW_PER_DAY,
        FLOW_PER_MONTH,
        FLOW_PER_YEAR
    }

    function balanceOf(bytes calldata state) external pure
        returns (uint256 amount) {
        return 0;
    }

    function composeState(
        bytes calldata currentState,
        bytes calldata additionalState) external pure
        returns (bytes memory newState) {
        (int256 cRate) = currentState.length >= 4 ?
            decodeFlow(currentState) : 0;
        (int256 aRate) = decodeFlow(additionalState);
        int256 newRate = cRate + aRate;
        newState = encodeFlow(newRate);
    }

    function createFlow(
        FlowAgreement.FlowRateType flowType,
        int256 flowRate) external pure
        returns (
            bytes memory senderNewFlow,
            bytes memory receiverNewFlow) {
        senderNewFlow = encodeFlow(flowRate);
        receiverNewFlow = encodeFlow(flowRate);
    }

    function encodeFlow(int256 flowRate) private pure
        returns (bytes memory) {
        return abi.encodePacked(flowRate);
    }

    function decodeFlow(bytes memory state) private pure
        returns (int256 flowRate) {
        flowRate = abi.decode(state, (int256));
    }

}
