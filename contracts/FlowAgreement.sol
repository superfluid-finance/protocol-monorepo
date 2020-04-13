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

    function balanceOf(bytes calldata state, uint256 time)
        external pure override
        returns (uint256 amount) {
        return 0;
    }

    function createFlow(
        ISuperToken token,
        address sender,
        address receiver,
        FlowAgreement.FlowRateType flowType,
        int256 flowRate) external {
        bytes memory senderNewFlow = encodeFlow(flowRate);
        bytes memory receiverNewFlow = encodeFlow(-flowRate);
        updateState(token, sender, senderNewFlow);
        updateState(token, receiver, receiverNewFlow);
    }

    function composeState(
        bytes memory currentState,
        bytes memory additionalState)
        internal pure override
        returns (bytes memory newState) {
        (int256 cRate) = currentState.length >= 4 ?
            decodeFlow(currentState) : 0;
        (int256 aRate) = decodeFlow(additionalState);
        int256 newRate = cRate + aRate;
        newState = encodeFlow(newRate);
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
