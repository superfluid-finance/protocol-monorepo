pragma solidity 0.5.17;

import "../FlowAgreement.sol";
import "../ISuperToken.sol";

/**
 * @title Superfluid's agreement interface
 * @notice 
 * @author Superfluid
 */
contract FlowPayment {

    FlowAgreement private flow;

    constructor(FlowAgreement flow_) public {
        flow = flow_;
    }

    function connect(
        ISuperToken token,
        address sender,
        address receiver,
        uint8 flowType,
        FlowAgreement.FlowRateType flowRate) external {
        bytes memory senderState = token.getState(sender);
        bytes memory receiverState = token.getState(receiver);
        bytes memory senderNewFLow = new bytes(4);
        bytes memory receiverNewFLow = new bytes(4);
        bytes memory senderNewState = flow.composeState(senderNewFLow, senderState);
        bytes memory receiverNewState = flow.composeState(senderNewState, senderState);
        token.updateState(sender, senderNewState);
        token.updateState(receiver, receiverNewState);
    }

}
