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
        FlowAgreement.FlowRateType flowType,
        int256 flowRate) external {
        (bytes memory senderNewFlow, bytes memory receiverNewFlow) =
            flow.createFlow(flowType, flowRate);
        flow.updateState(token, sender, senderNewFlow);
        flow.updateState(token, receiver, receiverNewFlow);
    }

}
