pragma solidity 0.6.6;

import "../FlowAgreement.sol";
import "../interface/ISuperToken.sol";

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
        int256 flowRate) external {
        flow.createFlow(token, sender, receiver, flowRate);
    }

}
