// SPDX-License-Identifier: MIT
pragma solidity ^0.6.6;

import "../FlowAgreement.sol";
import "../interface/ISuperToken.sol";

/**
 * @title Superfluid's agreement interface
 * @notice
 * @author Superfluid
 */
contract FlowPayment {

    FlowAgreement private _flow;

    constructor(FlowAgreement flow) public {
        _flow = flow;
    }

    function connect(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        external
    {
        _flow.updateFlow(token, sender, receiver, flowRate);
    }

}
