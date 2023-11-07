// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { ISuperfluid, ISuperToken } from "../interfaces/superfluid/ISuperfluid.sol";
import { SuperAppBaseFlow } from "../apps/SuperAppBaseFlow.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";

using SuperTokenV1Library for ISuperToken;

/// @title CrossStreamSuperApp
/// @author Superfluid
/// @dev A super app used for testing "cross-stream" flows in callbacks
/// and its behavior surrounding the internal protocol accounting.
/// That is, two senders sending a flow to the super app
contract CrossStreamSuperApp is SuperAppBaseFlow {
    address public flowRecipient;
    address public prevSender;
    int96 public prevFlowRate;

    constructor(ISuperfluid host_, address z_) SuperAppBaseFlow(host_, true, true, true, "") {
        flowRecipient = z_;
    }

    function onFlowCreated(ISuperToken superToken, address sender, bytes calldata ctx)
        internal
        override
        returns (bytes memory newCtx)
    {
        newCtx = ctx;

        // get incoming stream
        int96 inFlowRate = superToken.getFlowRate(sender, address(this));

        if (prevSender == address(0)) {
            // first flow to super app creates a flow
            newCtx = superToken.createFlowWithCtx(flowRecipient, inFlowRate, newCtx);
        } else {
            // subsequent flows to super app updates and deletes the flow
            newCtx = superToken.updateFlowWithCtx(flowRecipient, inFlowRate, newCtx);
            newCtx = superToken.deleteFlowWithCtx(prevSender, address(this), newCtx);
        }

        prevSender = sender;
        prevFlowRate = inFlowRate;
    }
}
