// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.23;

import { ISuperfluid, ISuperToken } from "../interfaces/superfluid/ISuperfluid.sol";
import { CFASuperAppBase } from "../apps/CFASuperAppBase.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";

using SuperTokenV1Library for ISuperToken;

/// @title CrossStreamSuperApp
/// @author Superfluid
/// @dev A super app used for testing "cross-stream" flows in callbacks
/// and its behavior surrounding the internal protocol accounting.
/// That is, two senders sending a flow to the super app
contract CrossStreamSuperApp is CFASuperAppBase {
    address public flowRecipient;
    address public prevSender;
    int96 public prevFlowRate;

    constructor(ISuperfluid host_, address z_) CFASuperAppBase(host_) {
        _initialize(true, true, true, true);
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
