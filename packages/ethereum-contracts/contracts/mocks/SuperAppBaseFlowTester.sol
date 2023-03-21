// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

// import { SuperAppBaseFlowMock, ISuperfluid, ISuperToken, SuperAppDefinitions } from "./SuperAppBaseFlowMock.sol";
import { SuperAppBaseFlow, ISuperToken, ISuperfluid } from "../apps/SuperAppBaseFlow.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";

contract SuperAppBaseFlowTester is SuperAppBaseFlow {
    using SuperTokenV1Library for ISuperToken;


    int96 public oldFlowRateHolder;
    address public afterSenderHolder;
    address public afterReceiverHolder;

    constructor(
        ISuperfluid host
    ) SuperAppBaseFlow (
        host,
        true,
        true,
        true
    ) {
        oldFlowRateHolder = 0; // appeasing linter
    }

    // SETTING ACCEPTED SUPER TOKENS

    function setAcceptedSuperToken(ISuperToken acceptedSuperToken, bool accepted) public {
        _acceptedSuperTokens[acceptedSuperToken] = accepted;
    }

    // ARBITRARY START STREAM

    function startStream(ISuperToken superToken, address receiver, int96 flowRate) public {
        superToken.createFlow(receiver, flowRate);
    }

    // CREATE

    function afterFlowCreated(
        ISuperToken /*superToken*/,
        address sender,
        bytes calldata ctx
    ) internal override returns(bytes memory) {
        
        afterSenderHolder = sender;
        return ctx;
        
    }

    // UPDATE

    function afterFlowUpdated(
        ISuperToken /*superToken*/,
        address sender,
        int96 oldFlowRate,
        bytes calldata ctx
    ) internal override returns(bytes memory) {
        
        oldFlowRateHolder = oldFlowRate;
        afterSenderHolder = sender;
        return ctx;

    }

    // DELETE

    function afterFlowDeleted(
        ISuperToken /*superToken*/,
        address sender,
        address receiver,
        int96 oldFlowRate,
        bytes calldata ctx
    ) internal override returns (bytes memory newCtx) {

        oldFlowRateHolder = oldFlowRate;
        afterSenderHolder = sender;
        afterReceiverHolder = receiver;
        return ctx;

    }

}