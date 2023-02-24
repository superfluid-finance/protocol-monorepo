// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import { SuperAppBaseFlowMock, ISuperfluid, ISuperToken, SuperAppDefinitions } from "./SuperAppBaseFlowMock.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";

contract SuperAppBaseFlowTester is SuperAppBaseFlowMock {
    using SuperTokenV1Library for ISuperToken;

    uint256 public lastUpdateHolder;

    int96 public oldFlowRateHolder;

    address public afterSenderHolder;
    address public beforeSenderHolder;

    address public afterReceiverHolder;
    address public beforeReceiverHolder;

    constructor(
        ISuperfluid host
    ) SuperAppBaseFlowMock (
        host,
        true,
        true,
        true
    ) {
        lastUpdateHolder = 0; // appeasing linter
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
        bytes calldata callBackData,
        bytes calldata ctx
    ) internal override returns(bytes memory) {
        (uint256 lastUpdate, int96 flowRate) = abi.decode(callBackData, (uint256,int96));
        
        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
        return ctx;
    }

    // UPDATE

    function afterFlowUpdated(
        ISuperToken /*superToken*/,
        address sender,
        bytes calldata callBackData,
        bytes calldata ctx
    ) internal override returns(bytes memory) {
        (uint256 lastUpdate, int96 flowRate) = abi.decode(callBackData, (uint256,int96));
        
        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
        return ctx;
    }

    // DELETE

    function afterFlowDeleted(
        ISuperToken /*superToken*/,
        address sender,
        address receiver,
        bytes calldata callBackData,
        bytes calldata ctx
    ) internal override returns (bytes memory newCtx) {
        (uint256 lastUpdate, int96 flowRate) = abi.decode(callBackData, (uint256,int96));

        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
        afterReceiverHolder = receiver;
        return ctx;
    }

}