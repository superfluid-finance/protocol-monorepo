// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import { SuperAppBaseFlowMock, ISuperfluid, ISuperToken, SuperAppDefinitions } from "./SuperAppBaseFlowMock.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";

error ErraticSender();
error ErraticReceiver();

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
    ) SuperAppBaseFlowMock(
        host,
        0
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

    function beforeFlowCreated(
        ISuperToken superToken,
        address sender,
        bytes calldata /*ctx*/
    ) internal view override returns(bytes memory) {
        (uint256 lastUpdate, int96 flowRate,,) = superToken.getFlowInfo(sender, address(this));
        return abi.encode(
            lastUpdate,
            flowRate,
            sender
        );
    }

    function afterFlowCreated(
        ISuperToken /*superToken*/,
        address sender,
        bytes calldata callBackData,
        bytes calldata ctx
    ) internal override returns(bytes memory) {

        (uint256 lastUpdate, int96 flowRate, address sender_) = abi.decode(callBackData, (uint256,int96,address));
        
        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
        beforeSenderHolder = sender_;
        return ctx;
    }

    // UPDATE

    function beforeFlowUpdated(
        ISuperToken superToken,
        address sender,
        bytes calldata /*ctx*/
    ) internal view override returns(bytes memory) {
        (uint256 lastUpdate, int96 flowRate,,) = superToken.getFlowInfo(sender, address(this));
        return abi.encode(
            lastUpdate,
            flowRate,
            sender
        );
    }

    function afterFlowUpdated(
        ISuperToken /*superToken*/,
        address sender,
        bytes calldata callBackData,
        bytes calldata ctx
    ) internal override returns(bytes memory) {
        (uint256 lastUpdate, int96 flowRate, address sender_) = abi.decode(callBackData, (uint256,int96,address));
        
        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
        beforeSenderHolder = sender_;
        return ctx;
    }

    // DELETE

    function beforeFlowDeleted(
        ISuperToken superToken,
        address sender,
        address receiver,
        bytes calldata /*ctx*/
    ) internal view override returns (bytes memory /*callbackData*/) {
        (uint256 lastUpdate, int96 flowRate,,) = superToken.getFlowInfo(sender, receiver);
        return abi.encode(
            lastUpdate,
            flowRate,
            sender,
            receiver
        );
    }

    function afterFlowDeleted(
        ISuperToken /*superToken*/,
        address sender,
        address receiver,
        bytes calldata callBackData,
        bytes calldata ctx
    ) internal override returns (bytes memory newCtx) {
        (
            uint256 lastUpdate, 
            int96 flowRate, 
            address sender_, 
            address receiver_
        ) = abi.decode(callBackData, (uint256,int96,address,address));

        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
        beforeSenderHolder = sender_;
        afterReceiverHolder = receiver;
        beforeReceiverHolder = receiver_;
        return ctx;
    }

}