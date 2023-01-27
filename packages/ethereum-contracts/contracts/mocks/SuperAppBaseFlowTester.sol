// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperfluid, ISuperToken, SuperAppBaseFlow, SuperAppDefinitions } from "../apps/SuperAppBaseFlow.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";

error ErraticSender();
error ErraticReceiver();

contract SuperAppBaseFlowTester is SuperAppBaseFlow {
    using SuperTokenV1Library for ISuperToken;

    uint256 public lastUpdateHolder;

    int96 public oldFlowRateHolder;

    address public afterSenderHolder;

    address public afterReceiverHolder;

    constructor(
        ISuperfluid host
    ) SuperAppBaseFlow(
        host,
        0
    ) {}

    // SETTING ACCEPTED SUPER TOKENS

    function setAcceptedSuperToken(ISuperToken acceptedSuperToken) public {
        _acceptedSuperTokens[acceptedSuperToken] = true;
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
        
        if (sender != sender_) revert ErraticSender();

        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
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
        
        if (sender != sender_) revert ErraticSender();

        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
        return ctx;
    }

    // DELETE

    function beforeFlowDeleted(
        ISuperToken superToken,
        address sender,
        address receiver,
        bytes calldata /*ctx*/
    ) internal view override returns (bytes memory /*callbackData*/) {
        (uint256 lastUpdate, int96 flowRate,,) = superToken.getFlowInfo(sender, address(this));
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
        ) = abi.decode(callBackData, (uint256,int96,address));
        
        if (sender != sender_) revert ErraticSender();
        if (receiver != receiver_) revert ErraticReceiver();

        lastUpdateHolder = lastUpdate;
        oldFlowRateHolder = flowRate;
        afterSenderHolder = sender;
        afterReceiverHolder = receiver;
        return ctx;
    }

}