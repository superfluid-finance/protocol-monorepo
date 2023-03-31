// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import { ISuperfluid, ISuperToken, ISuperApp, SuperAppDefinitions } from "../interfaces/superfluid/ISuperfluid.sol";
import { SuperAppBaseCFA } from "../apps/SuperAppBaseCFA.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";

contract SuperAppBaseCFATester is SuperAppBaseCFA {
    using SuperTokenV1Library for ISuperToken;

    int96 public oldFlowRateHolder;
    uint256 public lastUpdateHolder;
    address public afterSenderHolder;
    address public afterReceiverHolder;

    mapping(ISuperToken => bool) internal _acceptedSuperTokens;
    // irreversibly set to true once the setter is invoked
    bool internal _restrictAcceptedSuperTokens;

    constructor(
        ISuperfluid host,
        bool activateOnCreated,
        bool activateOnUpdated,
        bool activateOnDeleted
    ) SuperAppBaseCFA (host, activateOnCreated, activateOnUpdated, activateOnDeleted) {
        lastUpdateHolder = 0; // appeasing linter
    }

    // SETTING ACCEPTED SUPER TOKENS

    function setAcceptedSuperToken(ISuperToken acceptedSuperToken, bool accepted) public {
        _restrictAcceptedSuperTokens = true;
        _acceptedSuperTokens[acceptedSuperToken] = accepted;
    }

    // override filtering function
    function isAcceptedSuperToken(ISuperToken superToken) public view override returns (bool) {
        return _restrictAcceptedSuperTokens ?
            _acceptedSuperTokens[superToken] :
            super.isAcceptedSuperToken(superToken);
        // fallback to the default impl allows us to easily test it
    }

    // ARBITRARY START STREAM

    function startStream(ISuperToken superToken, address receiver, int96 flowRate) public {
        superToken.createFlow(receiver, flowRate);
    }

    // CREATE

    function onFlowCreated(
        ISuperToken /*superToken*/,
        address sender,
        bytes calldata ctx
    ) internal override returns(bytes memory) {
        afterSenderHolder = sender;
        return ctx;
    }

    // UPDATE

    function onFlowUpdated(
        ISuperToken /*superToken*/,
        address sender,
        int96 previousFlowRate,
        uint256 lastUpdated,
        bytes calldata ctx
    ) internal override returns(bytes memory) {
        lastUpdateHolder = lastUpdated;
        oldFlowRateHolder = previousFlowRate;
        afterSenderHolder = sender;
        return ctx;
    }

    // DELETE

    function onFlowDeleted(
        ISuperToken /*superToken*/,
        address sender,
        address receiver,
        int96 previousFlowRate,
        uint256 lastUpdated,
        bytes calldata ctx
    ) internal override returns (bytes memory newCtx) {
        lastUpdateHolder = lastUpdated;
        oldFlowRateHolder = previousFlowRate;
        afterSenderHolder = sender;
        afterReceiverHolder = receiver;
        return ctx;
    }
}
