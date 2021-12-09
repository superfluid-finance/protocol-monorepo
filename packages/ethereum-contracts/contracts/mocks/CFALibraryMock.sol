// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;
pragma abicoder v2;

import {
    ISuperfluid,
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluid.sol";
// When ready to move to leave Remix, change imports to follow this pattern:
// "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

import { 
    CFAWrapper
} from "../libs/CFALibrary.sol";

contract CFALibraryMock {

    struct CFALibrarySetup {
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperToken token
    }

    ISuperfluid private _host;
    IConstantFlowAgreementV1 private _cfa;
    CFALibrarySetup public _token; 

    constructor(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperToken token
    ) {
        _token = (host, cfa, token)

    }

    using CFAWrapper for CFALibrarySetup;


    function createFlowTest(
        address receiver,
        int96 flowRate
    ) public {
        _token.createFlow(receiver,flowRate);
    }

    function createFlowWithUserDataTest(
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
        _token.createFlow(receiver, flowRate, userData);
    }

    function updateFlowTest(
        address receiver,
        int96 flowRate
    ) public {
        _token.updateFlow(receiver, flowRate);
    }

    function updateFlowWithUserDataTest(
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
        _token.updateFlow(receiver, flowRate, userData);
    }

    function deleteFlowTest(
        address sender,
        address receiver,
    ) public {
        _token.createFlow(sender, receiver);
    }

    function deleteFlowWithUserDataTest(
        address sender
        address receiver,
        bytes memory userData
    ) public {
        _token.deleteFlow(sender, receiver, userData);
    }

    function createFlowWithCtxTest(
        bytes memory ctx
        address receiver,
        int96 flowRate
    ) public {
        _token.createFlowWithCtx(ctx, receiver,flowRate);
    }

    function createFlowWithCtxUserDataTest(
        bytes memory ctx,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
        _token.createFlowWithCtx(ctx, receiver, flowRate, userData);
    }

    function updateFlowWithCtxTest(
        bytes memory ctx,
        address receiver,
        int96 flowRate
    ) public {
        _token.updateFlowWithCtx(ctx, receiver, flowRate);
    }

    function updateFlowWithCtxUserDataTest(
        bytes memory ctx,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
        _token.updateFlow(ctx, receiver, flowRate, userData);
    }

    function deleteFlowWithCtxTest(
        bytes memory ctx,
        address sender,
        address receiver,
    ) public {
        _token.deleteFlow(ctx, sender, receiver);
    }

    function deleteFlowWithCtxUserDataTest(
        bytes memory ctx,
        address sender
        address receiver,
        bytes memory userData
    ) public {
        _token.deleteFlow(ctx, sender, receiver, userData);
    }

}  