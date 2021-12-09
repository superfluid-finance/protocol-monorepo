 //SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;
pragma experimental ABIEncoderV2;

import {
    ISuperfluid,
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

import { 
    CFAWrapper
} from "../libs/CFALibrary.sol";

contract CFALibraryTest {

    using CFALibrary for CFALibrary.CFALibrarySetup;

    //initialize cfaV1 variable
    CFALibrary.CFALibrarySetup public cfaV1; 

    constructor(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa
    ) {
        //initialize CFALibrarySetup struct, and set equal to cfaV1
        cfaV1 = CFALibrary.CFALibrarySetup(host, cfa);
    }

    function createFlowTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
        cfaV1.createFlow(receiver, token, flowRate);
    }

    function createFlowWithUserDataTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
        cfaV1.createFlow(receiver, token, flowRate, userData);
    }

    function updateFlowTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
        cfaV1.updateFlow(receiver, token, flowRate);
    }

    function updateFlowWithUserDataTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
        cfaV1.updateFlow(receiver, token, flowRate, userData);
    }

    function deleteFlowTest(
        address sender,
        address receiver,
        ISuperfluidToken token
    ) public {
        cfaV1.deleteFlow(sender, receiver, token);
    }

    function deleteFlowWithUserDataTest(
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) public {
        cfaV1.deleteFlow(sender, receiver, token, userData);
    }

    function createFlowWithCtxTest(
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.createFlowWithCtx(ctx, receiver,token, flowRate);
    }

    function createFlowWithCtxUserDataTest(
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) public {
        cfaV1.createFlowWithCtx(ctx, receiver, token, flowRate, userData);
    }

    function updateFlowWithCtxTest(
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.updateFlowWithCtx(ctx, receiver, token, flowRate);
    }

    function updateFlowWithCtxUserDataTest(
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) public {
        cfaV1.updateFlowWithCtx(ctx, receiver, token, flowRate, userData);
    }

    function deleteFlowWithCtxTest(
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token
    ) public {
        cfaV1.deleteFlowWithCtx(ctx, sender, receiver, token);
    }

    function deleteFlowWithCtxUserDataTest(
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) public {
        cfaV1.deleteFlowWithCtx(ctx, sender, receiver, token, userData);
    }

}  