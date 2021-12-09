//SPDX-License-Identifier: AGPLv3
pragma solidity ^0.7.0;
pragma experimental ABIEncoderV2;

import {
    ISuperfluid,
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

library CFAWrapper {

    struct CFALibrarySetup {
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperToken token
    }

    //@dev for working with the constant flow agreement within solidity
    //the first set of functions are each for callAgreement()
    //the second set of functions are each for use in callAgreementWithContext()

     //create flow without userData
    function createFlow(
        CFALibrarySetup cfaLibrary,
        address receiver,
        int96 flowRate
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.createFlow.selector,
                  cfaLibrary.token.,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //create flow with userData
    function createFlow(
        CFALibrarySetup cfaLibrary, 
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.createFlow.selector,
                  cfaLibrary.token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

    //update flow without userData
    function updateFlow(
        CFALibrarySetup cfaLibrary,
        address receiver,
        int96 flowRate
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.updateFlow.selector,
                  cfaLibrary.token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //update flow with userData
    function updateFlow(
        CFALibrarySetup cfaLibrary,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.updateFlow.selector,
                  cfaLibrary.token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

    //delete flow
    function deleteFlow(
        CFALibrarySetup cfaLibrary,
        address sender,
        address receiver
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.deleteFlow.selector,
                  cfaLibrary.token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //delete flow with userData 
    function deleteFlow(
        CFALibrarySetup cfaLibrary,
        address sender,
        address receiver,
        bytes memory userData
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.deleteFlow.selector,
                  cfaLibrary.token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

  //create flow with ctx 
    function createFlowWithCtx(
        CFALibrarySetup cfaLibrary,
        bytes memory ctx,
        address receiver,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.createFlow.selector,
                  cfaLibrary.token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              ctx
            );
        }

    //create flow with ctx and userData
    function createFlowWithCtx(
        CFALibrarySetup cfaLibrary,
        bytes memory ctx,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.createFlow.selector,
                  cfaLibrary.token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData,
              ctx
            );
        }

  //update flow with ctx but without userData 
    function updateFlowWithCtx(
        CFALibrarySetup cfaLibrary,
        bytes memory ctx,
        address receiver,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.updateFlow.selector,
                  cfaLibrary.token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              ctx
            );
        }

    //update flow with ctx and userData
    function updateFlowWithCtx(
        CFALibrarySetup cfaLibrary,
        bytes memory ctx,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData,
              newCtx
            );
        }

  //delete flow with ctx 
    function deleteFlowWithCtx(
        CFALibrarySetup cfaLibrary,
        bytes memory ctx,
        address sender,
        address receiver
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.deleteFlow.selector,
                  cfaLibrary.token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              ctx
            );
        }

    //delete flow with ctx and userData 
    function deleteFlowWithCtx(
        CFALibrarySetup cfaLibrary,
        bytes memory ctx,
        address sender,
        address receiver,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.deleteFlow.selector,
                  cfaLibrary.token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              userData,
              ctx
            );
        }
}