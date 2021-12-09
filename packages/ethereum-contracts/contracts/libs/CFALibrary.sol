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

library CFALibrary {

    struct CFALibrarySetup {
        ISuperfluid host;
        IConstantFlowAgreementV1 cfa;
    }

    //@dev for working with the constant flow agreement within solidity
    //the first set of functions are each for callAgreement()
    //the second set of functions are each for use in callAgreementWithContext()

     //create flow without userData
    function createFlow(
        CFALibrarySetup storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.createFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //create flow with userData
    function createFlow(
        CFALibrarySetup storage cfaLibrary, 
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.createFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

    //update flow without userData
    function updateFlow(
        CFALibrarySetup storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //update flow with userData
    function updateFlow(
        CFALibrarySetup memory cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

    //delete flow
    function deleteFlow(
        CFALibrarySetup memory cfaLibrary,
        address sender,
        address receiver,
        ISuperfluidToken token
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //delete flow with userData 
    function deleteFlow(
        CFALibrarySetup memory cfaLibrary,
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) internal {
            cfaLibrary.host.callAgreement(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

  //create flow with ctx 
    function createFlowWithCtx(
        CFALibrarySetup memory cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.createFlow.selector,
                  token,
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
        CFALibrarySetup memory cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.createFlow.selector,
                  token,
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
        CFALibrarySetup memory cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.updateFlow.selector,
                  token,
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
        CFALibrarySetup memory cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData,
              ctx
            );
        }

  //delete flow with ctx 
    function deleteFlowWithCtx(
        CFALibrarySetup memory cfaLibrary,
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.deleteFlow.selector,
                  token,
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
        CFALibrarySetup memory cfaLibrary,
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
            (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
                cfaLibrary.cfa,
                abi.encodeWithSelector(
                  cfaLibrary.cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              userData,
              ctx
            );
        }
}