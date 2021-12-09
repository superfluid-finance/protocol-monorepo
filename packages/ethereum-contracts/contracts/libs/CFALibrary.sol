//SPDX-License-Identifier: MIT
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

    //@dev for working with the constant flow agreement within solidity
    //the first set of functions are each for callAgreement()
    //the second set of functions are each for use in callAgreementWithContext()

     //create flow without userData or extra ctx for cfa
    function createFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.createFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //create flow with userData but without extra ctx for cfa
    function createFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.createFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

    //create flow with userData and extra ctx for cfa
    function createFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData,
        bytes memory cfaCtx
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.createFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  cfaCtx
              ),
              userData
            );
        }

    //update flow without userData or extra ctx for cfa
    function updateFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //update flow with userData but without extra ctx for cfa
    function updateFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

    //update flow with userData and extra ctx for cfa
    function updateFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData,
        bytes memory cfaCtx
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  cfaCtx
              ),
              userData
            );
        }

    //delete flow without userData or extra ctx for cfa
    function deleteFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address sender,
        address receiver
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //delete flow with userData but without extra ctx for cfa
    function deleteFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes memory userData
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              userData
            );
        }

    //delete flow with userData and extra ctx for cfa
    function deleteFlow(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes memory userData,
        bytes memory cfaCtx
    ) public {
            host.callAgreement(
                cfa,
                abi.encodeWithSelector(
                  cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  cfaCtx 
              ),
              userData
            );
        }

  //create flow with ctx but without userData or extra ctx for cfa
    function createFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.createFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              newCtx
            );
        }

    //create flow with ctx and userData but without extra ctx for cfa
    function createFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.createFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              userData,
              newCtx
            );
        }

    //create flow with ctx and userData and extra ctx for cfa
    function createFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData,
        bytes memory cfaCtx
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.createFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  cfaCtx
              ),
              userData,
              newCtx
            );
        }

  //update flow with ctx but without userData and extra ctx for cfa
    function updateFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              newCtx
            );
        }

    //update flow with ctx and userData, but without extra ctx for cfa
    function updateFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public returns (bytes memory newCtx) {
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

    //update flow with ctx and userData and extra ctx for cfa
    function updateFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData,
        bytes memory cfaCtx
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.updateFlow.selector,
                  token,
                  receiver,
                  flowRate,
                  cfaCtx
              ),
              userData,
              newCtx
            );
        }

  //delete flow with ctx but without userData or extra ctx for cfa
    function deleteFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address sender,
        address receiver
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              newCtx
            );
        }

    //delete flow with ctx and userData but without extra ctx for cfa
    function deleteFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes memory userData
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  new bytes(0) // placeholder
              ),
              userData,
              newCtx
            );
        }

    //delete flow with ctx and userData and extra ctx for cfa
    function deleteFlowWithCtx(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes memory userData,
        bytes memory cfaCtx
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = host.callAgreementWithContext(
                cfa,
                abi.encodeWithSelector(
                  cfa.deleteFlow.selector,
                  token,
                  sender,
                  receiver,
                  cfaCtx
              ),
              userData,
              newCtx
            );
        }
}