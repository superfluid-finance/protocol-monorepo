//SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;
pragma experimental ABIEncoderV2;

import {
    ISuperfluid,
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluid.sol";

// When ready to move to leave Remix, change imports to follow this pattern:
// "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

library CFAWrapper {

    //@dev for working with the constant flow agreement within solidity
    //the first set of functions are each for callAgreement()
    //the second set of functions are each for use in callAgreementWithContext()

     //create flow without userData or extra ctx for cfa
    function createFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate
    ) internal {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.createFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //create flow with userData but without extra ctx for cfa
    function createFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate,
        bytes memory _userData
    ) public {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.createFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  new bytes(0) // placeholder
              ),
              _userData
            );
        }

    //create flow with userData and extra ctx for cfa
    function createFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate,
        bytes memory _userData,
        bytes memory _cfaCtx
    ) public {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.createFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  _cfaCtx
              ),
              _userData
            );
        }

    //update flow without userData or extra ctx for cfa
    function updateFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate
    ) public {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.updateFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //update flow with userData but without extra ctx for cfa
    function updateFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate,
        bytes memory _userData
    ) public {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.updateFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  new bytes(0) // placeholder
              ),
              _userData
            );
        }

    //update flow with userData and extra ctx for cfa
    function updateFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate,
        bytes memory _userData,
        bytes memory _cfaCtx
    ) public {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.updateFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  _cfaCtx
              ),
              _userData
            );
        }

    //delete flow without userData or extra ctx for cfa
    function deleteFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _sender,
        address _receiver
    ) public {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.deleteFlow.selector,
                  _token,
                  _sender,
                  _receiver,
                  new bytes(0) // placeholder
              ),
              "0x" //empty user data
            );
        }
    

    //delete flow with userData but without extra ctx for cfa
    function deleteFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _sender,
        address _receiver,
        bytes memory _userData
    ) public {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.deleteFlow.selector,
                  _token,
                  _sender,
                  _receiver,
                  new bytes(0) // placeholder
              ),
              _userData
            );
        }

    //delete flow with userData and extra ctx for cfa
    function deleteFlow(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _sender,
        address _receiver,
        bytes memory _userData,
        bytes memory _cfaCtx
    ) public {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.deleteFlow.selector,
                  _token,
                  _sender,
                  _receiver,
                  _cfaCtx 
              ),
              _userData
            );
        }

  //create flow with ctx but without userData or extra ctx for cfa
    function createFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.createFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              newCtx
            );
        }

    //create flow with ctx and userData but without extra ctx for cfa
    function createFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate,
        bytes memory _userData
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.createFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  new bytes(0) // placeholder
              ),
              _userData,
              newCtx
            );
        }

    //create flow with ctx and userData and extra ctx for cfa
    function createFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate,
        bytes memory _userData,
        bytes memory _cfaCtx
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.createFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  _cfaCtx
              ),
              _userData,
              newCtx
            );
        }

  //update flow with ctx but without userData and extra ctx for cfa
    function updateFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.updateFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              newCtx
            );
        }

    //update flow with ctx and userData, but without extra ctx for cfa
    function updateFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate,
        bytes memory _userData
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.updateFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  new bytes(0) // placeholder
              ),
              _userData,
              newCtx
            );
        }

    //update flow with ctx and userData and extra ctx for cfa
    function updateFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _receiver,
        int96 _flowRate,
        bytes memory _userData,
        bytes memory _cfaCtx
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.updateFlow.selector,
                  _token,
                  _receiver,
                  _flowRate,
                  _cfaCtx
              ),
              _userData,
              newCtx
            );
        }

  //delete flow with ctx but without userData or extra ctx for cfa
    function deleteFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _sender,
        address _receiver
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.deleteFlow.selector,
                  _token,
                  _sender,
                  _receiver,
                  new bytes(0) // placeholder
              ),
              "0x", //empty user data
              newCtx
            );
        }

    //delete flow with ctx and userData but without extra ctx for cfa
    function deleteFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _sender,
        address _receiver,
        bytes memory _userData
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.deleteFlow.selector,
                  _token,
                  _sender,
                  _receiver,
                  new bytes(0) // placeholder
              ),
              _userData,
              newCtx
            );
        }

    //delete flow with ctx and userData and extra ctx for cfa
    function deleteFlowWithCtx(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _sender,
        address _receiver,
        bytes memory _userData,
        bytes memory _cfaCtx
    ) public returns (bytes memory newCtx) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                  _cfa.deleteFlow.selector,
                  _token,
                  _sender,
                  _receiver,
                  _cfaCtx
              ),
              _userData,
              newCtx
            );
        }

}