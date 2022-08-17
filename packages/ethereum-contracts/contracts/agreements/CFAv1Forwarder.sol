// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import {
    ISuperfluid,
    ISuperToken,
    BatchOperation
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

import {
    ICFAv1Forwarder
} from "../interfaces/agreements/ICFAv1Forwarder.sol";


import { CallUtils } from "../libs/CallUtils.sol";

contract CFAv1Forwarder is ICFAv1Forwarder {

    ISuperfluid internal _host;
    IConstantFlowAgreementV1 internal _cfa;

    // is tied to a specific instance of host and agreement contracts at deploy time
    constructor(ISuperfluid host) {
        _host = host;
        _cfa = IConstantFlowAgreementV1(address(_host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        )));
    }

    /// inheritdoc ICFAv1Forwarder
    function setFlowrate(ISuperToken token, address receiver, uint256 flowrate) external {
       _setFlowrateFrom(token, msg.sender, receiver, int96(int256(flowrate)));
    }

    /// inheritdoc ICFAv1Forwarder
    function setFlowrateFrom(ISuperToken token, address sender, address receiver, uint256 flowrate) external {
        _setFlowrateFrom(token, sender, receiver, int96(int256(flowrate)));
    }

    /// inheritdoc ICFAv1Forwarder
    function getFlowrate(ISuperToken token, address sender, address receiver) external view 
        returns(uint256 flowrate) 
    {
        (, int96 flowrate96, , ) = _cfa.getFlow(token, sender, receiver);
        assert(flowrate96 >= 0); // can't be negative
        return uint256(int256(flowrate96));
    }

    /// inheritdoc ICFAv1Forwarder
    function getFlowInfo(ISuperToken token, address sender, address receiver) external view
        returns(uint256 lastUpdated, uint256 flowrate, uint256 deposit, uint256 owedDeposit)
    {
        (uint256 ts, int96 fr96, uint256 d, uint256 od) = _cfa.getFlow(token, sender, receiver);
        lastUpdated = ts;
        assert(fr96 >= 0); // can't be negative
        flowrate = uint256(int256(fr96));
        deposit = d;
        owedDeposit = od;
    }

    /// inheritdoc ICFAv1Forwarder
    // TODO: check flowrate range?
    function getBufferAmountByFlowrate(ISuperToken token, uint256 flowrate) external view
        returns (uint256 bufferAmount)
    {
        return _cfa.getDepositRequiredForFlowRate(token, int96(int256(flowrate)));
    }


    /// inheritdoc ICFAv1Forwarder
    function getAccountFlowrate(ISuperToken token, address account) external view
        returns (int96 flowrate)
    {
        return _cfa.getNetFlow(token, account);
    }

    /// inheritdoc ICFAv1Forwarder
    function getAccountFlowInfo(ISuperToken token, address account) external view
        returns (uint256 lastUpdated, int96 flowrate, uint256 deposit, uint256 owedDeposit)
    {
        return _cfa.getAccountFlowInfo(token, account);
    }

    /// inheritdoc ICFAv1Forwarder
    function createFlow(ISuperToken token, address sender, address receiver, int96 flowrate, bytes memory userData) external
    {
        _createFlow(token, sender, receiver, flowrate, userData);
    }
    
    /// inheritdoc ICFAv1Forwarder
    function updateFlow(ISuperToken token, address sender, address receiver, int96 flowrate, bytes memory userData) external
    {
        _updateFlow(token, sender, receiver, flowrate, userData);
    }

    /// inheritdoc ICFAv1Forwarder
    function deleteFlow(ISuperToken token, address sender, address receiver, bytes memory userData) external
    {
        _deleteFlow(token, sender, receiver, userData);
    }

    /// inheritdoc ICFAv1Forwarder
    function grantPermissions(ISuperToken token, address flowOperator) external
    {
        // TODO
    }

    /// inheritdoc ICFAv1Forwarder
    function revokePermissions(ISuperToken token, address flowOperator) external
    {
        // TODO
    }
    
    /// inheritdoc ICFAv1Forwarder
    function updateFlowOperatorPermissions(ISuperToken token, address flowOperator, uint8 permissions, int96 flowrateAllowance) external
    {
        // TODO
    }

    /// inheritdoc ICFAv1Forwarder
    function getFlowOperatorPermissions(ISuperToken token, address sender, address flowOperator) external view
        returns (uint8 permissions, int96 flowrateAllowance)
    {
        // TODO
    }

    /**************************************************************************
     * Internal functions
     *************************************************************************/

    function _setFlowrateFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowrate
    ) internal {
         (, int96 prevFlowRate,,) = _cfa.getFlow(token, sender, receiver);
        bytes memory cfaCallData;

        if (flowrate > 0) {
            if (prevFlowRate == 0) {
                // create flow
                cfaCallData = sender == msg.sender ?
                    abi.encodeCall(
                        _cfa.createFlow,
                        (
                            token,
                            receiver,
                            flowrate,
                            new bytes(0) // placeholder
                        )
                    ) :
                    abi.encodeCall(
                        _cfa.createFlowByOperator,
                        (
                            token,
                            sender,
                            receiver,
                            flowrate,
                            new bytes(0) // placeholder
                        )
                    );
            } else if (prevFlowRate != flowrate) {
                // update flow
                cfaCallData = cfaCallData = sender == msg.sender ?
                    abi.encodeCall(
                        _cfa.updateFlow,
                        (
                            token,
                            receiver,
                            flowrate,
                            new bytes(0) // placeholder
                        )
                    ) :
                    abi.encodeCall(
                        _cfa.updateFlowByOperator,
                        (
                            token,
                            sender,
                            receiver,
                            flowrate,
                            new bytes(0) // placeholder
                        )
                    );
            } else {
                // no change
                return;
            }
        } else if (flowrate == 0) {
            if (prevFlowRate > 0) {
                // delete flow
                cfaCallData = sender == msg.sender ?
                    abi.encodeCall(
                        _cfa.deleteFlow,
                        (
                            token,
                            sender,
                            receiver,
                            new bytes(0) // placeholder
                        )
                    ) :
                    abi.encodeCall(
                        _cfa.deleteFlowByOperator,
                        (
                            token,
                            sender,
                            receiver,
                            new bytes(0) // placeholder
                        )
                    );
            } else {
                // do nothing
                return;
            }
        } else {
            revert("invalid flowrate");
        }

        _forwardBatchCall(address(_cfa), cfaCallData);
    }

    function _createFlow(ISuperToken token, address sender, address receiver, int96 flowrate, bytes memory userData) internal {
        bytes memory cfaCallData = sender == msg.sender ?
            abi.encodeCall(
                _cfa.createFlow,
                (
                    token,
                    receiver,
                    flowrate,
                    new bytes(0) // placeholder
                )
            ) :
            abi.encodeCall(
                _cfa.createFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowrate,
                    new bytes(0) // placeholder
                )
            );

        _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }

    function _updateFlow(ISuperToken token, address sender, address receiver, int96 flowrate, bytes memory userData) internal {
        bytes memory cfaCallData = sender == msg.sender ?
            abi.encodeCall(
                _cfa.updateFlow,
                (
                    token,
                    receiver,
                    flowrate,
                    new bytes(0) // placeholder
                )
            ) :
            abi.encodeCall(
                _cfa.updateFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowrate,
                    new bytes(0) // placeholder
                )
            );

        _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }

    function _deleteFlow(ISuperToken token, address sender, address receiver, bytes memory userData) internal {
        bytes memory cfaCallData = sender == msg.sender ?
            abi.encodeCall(
                _cfa.deleteFlow,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0) // placeholder
                )
            ) :
            abi.encodeCall(
                _cfa.deleteFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0) // placeholder
                )
            );

        _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }


    // compiles the calldata of a single operation for the host invocation and executes it
    function _forwardBatchCall(address target, bytes memory callData) internal {
        _forwardBatchCall(target, callData, new bytes(0));
    }

    // version with userData parameter
    function _forwardBatchCall(address target, bytes memory callData, bytes memory userData) internal {
        bytes[] memory callDataArr = new bytes[](1);
        bytes[] memory userDataArr = new bytes[](1);
        callDataArr[0] = callData;
        userDataArr[0] = userData;
        _forwardBatchCallMany(target, callDataArr, userDataArr);
    }

    // compiles the calldata of multiple operations for the host invocation and executes it
    function _forwardBatchCallMany(address target, bytes[] memory callDataArr, bytes[] memory userDataArr) internal {
        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](callDataArr.length);
        for (uint i = 0; i < callDataArr.length; ++i) {
            ops[i] = ISuperfluid.Operation(
                BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT, // type
                address(target), // target
                abi.encode(
                    callDataArr[i], // callData
                    userDataArr[i] // userData
                )
            );
        }

        bytes memory fwBatchCallData = abi.encodeCall(
            _host.forwardBatchCall,
            (
                ops
            )
        );

        // solhint-disable-next-line avoid-low-level-calls
        (bool success, bytes memory returnedData) = address(_host).call(abi.encodePacked(fwBatchCallData, msg.sender));

        if (!success) {
            CallUtils.revertFromReturnedData(returnedData);
        }
    }
}
