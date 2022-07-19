// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;


/**
* Provides an easier to use interface to Superfluid agreement specific functionality of Super Tokens
*/

import {
    ISuperfluid,
    ISuperToken,
    BatchOperation
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

import { CallUtils } from "../libs/CallUtils.sol";

// TODO: consider adding meta-tx capabilities to all functions
contract AgreementForwarder {

    ISuperfluid internal _host;
    IConstantFlowAgreementV1 internal _cfa;

    constructor(ISuperfluid host) {
        _host = host;
        _cfa = IConstantFlowAgreementV1(address(_host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        )));
    }

    function createConstantFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate
    ) external {
        bytes memory cfaCallData = abi.encodeCall(
            _cfa.createFlow,
            (
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            )
        );

        ISuperfluid.Operation memory op = ISuperfluid.Operation(
            BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT, // type
            address(_cfa), // target
            abi.encode(
                cfaCallData, // callData
                new bytes(0) // userData
            )
        );

        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](1);
        ops[0] = op;

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

    // aggregate create, update and delete
    // function constantFlow(token, sender, receiver, flowRate)

}
