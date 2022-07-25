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
    IInstantDistributionAgreementV1
} from "../interfaces/agreements/IInstantDistributionAgreementV1.sol";

import { CallUtils } from "../libs/CallUtils.sol";

/**
 * The AgreementForwarder contract provides an easy to use interface to
 * Superfluid agreement specific functionality of Super Tokens.
 * Instances of this contract can operate on the protocol only if configured as "trusted forwarder"
 * by protocol governance.
 *
 * TODO: add meta-tx capabilities to methods with constant calldata length
 */
contract AgreementForwarder {

    ISuperfluid internal _host;
    IConstantFlowAgreementV1 internal _cfa;
    IInstantDistributionAgreementV1 internal _ida;

    // is tied to a specific instance of host and agreement contracts at deploy time
    constructor(ISuperfluid host) {
        _host = host;
        _cfa = IConstantFlowAgreementV1(address(_host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        )));
        _ida = IInstantDistributionAgreementV1(address(_host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1")
        )));
    }

    /**************************************************************************
     * CFA operations
     *************************************************************************/

    /**
     * @notice Sets a constant flow to the receiver with the given flowrate
     * @dev Higher-order function which invokes createFlow, updateFlow or deleteFlow as needed
     * @param token Super token address
     * @param receiver The receiver of the flow
     * @param flowRate The new flowrate to be set
     */
    function setFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate
    ) external {
        (, int96 prevFlowRate,,) = _cfa.getFlow(token, msg.sender, receiver);
        bytes memory cfaCallData;

        if (flowRate > 0) {
            if (prevFlowRate == 0) {
                // create flow
                cfaCallData = abi.encodeCall(
                    _cfa.createFlow,
                    (
                        token,
                        receiver,
                        flowRate,
                        new bytes(0) // placeholder
                    )
                );
            } else if (prevFlowRate != flowRate) {
                // update flow
                cfaCallData = abi.encodeCall(
                    _cfa.updateFlow,
                    (
                        token,
                        receiver,
                        flowRate,
                        new bytes(0) // placeholder
                    )
                );
            } else {
                // no change
                return;
            }
        } else if (flowRate == 0) {
            if (prevFlowRate > 0) {
                // delete flow
                cfaCallData = abi.encodeCall(
                    _cfa.deleteFlow,
                    (
                        token,
                        msg.sender,
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

        forwardBatchCall(address(_cfa), cfaCallData);
    }

    /**************************************************************************
     * IDA operations - publisher
     *************************************************************************/

    /**
     * @notice Creates an index with the msg sender as publisher
     * @dev see IInstantDistributionAgreementV1.createIndex
     * @param token Super token address
     * @param indexId Id of the index, unique per publisher
     * @dev see IInstantDistributionAgreementV1.createIndex
     */
    function createIndex(
        ISuperToken token,
        uint32 indexId
    ) public {
        bytes memory idaCallData = abi.encodeCall(
            _ida.createIndex,
            (
                token,
                indexId,
                new bytes(0) // placeholder
            )
        );

        forwardBatchCall(address(_ida), idaCallData);
    }

    /**
     * @notice Creates an index and sets initial subscriptions
     * @dev Higher-order function which batches createIndex and 1-n updateSubscription
     * @param token Super token address
     * @param indexId Id of the index, unique per publisher
     * @param subscribers List of accounts to be subscribed
     * @param units List of units to be assigned to subscribed accounts
     */
    function createIndexWithSubscriptions(
        ISuperToken token,
        uint32 indexId,
        address[] calldata subscribers,
        uint128[] calldata units
    ) external {
        createIndex(token, indexId);
        updateSubscriptions(token, indexId, subscribers, units);
    }

    /**
     * @notice Creates or changes a subscription to an index
     * @dev see IInstantDistributionAgreementV1.updateSubscription
     * @param token Super token address
     * @param indexId Id of the index, unique per publisher
     * @param subscriber Account to be subscribed
     * @param units Units to be assigned to the subscribed account
     */
    function updateSubscription(
        ISuperToken token,
        uint32 indexId,
        address subscriber,
        uint128 units
    ) external {
        bytes memory idaCallData = abi.encodeCall(
            _ida.updateSubscription,
            (
                token,
                indexId,
                subscriber,
                units,
                new bytes(0) // placeholder
            )
        );

        forwardBatchCall(address(_ida), idaCallData);
    }

    /**
     * @notice Creates and/or changes subscriptions to an index
     * @dev Higher-order function which batches multiple invocations of updateSubscription
     * @param token Super token address
     * @param indexId Id of the index, unique per publisher
     * @param subscribers List of accounts to be subscribed
     * @param units List of units to be assigned to subscribed accounts
     */
    function updateSubscriptions(
        ISuperToken token,
        uint32 indexId,
        address[] calldata subscribers,
        uint128[] calldata units
    ) public {
        require(subscribers.length == units.length, "array length mismatch");
        bytes[] memory idaCallDataArr = new bytes[](subscribers.length);
        for (uint i = 0; i < subscribers.length; ++i) {
            idaCallDataArr[i] = abi.encodeCall(
                _ida.updateSubscription,
                (
                    token,
                    indexId,
                    subscribers[i],
                    units[i],
                    new bytes(0) // placeholder
                )
            );
        }

        forwardBatchCallMany(address(_ida), idaCallDataArr);
    }

    /**
     * @notice Distributes a given amount of tokens to subscribers of the given index
     * @dev see IInstantDistributionAgreementV1.distribute
     * @param token Super token address
     * @param indexId Id of the index, unique per publisher
     * @param amount The amount to be distributed
     */
    function distribute(
        ISuperToken token,
        uint32 indexId,
        uint256 amount
    ) external {
        bytes memory idaCallData = abi.encodeCall(
            _ida.distribute,
            (
            token,
            indexId,
            amount,
            new bytes(0) // placeholder
            )
        );

        forwardBatchCall(address(_ida), idaCallData);
    }


    /**************************************************************************
     * Index operations - subscriber
     *************************************************************************/

    /**
     * @notice
     * @dev see IInstantDistributionAgreementV1.approveSubscription
     * @param token Super token address
     * @param publisher The publisher of the index
     * @param indexId Id of the index, unique per publisher
     */
    function approveSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId
    ) external {
        bytes memory idaCallData = abi.encodeCall(
            _ida.approveSubscription,
            (
                token,
                publisher,
                indexId,
                new bytes(0) // placeholder
            )
        );

        forwardBatchCall(address(_ida), idaCallData);
    }

    /**************************************************************************
     * Internal functions
     *************************************************************************/

    // compiles the calldata of a single operation for the host invocation and executes it
    function forwardBatchCall(address target, bytes memory callData) internal {
        bytes[] memory callDataArr = new bytes[](1);
        callDataArr[0] = callData;
        forwardBatchCallMany(target, callDataArr);
    }

    // compiles the calldata of multiple operations for the host invocation and executes it
    function forwardBatchCallMany(address target, bytes[] memory callDataArr) internal {
        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](callDataArr.length);
        for (uint i = 0; i < callDataArr.length; ++i) {
            ops[i] = ISuperfluid.Operation(
                BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT, // type
                address(target), // target
                abi.encode(
                    callDataArr[i], // callData
                    new bytes(0) // userData
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
