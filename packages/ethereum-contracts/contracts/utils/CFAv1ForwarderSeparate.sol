// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.4;

import {
    ISuperfluid,
    ISuperToken,
    BatchOperation,
    FlowOperatorDefinitions
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

import { CallUtils } from "../libs/CallUtils.sol";

/**
 * The CFAv1Forwarder contract provides an easy to use interface to
 * ConstantFlowAgreementV1 specific functionality of Super Tokens.
 * Instances of this contract can operate on the protocol only if configured as "trusted forwarder"
 * by protocol governance.
 */
contract CFAv1ForwarderSeparate {
    error CFA_FWD_INVALID_FLOW_RATE();

    ISuperfluid internal immutable _host;
    IConstantFlowAgreementV1 internal immutable _cfa;

    // is tied to a specific instance of host and agreement contracts at deploy time
    constructor(ISuperfluid host) {
        _host = host;
        _cfa = IConstantFlowAgreementV1(address(_host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        )));
    }

    /**
    * @notice Wrapper of createFlow/createFlowByOperator.
     * If the address of msg.sender is not the same as the address of the `sender` argument,
     * createFlowByOperator is used internally. In this case msg.sender needs to have permission to create flows
     * on behalf of the given sender account with sufficient flowRateAllowance.
     * Currently, only 1 flow can exist between 2 accounts, thus `createFlow` will fail if one already exists.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param flowRate The flowrate in wad/second to be set initially
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     * @return true if the call succeeded
     */
    function createFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) external returns (bool) {
        return _createFlow(token, sender, receiver, flowRate, userData);
    }

    function createFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate
    ) external returns (bool) {
        return _createFlow(token, sender, receiver, flowRate, new bytes(0));
    }

    function createFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) external returns (bool) {
        return _createFlow(token, msg.sender, receiver, flowRate, userData);
    }

    function createFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate
    ) external returns (bool) {
        return _createFlow(token, msg.sender, receiver, flowRate, new bytes(0));
    }

    /**
     * @notice Wrapper of updateFlow/updateFlowByOperator.
     * If the address of msg.sender doesn't match the address of the `sender` argument,
     * updateFlowByOperator is invoked. In this case msg.sender needs to have permission to update flows
     * on behalf of the given sender account with sufficient flowRateAllowance.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param flowRate The flowrate in wad/second the flow should be updated to
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     * @return bool
     */
    function updateFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) external returns (bool) {
        return _updateFlow(token, sender, receiver, flowRate, userData);
    }

    function updateFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate
    ) external returns (bool) {
        return _updateFlow(token, sender, receiver, flowRate, new bytes(0));
    }

    function updateFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) external returns (bool) {
        return _updateFlow(token, msg.sender, receiver, flowRate, userData);
    }

    function updateFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate
    ) external returns (bool) {
        return _updateFlow(token, msg.sender, receiver, flowRate, new bytes(0));
    }

    /**
     * @notice Wrapper of deleteFlow/deleteFlowByOperator.
     * If msg.sender isn't the same as sender address, msg.sender needs to have permission
     * to delete flows on behalf of the given sender account.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     * @return bool
     */
    function deleteFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory userData
    ) external returns (bool) {
        return _deleteFlow(token, sender, receiver, userData);
    }

    function deleteFlowFrom(
        ISuperToken token,
        address sender,
        address receiver
    ) external returns (bool) {
        return _deleteFlow(token, sender, receiver, new bytes(0));
    }

    function deleteFlow(
        ISuperToken token,
        address receiver,
        bytes memory userData
    ) external returns (bool) {
        return _deleteFlow(token, msg.sender, receiver, userData);
    }

    function deleteFlow(
        ISuperToken token,
        address receiver
    ) external returns (bool) {
        return _deleteFlow(token, msg.sender, receiver, new bytes(0));
    }

    /**
     * @notice Sets the given flowrate between sender and receiver account,
     * using the appropriate (depending on previous state) CRUD operation.
     * If the msg.sender is not the flow sender, the msg.sender needs to have ACL permissions.
     * @param token Super token address
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The wanted flowrate in wad/second. Only positive values are valid here.
     * @param userData optional - can be used to provide application specific data to SuperApps
     * @return ctx - only needed when invoked from SuperApps
     */
    function setFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) external returns (bool) {
        return _setFlowFrom(token, sender, receiver, flowRate, userData);
    }

    function setFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate
    ) external returns (bool) {
        return _setFlowFrom(token, sender, receiver, flowRate, new bytes(0));
    }

    function setFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) external returns (bool) {
        return _setFlowFrom(token, msg.sender, receiver, flowRate, userData);
    }

    function setFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate
    ) external returns (bool) {
        return _setFlowFrom(token, msg.sender, receiver, flowRate, new bytes(0));
    }

    /**
     * @notice Sets the permissions for a specific flowOperator
     * In order to restrict what a flowOperator can or can't do, the flowOperator account
     * should be a contract implementing the desired restrictions.
     * @param token Super token address
     * @param flowOperator Account to which permissions are granted
     * @param allowCreate If set, the flowOperator is allowed to create flows
     * @param allowUpdate If set, the flowOperator is allowed to update flows
     * @param allowDelete If set, the flowOperator is allowed to delete flows
     * @param flowRateAllowance Max. flowrate in wad/second the operator can set for individual flows.
     * @return bool
     */
    function setFlowPermissions(
        ISuperToken token,
        address flowOperator,
        bool allowCreate,
        bool allowUpdate,
        bool allowDelete,
        int96 flowRateAllowance
    ) external returns (bool) {
        uint8 permissionsBitmask = (allowCreate ? 1 : 0)
        | (allowUpdate ? 1 : 0) << 1
        | (allowDelete ? 1 : 0) << 2;
        return _updateFlowOperatorPermissions(
            token,
            flowOperator,
            permissionsBitmask,
            flowRateAllowance
        );
    }

    /**
     * @notice Get the flowrate of the flow between 2 accounts if exists.
     * @dev Currently, only 0 or 1 flows can exist between 2 accounts. This may change in the future.
     * @param token Super token address
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @return flowRate The flowrate from the sender to the receiver account. Returns 0 if no flow exists.
     */
    function getFlowRate(ISuperToken token, address sender, address receiver) external view
        returns(int96 flowRate)
    {
        (, flowRate, , ) = _cfa.getFlow(token, sender, receiver);
    }

    /**
     * @notice Get all available information about a flow (if exists).
     * If only the flowrate is needed, consider using `getFlowrate` instead.
     * @param token Super token address
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @return lastUpdated Timestamp of last update (flowrate change) or zero if no flow exists
     * @return flowRate Current flowrate of the flow or zero if no flow exists
     * @return deposit Deposit amount locked as security buffer during the lifetime of the flow
     * @return owedDeposit Extra deposit amount borrowed to a SuperApp receiver by the flow sender
     */
    function getFlowInfo(ISuperToken token, address sender, address receiver) external view
        returns(uint256 lastUpdated, int96 flowRate, uint256 deposit, uint256 owedDeposit)
    {
        (lastUpdated, flowRate, deposit, owedDeposit) = _cfa.getFlow(token, sender, receiver);
    }

    /**
     * @notice Get the net flowrate of an account.
     * @param token Super token address
     * @param account Account to query
     * @return flowRate The net flowrate (aggregate incoming minus aggregate outgoing flowrate), can be negative.
     */
    function getNetFlowRate(ISuperToken token, address account) external view
        returns (int96 flowRate)
    {
        return _cfa.getNetFlow(token, account);
    }

    /**
     * @notice Get information about the net flow (if any flow exists) of an account.
     * If only the net flowrate is needed, consider using `getAccountFlowrate` instead.
     * @param token Super token address
     * @param account Account to query
     * @return lastUpdated Timestamp of last update of a flow to or from the account (flowRate change)
     * @return flowRate Current net aggregate flowrate
     * @return deposit Aggregate deposit amount currently locked as security buffer for outgoing flows
     * @return owedDeposit Aggregate extra deposit amount currently borrowed to SuperApps receiving from this account
     */
    function getNetFlowInfo(ISuperToken token, address account) external view
        returns (uint256 lastUpdated, int96 flowRate, uint256 deposit, uint256 owedDeposit)
    {
        return _cfa.getAccountFlowInfo(token, account);
    }

    /**
     * @notice Get the buffer amount required for the given token and flowrate.
     * This amount can vary based on the combination of token, flowrate and chain being queried.
     * The result for a given set of parameters can change over time,
     * because it depends on governance configurable protocol parameters.
     * Changes of the required buffer amount affect only flows created or updated after the change.
     * @param token Super token address
     * @param flowRate The flowrate for which the buffer amount is calculated
     * @return bufferAmount The buffer amount required for the given configuration.
     */
    function getBufferAmountByFlowRate(ISuperToken token, int96 flowRate) external view
        returns (uint256 bufferAmount)
    {
        return _cfa.getDepositRequiredForFlowRate(token, flowRate);
    }

    /**
     * @notice Get the currently set permissions granted to the given flowOperator by the given sender account.
     * @param token Super token address
     * @param sender The account which (possiby) granted permissions
     * @param flowOperator Account to which (possibly) permissions were granted
     * @return allowCreate True if the flowOperator has permission to create flows
     * @return allowUpdate True if the flowOperator has permission to update flows
     * @return allowDelete True if the flowOperator has permission to delete flows
     * @return flowRateAllowance Max. flowrate in wad/second the flowOperator can set for individual flows.
     */
    function getFlowPermissions(ISuperToken token, address sender, address flowOperator) external view
        returns (bool allowCreate, bool allowUpdate, bool allowDelete, int96 flowRateAllowance)
    {
        uint8 permissionsBitmask;
        (, permissionsBitmask, flowRateAllowance) = _cfa.getFlowOperatorData(token, sender, flowOperator);
        allowCreate = permissionsBitmask & 1 == 1 ? true : false;
        allowUpdate = permissionsBitmask >> 1 & 1 == 1 ? true : false;
        allowDelete = permissionsBitmask >> 2 & 1 == 1 ? true : false;
    }

    /**************************************************************************
     * Internal functions
     *************************************************************************/

    function _setFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bool) {
        (, int96 prevFlowRate,,) = _cfa.getFlow(token, sender, receiver);

        if (flowRate > 0) {
            if (prevFlowRate == 0) {
                return _createFlow(token, sender, receiver, flowRate, userData);
            } else if (prevFlowRate != flowRate) {
                return _updateFlow(token, sender, receiver, flowRate, userData);
            } // else no change, do nothing
            return true;
        } else if (flowRate == 0) {
            if (prevFlowRate > 0) {
                return _deleteFlow(token, sender, receiver, userData);
            } // else no change, do nothing
            return true;
        } else {
            revert CFA_FWD_INVALID_FLOW_RATE();
        }
    }

    function _createFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    )
        internal returns (bool)
    {
        bytes memory cfaCallData = sender == msg.sender ?
            abi.encodeCall(
                _cfa.createFlow,
                (
                    token,
                    receiver,
                    flowRate,
                    new bytes(0) // ctx
                )
            ) :
            abi.encodeCall(
                _cfa.createFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowRate,
                    new bytes(0) // ctx
                )
            );

        return _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }

    function _updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    )
        internal returns (bool)
    {
        bytes memory cfaCallData = sender == msg.sender ?
            abi.encodeCall(
                _cfa.updateFlow,
                (
                    token,
                    receiver,
                    flowRate,
                    new bytes(0) // ctx
                )
            ) :
            abi.encodeCall(
                _cfa.updateFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowRate,
                    new bytes(0) // ctx
                )
            );

        return _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }

    function _deleteFlow(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory userData
    )
        internal returns (bool)
    {
        bytes memory cfaCallData = sender == msg.sender || receiver == msg.sender ?
            abi.encodeCall(
                _cfa.deleteFlow,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0) // ctx
                )
            ) :
            abi.encodeCall(
                _cfa.deleteFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0) // ctx
                )
            );

        return _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }

    function _updateFlowOperatorPermissions(
        ISuperToken token,
        address flowOperator,
        uint8 permissions,
        int96 flowRateAllowance
    )
        internal returns (bool)
    {
        bytes memory cfaCallData =
            abi.encodeCall(
                _cfa.updateFlowOperatorPermissions,
                (
                    token,
                    flowOperator,
                    permissions,
                    flowRateAllowance,
                    new bytes(0) // ctx
                )
            );

        return _forwardBatchCall(address(_cfa), cfaCallData, new bytes(0));
    }

    // compiles the calldata of a single operation for the host invocation and executes it
    function _forwardBatchCall(address target, bytes memory callData, bytes memory userData) internal returns (bool) {
        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](1);
        ops[0] = ISuperfluid.Operation(
            BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT, // type
            address(target), // target
            abi.encode( // data
                callData,
                userData
            )
        );

        bytes memory fwBatchCallData = abi.encodeCall(
            _host.forwardBatchCall,
            (
                ops
            )
        );

        // https://eips.ethereum.org/EIPS/eip-2771
        // we encode the msg.sender as the last 20 bytes per EIP-2771 to extract the original txn signer later on
        // solhint-disable-next-line avoid-low-level-calls
        (bool success, bytes memory returnedData) = address(_host).call(abi.encodePacked(fwBatchCallData, msg.sender));

        if (!success) {
            CallUtils.revertFromReturnedData(returnedData);
        }

        return true;
    }
}
