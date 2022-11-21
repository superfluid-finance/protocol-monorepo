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
contract CFAv1Forwarder {
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
     * @notice Sets the given flowrate between msg.sender and a given receiver.
     * If there's no pre-existing flow and `flowrate` non-zero, a new flow is created.
     * If there's an existing flow and `flowrate` non-zero, the flowrate of that flow is updated.
     * If there's an existing flow and `flowrate` zero, the flow is deleted.
     * If the existing and given flowrate are equal, no action is taken.
     * On creation of a flow, a "buffer" amount is automatically detracted from the sender account's available balance.
     * If the sender account is solvent when the flow is deleted, this buffer is redeemed to it.
     * @param token Super token address
     * @param receiver The receiver of the flow
     * @param flowrate The wanted flowrate in wad/second. Only positive values are valid here.
     * @return bool
     */
    function setFlow(ISuperToken token, address receiver, int96 flowrate) external returns (bool) {
       return _setFlowFrom(token, msg.sender, receiver, flowrate, new bytes(0), new bytes(0));
    }

    // overloaded: with userData
    function setFlow(ISuperToken token, address receiver, int96 flowrate, bytes memory userData)
        external returns (bool)
    {
        return _setFlowFrom(token, msg.sender, receiver, flowrate, userData, new bytes(0));
    }

    // overloaded: with userData and ctx
    function setFlow(ISuperToken token, address receiver, int96 flowrate, bytes memory userData, bytes memory ctx)
        external returns (bool)
    {
        return _setFlowFrom(token, msg.sender, receiver, flowrate, userData, ctx);
    }

    /**
     * @notice Like `setFlowrate`, but can be invoked by an account with flowOperator permissions
     * on behalf of the sender account.
     * @param token Super token address
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowrate The wanted flowrate in wad/second. Only positive values are valid here.
     * @return bool
     */
    function setFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowrate
    ) external returns (bool) {
        return _setFlowFrom(token, sender, receiver, flowrate, new bytes(0), new bytes(0));
    }

    // overloaded: with userData
    function setFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowrate,
        bytes memory userData
    ) external returns (bool) {
        return _setFlowFrom(token, sender, receiver, flowrate, userData, new bytes(0));
    }

    // overloaded: with ctx
    function setFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowrate,
        bytes memory userData,
        bytes memory ctx
    ) external returns (bool) {
        return _setFlowFrom(token, sender, receiver, flowrate, userData, ctx);
    }

    /*
     * @notice Allow to increase or decrease the flowrate by a given delta
     * If msg.sender isn't the same as sender address, msg.sender needs to have permission
     * to use the needed operation (create/update/delete flow) on behalf of the given sender account.
     * The needed operation is determined by the previous and resulting flowrate.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param flowrateDelta If > 0, the flowrate will be increased, if < 0, it will be decreased.
     * Note that the call will fail if the resulting new flowrate would be negative!
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     * @param ctx (optional) To be set only when called from a SuperApp
     * @return newFlowrate The flowrate after applying the delta
     */
    function applyFlowrateDelta(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowrateDelta,
        bytes memory userData,
        bytes memory ctx
    ) external returns(int96 newFlowrate) {
        (, int96 oldFlowrate, , ) = _cfa.getFlow(token, sender, receiver);
        int96 uncappedNewFlowrate = oldFlowrate + flowrateDelta; // can be negative
        if (uncappedNewFlowrate < 0) revert CFA_FWD_INVALID_FLOW_RATE();
        newFlowrate = 0 > uncappedNewFlowrate ? int96(0) : uncappedNewFlowrate;
        _setFlowFrom(token, sender, receiver, newFlowrate, userData, ctx);
    }

    /**
     * @notice Get the flowrate of the flow between 2 accounts if exists.
     * @dev Currently, only 0 or 1 flows can exist between 2 accounts. This may change in the future.
     * @param token Super token address
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @return flowrate The flowrate from the sender to the receiver account. Returns 0 if no flow exists.
     */
    function getFlowrate(ISuperToken token, address sender, address receiver) external view
        returns(int96 flowrate)
    {
        (, flowrate, , ) = _cfa.getFlow(token, sender, receiver);
    }

    /**
     * @notice Get all available information about a flow (if exists).
     * If only the flowrate is needed, consider using `getFlowrate` instead.
     * @param token Super token address
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @return lastUpdated Timestamp of last update (flowrate change) or zero if no flow exists
     * @return flowrate Current flowrate of the flow or zero if no flow exists
     * @return deposit Deposit amount locked as security buffer during the lifetime of the flow
     * @return owedDeposit Extra deposit amount borrowed to a SuperApp receiver by the flow sender
     */
    function getFlowInfo(ISuperToken token, address sender, address receiver) external view
        returns(uint256 lastUpdated, int96 flowrate, uint256 deposit, uint256 owedDeposit)
    {
        (lastUpdated, flowrate, deposit, owedDeposit) = _cfa.getFlow(token, sender, receiver);
    }

    /**
     * @notice Get the net flowrate of an account.
     * @param token Super token address
     * @param account Account to query
     * @return flowrate The net flowrate (aggregate incoming minus aggregate outgoing flowrate), can be negative.
     */
    function getNetFlowrate(ISuperToken token, address account) external view
        returns (int96 flowrate)
    {
        return _cfa.getNetFlow(token, account);
    }

    /**
     * @notice Get information about the net flow (if any flow exists) of an account.
     * If only the net flowrate is needed, consider using `getAccountFlowrate` instead.
     * @param token Super token address
     * @param account Account to query
     * @return lastUpdated Timestamp of last update of a flow to or from the account (flowrate change)
     * @return flowrate Current net aggregate flowrate
     * @return deposit Aggregate deposit amount currently locked as security buffer for outgoing flows
     * @return owedDeposit Aggregate extra deposit amount currently borrowed to SuperApps receiving from this account
     */
    function getNetFlowInfo(ISuperToken token, address account) external view
        returns (uint256 lastUpdated, int96 flowrate, uint256 deposit, uint256 owedDeposit)
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
     * @param flowrate The flowrate for which the buffer amount is calculated
     * @return bufferAmount The buffer amount required for the given configuration.
     */
    function getBufferAmountByFlowrate(ISuperToken token, int96 flowrate) external view
        returns (uint256 bufferAmount)
    {
        return _cfa.getDepositRequiredForFlowRate(token, flowrate);
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
     * @param flowrate The flowrate in wad/second to be set initially
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     * @param ctx (optional) To be set only when called from a SuperApp
     * @return bool
     */
    function createFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowrate,
        bytes memory userData,
        bytes memory ctx
    )
        external returns (bool)
    {
        return _createFlow(token, sender, receiver, flowrate, userData, ctx);
    }

    /**
     * @notice Wrapper of updateFlow/updateFlowByOperator.
     * If the address of msg.sender doesn't match the address of the `sender` argument,
     * updateFlowByOperator is invoked. In this case msg.sender needs to have permission to update flows
     * on behalf of the given sender account with sufficient flowRateAllowance.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param flowrate The flowrate in wad/second the flow should be updated to
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     * @param ctx (optional) To be set only when called from a SuperApp
     * @return bool
     */
    function updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowrate,
        bytes memory userData,
        bytes memory ctx
    )
        external returns (bool)
    {
        return _updateFlow(token, sender, receiver, flowrate, userData, ctx);
    }

    /**
     * @notice Wrapper of deleteFlow/deleteFlowByOperator.
     * If msg.sender isn't the same as sender address, msg.sender needs to have permission
     * to delete flows on behalf of the given sender account.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     * @param ctx (optional) To be set only when called from a SuperApp
     * @return bool
     */
    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory userData,
        bytes memory ctx
    )
        external returns (bool)
    {
        return _deleteFlow(token, sender, receiver, userData, ctx);
    }

    /**
     * @notice Sets the permissions for a specific flowOperator
     * In order to restrict what a flowOperator can or can't do, the flowOperator account
     * should be a contract implementing the desired restrictions.
     * @param token Super token address
     * @param flowOperator Account to which permissions are granted
     * @param permissionsBitmask see `getPermissionsBitmask`. Set to 0 in order to revoke all permissions.
     * @param flowrateAllowance Max. flowrate in wad/second the operator can set for individual flows.
     * @return bool
     */
    function setFlowPermissions(
        ISuperToken token,
        address flowOperator,
        bool allowCreate,
        bool allowUpdate,
        bool allowDelete,
        int96 flowrateAllowance
    )
        external returns (bool)
    {
        uint8 permissionsBitmask = (allowCreate ? 1 : 0)
            | (allowUpdate ? 1 : 0) << 1
            | (allowDelete ? 1 : 0) << 2;
        return _updateFlowOperatorPermissions(
            token,
            flowOperator,
            permissionsBitmask,
            flowrateAllowance,
            new bytes(0)
        );
    }

    /**
     * @notice Get the currently set permissions granted to the given flowOperator by the given sender account.
     * @param token Super token address
     * @param sender The account which (possiby) granted permissions
     * @param flowOperator Account to which (possibly) permissions were granted
     * @return permissions A bitmask of the permissions currently granted (or not) by `sender` to `flowOperator`
     * @return flowrateAllowance Max. flowrate in wad/second the flowOperator can set for individual flows.
     */
    function getFlowPermissions(ISuperToken token, address sender, address flowOperator) external view
        returns (bool allowCreate, bool allowUpdate, bool allowDelete, int96 flowrateAllowance)
    {
        uint8 permissionsBitmask;
        (, permissionsBitmask, flowrateAllowance) = _cfa.getFlowOperatorData(token, sender, flowOperator);
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
        int96 flowrate,
        bytes memory userData,
        bytes memory ctx
    ) internal returns (bool) {
        (, int96 prevFlowRate,,) = _cfa.getFlow(token, sender, receiver);

        if (flowrate > 0) {
            if (prevFlowRate == 0) {
                return _createFlow(token, sender, receiver, flowrate, userData, ctx);
            } else if (prevFlowRate != flowrate) {
                return _updateFlow(token, sender, receiver, flowrate, userData, ctx);
            } // else no change, do nothing
            return true;
        } else if (flowrate == 0) {
            if (prevFlowRate > 0) {
                return _deleteFlow(token, sender, receiver, userData, ctx);
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
        int96 flowrate,
        bytes memory userData,
        bytes memory ctx
    )
        internal returns (bool)
    {
        bytes memory cfaCallData = sender == msg.sender ?
            abi.encodeCall(
                _cfa.createFlow,
                (
                    token,
                    receiver,
                    flowrate,
                    ctx
                )
            ) :
            abi.encodeCall(
                _cfa.createFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowrate,
                    ctx
                )
            );

        return _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }

    function _updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowrate,
        bytes memory userData,
        bytes memory ctx
    )
        internal returns (bool)
    {
        bytes memory cfaCallData = sender == msg.sender ?
            abi.encodeCall(
                _cfa.updateFlow,
                (
                    token,
                    receiver,
                    flowrate,
                    ctx
                )
            ) :
            abi.encodeCall(
                _cfa.updateFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowrate,
                    ctx
                )
            );

        return _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }

    function _deleteFlow(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory userData,
        bytes memory ctx
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
                    ctx
                )
            ) :
            abi.encodeCall(
                _cfa.deleteFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    ctx
                )
            );

        return _forwardBatchCall(address(_cfa), cfaCallData, userData);
    }

    function _updateFlowOperatorPermissions(
        ISuperToken token,
        address flowOperator,
        uint8 permissions,
        int96 flowrateAllowance,
        bytes memory ctx
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
                    flowrateAllowance,
                    ctx
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
