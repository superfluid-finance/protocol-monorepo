// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import {
    ISuperfluid,
    ISuperToken
} from "../superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "./IConstantFlowAgreementV1.sol";

/**
 * The CFAv1Forwarder contract provides an easy to use interface to
 * ConstantFlowAgreementV1 specific functionality of Super Tokens.
 * Instances of this contract can operate on the protocol only if configured as "trusted forwarder"
 * by protocol governance.
 * TODO unified terminology for deposit/buffer
 * TODO clarify flowrate type
 */

 interface ICFAv1Forwarder {
    /**
     * @notice Sets the given flowrate between msg.sender and a given receiver.
     * If there's no pre-existing flow and `flowrate` non-zero, a new flow is created.
     * If there's an existing flow and `flowrate` non-zero, the flowrate of that flow is updated.
     * If there's an existing flow and `flowrate` zero, the flow is deleted.
     * If the existing and given flowrate are equal, no action is taken.
     * On creation of a flow, a "buffer" amount is automatically detracted from the sender account's available token balance.
     * This buffer amount is redeemed upon deletion of that flow, provided that the sender account is then solvent (non-zero balance).
     * @param token Super token address
     * @param receiver The receiver of the flow
     * @param flowrate The flowrate of the flow denominated in wad/second. Supported values range from 0 to 2^95.
     */
    function setFlowrate(ISuperToken token, address receiver, uint256 flowrate) external;

    /** 
     * @notice Like `setFlowrate`, but can be invoked by an account with flowOperator permissions on behalf of the sender account.
     */
    function setFlowrateFrom(ISuperToken token, address sender, address receiver, uint256 flowrate) external;

    /**
     * @notice Get the flowrate of the flow between 2 accounts if exists.
     * @dev Currently, only 0 or 1 flows can exist between 2 accounts. This may change in the future.
     * @param token Super token address
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @return flowrate The flowrate from the sender to the receiver account. Returns 0 if no flow exists.
     */
    function getFlowrate(ISuperToken token, address sender, address receiver) external view
        returns(uint256 flowrate);

    /**
     * @notice Get all available information about a flow (if exists).
     * If only the flowrate is needed, consider using `getFlowrate` instead.
     * @param token Super token address
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @return lastUpdated Timestamp of last update (flowrate change) or zero if no flow exists
     * @return flowrate Current flowrate of the flow or zero if no flow exists
     * @return deposit Deposit amount locked. Returned to the the flow is deleted while solvent. TODO: rename to buffer?
     * @return owedDeposit TODO
     */
    function getFlowInfo(ISuperToken token, address sender, address receiver) external view
        returns(uint256 lastUpdated, uint256 flowrate, uint256 deposit, uint256 owedDeposit);

    /**
     * @notice Get the buffer amount required for the given token and flowrate.
     * This amount can vary based on the combination of token, flowrate and chain being queried.
     * The result for a given set of parameters can change over time, because it depends on governance configurable protocol parameters.
     * Changes of the required buffer amount affect only flows created or updated after the change.
     * @param token Super token address
     * @param flowrate The flowrate for which the buffer amount is calculated
     * @return bufferAmount The buffer amount required for the given configuration.
     */
    function getBufferAmountByFlowrate(ISuperToken token, uint256 flowrate) external
        returns (uint256 bufferAmount);


    /**
     * @notice Get the net flowrate of an account.
     * @param token Super token address
     * @param account Account to query
     * @return flowrate The net flowrate. Negative if the aggregate outgoing flowrate exceed the aggregate incoming flowrate.
     * TODO: should we really use int96 here?
     */
    function getAccountFlowrate(ISuperToken token, address account) external view
        returns (int96 flowrate);

    /**
     * @notice Get aggregated flow information (if any exist) of an account.
     * If only the net flowrate is needed, consider using `getAccountFlowrate` instead.
     * @param token Super token address
     * @param account Account to query
     * @return lastUpdated Timestamp of last update of a flow to or from the account (flowrate change)
     * @return flowrate Current net aggregate flowrate 
     * @return deposit Deposit amount locked. Returned to the the flow is deleted while solvent. TODO: rename to buffer?
     * @return owedDeposit TODO
     */
    function getAccountFlowInfo(ISuperToken token, address account) external view
        returns (uint256 lastUpdated, int96 flowrate, uint256 deposit, uint256 owedDeposit);

    /**
     * @notice Low-level wrapper of createFlow/createFlowByOperator.
     * If the address of msg.sender is not the same as the address of the `sender` argument,
     * createFlowByOperator is used internally. In this case msg.sender needs to have permission to create flows
     * on behalf of the given sender account with sufficient flowRateAllowance.
     * Currently, only 1 flow can exist between 2 accounts, thus attempts to invoke `createFlow` while a flow already exists will fail.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param flowrate The flowrate in wad/second to be set initially
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     */
    function createFlow(ISuperToken token, address sender, address receiver, int96 flowrate, bytes memory userData) external;
    
    /**
     * @notice Low-level wrapper if updateFlow/updateFlowByOperator.
     * If the address of msg.sender doesn't match the address of the `sender` argument,
     * updateFlowByOperator is invoked. In this case msg.sender needs to have permission to update flows
     * on behalf of the given sender account with sufficient flowRateAllowance.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param flowrate The flowrate in wad/second the flow should be updated to
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     */
    function updateFlow(ISuperToken token, address sender, address receiver, int96 flowrate, bytes memory userData) external;

    /**
     * @notice Low-level wrapper of deleteFlow/deleteFlowByOperator.
     * If msg.sender isn't the same as sender address, msg.sender needs to have permission 
     * to delete flows on behalf of the given sender account.
     * @param token Super token address
     * @param sender Sender address of the flow
     * @param receiver Receiver address of the flow
     * @param userData (optional) User data to be set. Should be set to zero if not needed.
     */
    function deleteFlow(ISuperToken token, address sender, address receiver, bytes memory userData) external;

    /**
     * @notice Grants a flowOperator permission to create/update/delete flows on behalf of msg.sender.
     * In order to restrict what a flowOperator can or can't do, the flowOperator account
     * should be a contract implementing the desired restrictions.
     * @param token Super token address
     * @param flowOperator Account to which permissions are granted
     */
    function grantPermissions(ISuperToken token, address flowOperator) external;

    /**
     * @notice Revokes all permissions previously granted to a flowOperator by msg.sender.
     * Revocation doesn't undo or reset flows previously created/updated by the flowOperator.
     * In order to be sure about the state of flows at the time of revocation, you need to check that state 
     * either in the same transaction or after this transaction.
     * @param token Super token address
     * @param flowOperator Account from which permissions are revoked
     */
    function revokePermissions(ISuperToken token, address flowOperator) external;
    
    /**
     * @notice Low-level wrapper of `IConstantFlowAgreementV1.updateFlowOperatorPermissions`
     * @param token Super token address
     * @param flowOperator Account for which permissions are set on behalf of msg.sender
     * @param permissions Bitmask for create/update/delete permission flags. See library `FlowOperatorDefinitions`
     * @param flowrateAllowance Max. flowrate in wad/second the operator can set for individual flows.
     * @notice flowrateAllowance does NOT restrict the net flowrate a flowOperator is able to set.
     * In order to restrict that, flowOperator needs to be a contract implementing the wanted limitations.
     */
    function updateFlowOperatorPermissions(ISuperToken token, address flowOperator, uint8 permissions, int96 flowrateAllowance) external;

    /**
     * @notice Get the currently set permissions granted to the given flowOperator by the given sender account.
     * @param token Super token address
     * @param sender The account which (possiby) granted permissions
     * @param flowOperator Account to which (possibly) permissions were granted
     * @return permissions A bitmask of the permissions currently granted (or not) by `sender` to `flowOperator`
     * @return flowrateAllowance Max. flowrate in wad/second the flowOperator can set for individual flows.
     */
    function getFlowOperatorPermissions(ISuperToken token, address sender, address flowOperator) external view
        returns (uint8 permissions, int96 flowrateAllowance);
 }