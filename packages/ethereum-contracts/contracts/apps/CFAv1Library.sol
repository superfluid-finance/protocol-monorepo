// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import {
    ISuperfluid,
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

/**
 * @title Constant flow agreement v1 library
 * @author Superfluid
 * @dev for working with the constant flow agreement within solidity
 * @dev the first set of functions are each for callAgreement()
 * @dev the second set of functions are each for use in callAgreementWithContext()
 */
library CFAv1Library {

    /**
     * @dev Initialization data
     * @param host Superfluid host for calling agreements
     * @param cfa Constant Flow Agreement contract
     */
    struct InitData {
        ISuperfluid host;
        IConstantFlowAgreementV1 cfa;
    }

    /**
     * @dev Create flow without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     */
    function createFlow(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) internal {
        createFlow(cfaLibrary, token, receiver, flowRate, new bytes(0));
    }

    /**
     * @dev Create flow with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function createFlow(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.createFlow,
                (
                    token,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            userData
        );
    }

    /**
     * @dev Update flow without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     */
    function updateFlow(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) internal {
        updateFlow(cfaLibrary, token, receiver, flowRate, new bytes(0));
    }

    /**
     * @dev Update flow with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function updateFlow(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.updateFlow,
                (
                    token,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            userData
        );
    }

    /**
     * @dev Delete flow without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     */
    function deleteFlow(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver
    ) internal {
        deleteFlow(cfaLibrary, token, sender, receiver, new bytes(0));
    }

    /**
     * @dev Delete flow with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param userData The user provided data
     */
    function deleteFlow(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.deleteFlow,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0) // placeholder
                )
            ),
            userData
        );
    }

    /**
     * @dev Create flow with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     */
    function createFlowWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.createFlow,
                (
                    token,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            new bytes(0), // userData
            ctx
        );
    }

    /**
     * @dev Update flow with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     */
    function updateFlowWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.updateFlow,
                (
                    token,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
                new bytes(0), // userData
            ctx
        );
    }

    /**
     * @dev Delete flow with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     */
    function deleteFlowWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.deleteFlow,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0) // placeholder
                )
            ),
            new bytes(0), // userData
            ctx
        );
    }

    /**
     * @dev Creates flow as an operator without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     */
    function createFlowByOperator(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        int96 flowRate
    ) internal {
        createFlowByOperator(cfaLibrary, token, sender, receiver, flowRate, new bytes(0));
    }

    /**
     * @dev Creates flow as an operator with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function createFlowByOperator(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.createFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            userData
        );
    }

    /**
     * @dev Creates flow as an operator with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     */
    function createFlowByOperatorWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.createFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            new bytes(0), // userData
            ctx
        );
    }

    /**
     * @dev Updates a flow as an operator without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     */
    function updateFlowByOperator(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        int96 flowRate
    ) internal {
        updateFlowByOperator(cfaLibrary, token, sender, receiver, flowRate, new bytes(0));
    }

    /**
     * @dev Updates flow as an operator with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function updateFlowByOperator(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.updateFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowRate,
                    new bytes(0)
                )
            ),
            userData
        );
    }

    /**
     * @dev Updates a flow as an operator with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     */
    function updateFlowByOperatorWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.updateFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowRate,
                    new bytes(0)
                )
            ),
            new bytes(0), // userData
            ctx
        );
    }

    /**
     * @dev Deletes a flow as an operator without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     */
    function deleteFlowByOperator(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver
    ) internal {
        deleteFlowByOperator(cfaLibrary, token, sender, receiver, new bytes(0));
    }

    /**
     * @dev Deletes a flow as an operator with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param userData The user provided data
     */
    function deleteFlowByOperator(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.deleteFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0)
                )
            ),
            userData
        );
    }

    /**
     * @dev Deletes a flow as an operator with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     */
    function deleteFlowByOperatorWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address sender,
        address receiver,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.deleteFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0)
                )
            ),
            new bytes(0), // userData
            ctx
        );
    }

    /**
     * @dev Updates the permissions of a flow operator
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token of flows handled by the operator
     * @param flowOperator The operator that can create/update/delete flows
     * @param permissions The number of the permissions: create = 1; update = 2; delete = 4;
     * To give multiple permissions, sum the above. create_delete = 5; create_update_delete = 7; etc
     * @param flowRateAllowance The allowance for flow creation. Decremented as flowRate increases
     */
    function updateFlowOperatorPermissions(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address flowOperator,
        uint8 permissions,
        int96 flowRateAllowance
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.updateFlowOperatorPermissions,
                (
                    token,
                    flowOperator,
                    permissions,
                    flowRateAllowance,
                    new bytes(0)
                )
            ),
            new bytes(0)
        );
    }

    /**
     * @dev Updates the permissions of a flow operator with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token of flows handled by the operator
     * @param flowOperator The operator that can create/update/delete flows
     * @param permissions The number of the permissions: create = 1; update = 2; delete = 4;
     * To give multiple permissions, sum the above. create_delete = 5; create_update_delete = 7; etc
     * @param flowRateAllowance The allowance for flow creation. Decremented as flowRate increases
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     */
    function updateFlowOperatorPermissionsWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address flowOperator,
        uint8 permissions,
        int96 flowRateAllowance,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.updateFlowOperatorPermissions,
                (
                    token,
                    flowOperator,
                    permissions,
                    flowRateAllowance,
                    new bytes(0)
                )
            ),
            new bytes(0),
            ctx
        );
    }

    /**
     * @dev Grants full, unlimited permission to a flow operator
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token of flows handled by the operator
     * @param flowOperator The operator that can create/update/delete flows
     */
    function authorizeFlowOperatorWithFullControl(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address flowOperator
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.authorizeFlowOperatorWithFullControl,
                (
                    token,
                    flowOperator,
                    new bytes(0)
                )
            ),
            new bytes(0)
        );
    }

    /**
     * @dev Grants full, unlimited permission to a flow operator with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token of flows handled by the operator
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param flowOperator The operator that can create/update/delete flows
     */
    function authorizeFlowOperatorWithFullControlWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address flowOperator,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.authorizeFlowOperatorWithFullControl,
                (
                    token,
                    flowOperator,
                    new bytes(0)
                )
            ),
            new bytes(0),
            ctx
        );
    }

    /**
     * @dev Revokes all permissions from a flow operator
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token of flows handled by the operator
     * @param flowOperator The operator that can create/update/delete flows
     */
    function revokeFlowOperatorWithFullControl(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address flowOperator
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.revokeFlowOperatorWithFullControl,
                (
                    token,
                    flowOperator,
                    new bytes(0)
                )
            ),
            new bytes(0)
        );
    }

    /**
     * @dev Revokes all permissions from a flow operator
     * @param cfaLibrary The cfaLibrary storage variable
     * @param token The token of flows handled by the operator
     * @param flowOperator The operator that can create/update/delete flows
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     */
    function revokeFlowOperatorWithFullControlWithCtx(
        InitData storage cfaLibrary,
        ISuperfluidToken token,
        address flowOperator,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeCall(
                cfaLibrary.cfa.revokeFlowOperatorWithFullControl,
                (
                    token,
                    flowOperator,
                    new bytes(0)
                )
            ),
            new bytes(0),
            ctx
        );
    }
}
