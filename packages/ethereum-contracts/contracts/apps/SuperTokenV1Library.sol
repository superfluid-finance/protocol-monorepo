// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.11;

import {
    ISuperfluid,
    ISuperToken,
    IConstantFlowAgreementV1,
    IInstantDistributionAgreementV1
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IGeneralDistributionAgreementV1,
    ISuperfluidPool,
    PoolConfig
} from "../interfaces/agreements/gdav1/IGeneralDistributionAgreementV1.sol";

/**
 * @title Library for Token Centric Interface
 * @author Superfluid
 * @dev Set `using for ISuperToken` in including file, and call any of these functions on an instance
 * of ISuperToken.
 * Note that it is important to "warm up" the cache and cache the host, cfa, ida before calling,
 * this is only applicable to Foundry tests where the vm.expectRevert() will not work as expected.
 * You must use vm.startPrank(account) instead of vm.prank when executing functions if the cache
 * isn't "warmed up" yet. vm.prank impersonates the account only for the first call, which will be
 * used for caching.
 */
library SuperTokenV1Library {
    /** CFA BASE CRUD ************************************* */

    /**
     * @dev Create flow without userData
     * @param token The token used in flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     */
    function createFlow(ISuperToken token, address receiver, int96 flowRate)
        internal returns (bool)
    {
        return createFlow(token, receiver, flowRate, new bytes(0));
    }

    /**
     * @dev Create flow with userData
     * @param token The token used in flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param userData The userdata passed along with call
     */
    function createFlow(ISuperToken token, address receiver, int96 flowRate, bytes memory userData)
        internal returns (bool)
    {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.createFlow,
                (token, receiver, flowRate, new bytes(0))
            ),
            userData // userData
        );
        return true;
    }


    /**
     * @dev Update flow without userData
     * @param token The token used in flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     */
    function updateFlow(ISuperToken token, address receiver, int96 flowRate)
        internal returns (bool)
    {
        return updateFlow(token, receiver, flowRate, new bytes(0));
    }


    /**
     * @dev Update flow with userData
     * @param token The token used in flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param userData The userdata passed along with call
     */
    function updateFlow(ISuperToken token, address receiver, int96 flowRate, bytes memory userData)
        internal returns (bool)
    {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.updateFlow,
                (token, receiver, flowRate, new bytes(0))
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Delete flow without userData
     * @param token The token used in flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     */
    function deleteFlow(ISuperToken token, address sender, address receiver)
        internal returns (bool)
    {
        return deleteFlow(token, sender, receiver, new bytes(0));
    }

    /**
     * @dev Delete flow with userData
     * @param token The token used in flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param userData The userdata passed along with call
     */
    function deleteFlow(ISuperToken token, address sender, address receiver, bytes memory userData)
        internal returns (bool)
    {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.deleteFlow,
                (token, sender, receiver, new bytes(0))
            ),
            userData
        );
        return true;
    }

    /** CFA ACL ************************************* */

    /**
     * @dev Update permissions for flow operator
     * @param token The token used in flow
     * @param flowOperator The address given flow permissions
     * @param allowCreate creation permissions
     * @param allowCreate update permissions
     * @param allowCreate deletion permissions
     * @param flowRateAllowance The allowance provided to flowOperator
     */
    function setFlowPermissions(
        ISuperToken token,
        address flowOperator,
        bool allowCreate,
        bool allowUpdate,
        bool allowDelete,
        int96 flowRateAllowance
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        uint8 permissionsBitmask = (allowCreate ? 1 : 0)
            | (allowUpdate ? 1 : 0) << 1
            | (allowDelete ? 1 : 0) << 2;
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.updateFlowOperatorPermissions,
                (token, flowOperator, permissionsBitmask, flowRateAllowance, new bytes(0))
            ),
            new bytes(0)
        );
        return true;
    }

    /**
     * @dev Update permissions for flow operator - give operator max permissions
     * @param token The token used in flow
     * @param flowOperator The address given flow permissions
     */
    function setMaxFlowPermissions(
        ISuperToken token,
        address flowOperator
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.authorizeFlowOperatorWithFullControl,
                (token, flowOperator, new bytes(0))
            ),
            new bytes(0)
        );
        return true;
    }

    /**
     * @dev Update permissions for flow operator - revoke all permission
     * @param token The token used in flow
     * @param flowOperator The address given flow permissions
     */
    function revokeFlowPermissions(
        ISuperToken token,
        address flowOperator
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.revokeFlowOperatorWithFullControl,
                (token, flowOperator, new bytes(0))
            ),
            new bytes(0)
        );
        return true;
    }

    /**
     * @dev Increases the flow rate allowance for flow operator
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address whose flow rate allowance is increased
     * @param addedFlowRateAllowance amount to increase allowance by
     */
    function increaseFlowRateAllowance(ISuperToken token, address flowOperator, int96 addedFlowRateAllowance)
        internal
        returns (bool)
    {
        return increaseFlowRateAllowance(token, flowOperator, addedFlowRateAllowance, new bytes(0));
    }

    /**
     * @dev Increases the flow rate allowance for flow operator
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address whose flow rate allowance is increased
     * @param addedFlowRateAllowance amount to increase allowance by
     * @param userData The userdata passed along with call
     */
    function increaseFlowRateAllowance(
        ISuperToken token,
        address flowOperator,
        int96 addedFlowRateAllowance,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(cfa.increaseFlowRateAllowance, (token, flowOperator, addedFlowRateAllowance, new bytes(0))),
            userData
        );
        return true;
    }

    /**
     * @dev Decreases the flow rate allowance for flow operator
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address whose flow rate allowance is decreased
     * @param subtractedFlowRateAllowance amount to decrease allowance by
     */
    function decreaseFlowRateAllowance(ISuperToken token, address flowOperator, int96 subtractedFlowRateAllowance)
        internal
        returns (bool)
    {
        return decreaseFlowRateAllowance(token, flowOperator, subtractedFlowRateAllowance, new bytes(0));
    }

    /**
     * @dev Decreases the flow rate allowance for flow operator
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address whose flow rate allowance is decreased
     * @param subtractedFlowRateAllowance amount to decrease allowance by
     * @param userData The userdata passed along with call
     */
    function decreaseFlowRateAllowance(
        ISuperToken token,
        address flowOperator,
        int96 subtractedFlowRateAllowance,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.decreaseFlowRateAllowance, (token, flowOperator, subtractedFlowRateAllowance, new bytes(0))
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Increases the flow rate allowance for flow operator and adds the permissions
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address whose flow rate allowance is increased
     * @param permissionsToAdd The permissions to add for the flow operator
     * @param addedFlowRateAllowance amount to increase allowance by
     */
    function increaseFlowRateAllowanceWithPermissions(
        ISuperToken token,
        address flowOperator,
        uint8 permissionsToAdd,
        int96 addedFlowRateAllowance
    ) internal returns (bool) {
        return
            increaseFlowRateAllowanceWithPermissions(
                token,
                flowOperator,
                permissionsToAdd,
                addedFlowRateAllowance,
                new bytes(0)
            );
    }

    /**
     * @dev Increases the flow rate allowance for flow operator and adds the permissions
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address whose flow rate allowance is increased
     * @param permissionsToAdd The permissions to add for the flow operator
     * @param addedFlowRateAllowance amount to increase allowance by
     * @param userData The userdata passed along with call
     */
    function increaseFlowRateAllowanceWithPermissions(
        ISuperToken token,
        address flowOperator,
        uint8 permissionsToAdd,
        int96 addedFlowRateAllowance,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.increaseFlowRateAllowanceWithPermissions,
                (token, flowOperator, permissionsToAdd, addedFlowRateAllowance, new bytes(0))
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Decreases the flow rate allowance for flow operator and removes the permissions
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address whose flow rate allowance is subtracted
     * @param permissionsToRemove The permissions to remove for the flow operator
     * @param subtractedFlowRateAllowance amount to subtract allowance by
     */
    function decreaseFlowRateAllowanceWithPermissions(
        ISuperToken token,
        address flowOperator,
        uint8 permissionsToRemove,
        int96 subtractedFlowRateAllowance
    ) internal returns (bool) {
        return decreaseFlowRateAllowanceWithPermissions(
            token, flowOperator, permissionsToRemove, subtractedFlowRateAllowance, new bytes(0)
        );
    }

    /**
     * @dev Decreases the flow rate allowance for flow operator and removes the permissions
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address whose flow rate allowance is subtracted
     * @param permissionsToRemove The permissions to remove for the flow operator
     * @param subtractedFlowRateAllowance amount to subtract allowance by
     * @param userData The userdata passed along with call
     */
    function decreaseFlowRateAllowanceWithPermissions(
        ISuperToken token,
        address flowOperator,
        uint8 permissionsToRemove,
        int96 subtractedFlowRateAllowance,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.decreaseFlowRateAllowanceWithPermissions,
                (token, flowOperator, permissionsToRemove, subtractedFlowRateAllowance, new bytes(0))
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Update permissions for flow operator in callback
     * @notice allowing userData to be a parameter here triggered stack too deep error
     * @param token The token used in flow
     * @param flowOperator The address given flow permissions
     * @param allowCreate creation permissions
     * @param allowCreate update permissions
     * @param allowCreate deletion permissions
     * @param flowRateAllowance The allowance provided to flowOperator
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function setFlowPermissionsWithCtx(
        ISuperToken token,
        address flowOperator,
        bool allowCreate,
        bool allowUpdate,
        bool allowDelete,
        int96 flowRateAllowance,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        uint8 permissionsBitmask = (allowCreate ? 1 : 0)
            | (allowUpdate ? 1 : 0) << 1
            | (allowDelete ? 1 : 0) << 2;
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.updateFlowOperatorPermissions,
                (
                    token,
                    flowOperator,
                    permissionsBitmask,
                    flowRateAllowance,
                    new bytes(0)
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Update permissions for flow operator - give operator max permissions
     * @param token The token used in flow
     * @param flowOperator The address given flow permissions
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function setMaxFlowPermissionsWithCtx(
        ISuperToken token,
        address flowOperator,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.authorizeFlowOperatorWithFullControl,
                (
                    token,
                    flowOperator,
                    new bytes(0)
                )
            ),
            "0x",
            ctx
        );
    }

    /**
    * @dev Update permissions for flow operator - revoke all permission
     * @param token The token used in flow
     * @param flowOperator The address given flow permissions
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function revokeFlowPermissionsWithCtx(
        ISuperToken token,
        address flowOperator,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.revokeFlowOperatorWithFullControl,
                (token, flowOperator, new bytes(0))
            ),
            "0x",
            ctx
        );
    }


    /**
     * @dev Creates flow as an operator without userData
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     */
    function createFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate
    ) internal returns (bool) {
        return createFlowFrom(token, sender, receiver, flowRate, new bytes(0));
    }

    /**
     * @dev Creates flow as an operator with userData
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function createFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.createFlowByOperator,
                (token, sender, receiver, flowRate, new bytes(0))
            ),
            userData
        );
        return true;
    }


    /**
     * @dev Updates flow as an operator without userData
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     */
    function updateFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate
    ) internal returns (bool) {
        return updateFlowFrom(token, sender, receiver, flowRate, new bytes(0));
    }

    /**
     * @dev Updates flow as an operator with userData
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function updateFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.updateFlowByOperator,
                (token, sender, receiver, flowRate, new bytes(0))
            ),
            userData
        );
        return true;
    }

     /**
     * @dev Deletes flow as an operator without userData
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     */
    function deleteFlowFrom(
        ISuperToken token,
        address sender,
        address receiver
    ) internal returns (bool) {
        return deleteFlowFrom(token, sender, receiver, new bytes(0));
    }

    /**
     * @dev Deletes flow as an operator with userData
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param userData The user provided data
     */
    function deleteFlowFrom(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        host.callAgreement(
            cfa,
            abi.encodeCall(
                cfa.deleteFlowByOperator,
                (token, sender, receiver, new bytes(0))
            ),
            userData
        );
        return true;
    }


    /** CFA With CTX FUNCTIONS ************************************* */

    /**
     * @dev Create flow with context and userData
     * @param token The token to flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function createFlowWithCtx(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.createFlow,
                (
                    token,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Create flow by operator with context
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function createFlowFromWithCtx(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.createFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Update flow with context
     * @param token The token to flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function updateFlowWithCtx(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.updateFlow,
                (
                    token,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Update flow by operator with context
     * @param token The token to flow
     * @param sender The receiver of the flow
     * @param receiver The receiver of the flow
     * @param flowRate The desired flowRate
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function updateFlowFromWithCtx(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.updateFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    flowRate,
                    new bytes(0) // placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Delete flow with context
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function deleteFlowWithCtx(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.deleteFlow,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0) // placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Delete flow by operator with context
     * @param token The token to flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function deleteFlowFromWithCtx(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IConstantFlowAgreementV1 cfa) = _getAndCacheHostAndCFA(token);
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeCall(
                cfa.deleteFlowByOperator,
                (
                    token,
                    sender,
                    receiver,
                    new bytes(0) // placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /** CFA VIEW FUNCTIONS ************************************* */

    /**
     * @dev get flow rate between two accounts for given token
     * @param token The token used in flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @return flowRate The flow rate
     */
    function getFlowRate(ISuperToken token, address sender, address receiver)
        internal view returns(int96 flowRate)
    {
        (, IConstantFlowAgreementV1 cfa) = _getHostAndCFA(token);
        (, flowRate, , ) = cfa.getFlow(token, sender, receiver);
    }

    /**
     * @dev get flow info between two accounts for given token
     * @param token The token used in flow
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @return lastUpdated Timestamp of flow creation or last flowrate change
     * @return flowRate The flow rate
     * @return deposit The amount of deposit the flow
     * @return owedDeposit The amount of owed deposit of the flow
     */
    function getFlowInfo(ISuperToken token, address sender, address receiver)
        internal view
        returns(uint256 lastUpdated, int96 flowRate, uint256 deposit, uint256 owedDeposit)
    {
        (, IConstantFlowAgreementV1 cfa) = _getHostAndCFA(token);
        (lastUpdated, flowRate, deposit, owedDeposit) = cfa.getFlow(token, sender, receiver);
    }

    /**
     * @dev get flow info of a distributor to a pool for given token
     * @param token The token used in flow
     * @param distributor The sitributor of the flow
     * @param pool The GDA pool
     * @return lastUpdated Timestamp of flow creation or last flowrate change
     * @return flowRate The flow rate
     * @return deposit The amount of deposit the flow
     */
    function getGDAFlowInfo(ISuperToken token, address distributor, ISuperfluidPool pool)
        internal view
        returns(uint256 lastUpdated, int96 flowRate, uint256 deposit)
    {
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        return gda.getFlow(token, distributor, pool);
    }

    /* function getGDAFlowInfo(ISuperToken token, address distributor, ISuperfluidPool pool) */
    /* { */
    /* } */

    /**
     * @dev get net flow rate for given account for given token (CFA + GDA)
     * @param token Super token address
     * @param account Account to query
     * @return flowRate The net flow rate of the account
     */
    function getNetFlowRate(ISuperToken token, address account)
        internal view returns (int96 flowRate)
    {
        (, IConstantFlowAgreementV1 cfa) = _getHostAndCFA(token);
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        int96 cfaNetFlow = cfa.getNetFlow(token, account);
        int96 gdaNetFlow = gda.getNetFlow(token, account);
        return cfaNetFlow + gdaNetFlow;
    }

    /**
     * @dev get CFA net flow rate for given account for given token
     * @param token Super token address
     * @param account Account to query
     * @return flowRate The net flow rate of the account
     */
    function getCFANetFlowRate(ISuperToken token, address account)
        internal view returns (int96 flowRate)
    {
        (, IConstantFlowAgreementV1 cfa) = _getHostAndCFA(token);
        return cfa.getNetFlow(token, account);
    }

    /**
     * @dev get GDA net flow rate for given account for given token
     * @param token Super token address
     * @param account Account to query
     * @return flowRate The net flow rate of the account
     */
    function getGDANetFlowRate(ISuperToken token, address account)
        internal view returns (int96 flowRate)
    {
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        return gda.getNetFlow(token, account);
    }

    /**
     * @dev get the aggregated flow info of the account (CFA + GDA)
     * @param token Super token address
     * @param account Account to query
     * @return lastUpdated Timestamp of the last change of the net flow
     * @return flowRate The net flow rate of token for account
     * @return deposit The sum of all deposits for account's flows
     * @return owedDeposit The sum of all owed deposits for account's flows
     */
    function getNetFlowInfo(ISuperToken token, address account)
        internal
        view
        returns (uint256 lastUpdated, int96 flowRate, uint256 deposit, uint256 owedDeposit)
    {
        (, IConstantFlowAgreementV1 cfa) = _getHostAndCFA(token);
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);

        {
            (uint256 lastUpdatedCFA, int96 cfaNetFlowRate, uint256 cfaDeposit, uint256 cfaOwedDeposit) =
                cfa.getAccountFlowInfo(token, account);

            lastUpdated = lastUpdatedCFA;
            flowRate += cfaNetFlowRate;
            deposit += cfaDeposit;
            owedDeposit += cfaOwedDeposit;
        }

        {
            (uint256 lastUpdatedGDA, int96 gdaNetFlowRate, uint256 gdaDeposit) = gda.getAccountFlowInfo(token, account);

            if (lastUpdatedGDA > lastUpdated) {
                lastUpdated = lastUpdatedGDA;
            }
            flowRate += gdaNetFlowRate;
            deposit += gdaDeposit;
        }
    }

    /**
     * @dev get the aggregated CFA flow info of the account
     * @param token Super token address
     * @param account Account to query
     * @return lastUpdated Timestamp of the last change of the net flow
     * @return flowRate The net flow rate of token for account
     * @return deposit The sum of all deposits for account's flows
     * @return owedDeposit The sum of all owed deposits for account's flows
     */
    function getCFANetFlowInfo(ISuperToken token, address account)
        internal
        view
        returns (uint256 lastUpdated, int96 flowRate, uint256 deposit, uint256 owedDeposit)
    {
        (, IConstantFlowAgreementV1 cfa) = _getHostAndCFA(token);
        return cfa.getAccountFlowInfo(token, account);
    }

    /**
     * @dev get the aggregated GDA flow info of the account
     * @param token Super token address
     * @param account Account to query
     * @return lastUpdated Timestamp of the last change of the net flow
     * @return flowRate The net flow rate of token for account
     * @return deposit The sum of all deposits for account's flows
     * @return owedDeposit The sum of all owed deposits for account's flows
     */
    function getGDANetFlowInfo(ISuperToken token, address account)
        internal
        view
        returns (uint256 lastUpdated, int96 flowRate, uint256 deposit, uint256 owedDeposit)
    {
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        (lastUpdated, flowRate, deposit) = gda.getAccountFlowInfo(token, account);
        owedDeposit = 0; // unused in GDA
    }

    /**
     * @dev get the adjustment flow rate for a pool
     * @param token Super token address
     * @param pool The pool to query
     * @return poolAdjustmentFlowRate The adjustment flow rate of the pool
     */
    function getPoolAdjustmentFlowRate(ISuperToken token, ISuperfluidPool pool)
        internal
        view
        returns (int96 poolAdjustmentFlowRate)
    {
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        return gda.getPoolAdjustmentFlowRate(address(pool));
    }

    /**
     * @dev Get the total amount of tokens received by a member via instant and flowing distributions
     * @param pool The pool to query
     * @param memberAddr The member to query
     * @return totalAmountReceived The total amount received by the member
     */
    function getTotalAmountReceivedByMember(ISuperfluidPool pool, address memberAddr)
        internal
        view
        returns (uint256 totalAmountReceived)
    {
        return pool.getTotalAmountReceivedByMember(memberAddr);
    }

    /**
     * @notice calculate buffer for a CFA/GDA flow rate
     * @dev Even though we are using the CFA, the logic for calculating buffer is the same in the GDA
     *      and a change in the buffer logic in either means it is a BREAKING change
     * @param token The token used in flow
     * @param flowRate The flowrate to calculate the needed buffer for
     * @return bufferAmount The buffer amount based on flowRate, liquidationPeriod and minimum deposit
     */
    function getBufferAmountByFlowRate(ISuperToken token, int96 flowRate) internal view
        returns (uint256 bufferAmount)
    {
        (, IConstantFlowAgreementV1 cfa) = _getHostAndCFA(token);
        return cfa.getDepositRequiredForFlowRate(token, flowRate);
    }

    /**
     * @dev get existing flow permissions
     * @param token The token used in flow
     * @param sender sender of a flow
     * @param flowOperator the address we are checking permissions of for sender & token
     * @return allowCreate is true if the flowOperator can create flows
     * @return allowUpdate is true if the flowOperator can update flows
     * @return allowDelete is true if the flowOperator can delete flows
     * @return flowRateAllowance The flow rate allowance the flowOperator is granted (only goes down)
     */
    function getFlowPermissions(ISuperToken token, address sender, address flowOperator)
        internal view
        returns (bool allowCreate, bool allowUpdate, bool allowDelete, int96 flowRateAllowance)
    {
        (, IConstantFlowAgreementV1 cfa) = _getHostAndCFA(token);
        uint8 permissionsBitmask;
        (, permissionsBitmask, flowRateAllowance) = cfa.getFlowOperatorData(token, sender, flowOperator);
        allowCreate = permissionsBitmask & 1 == 1;
        allowUpdate = permissionsBitmask >> 1 & 1 == 1;
        allowDelete = permissionsBitmask >> 2 & 1 == 1;
    }


     /** IDA VIEW FUNCTIONS ************************************* */


    /**
     * @dev Gets an index by its ID and publisher.
     * @param token Super token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @return exist True if the index exists.
     * @return indexValue Total value of the index.
     * @return totalUnitsApproved Units of the index approved by subscribers.
     * @return totalUnitsPending Units of teh index not yet approved by subscribers.
     */
    function getIndex(ISuperToken token, address publisher, uint32 indexId)
        internal view
        returns (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending)
    {
        (, IInstantDistributionAgreementV1 ida) = _getHostAndIDA(token);
        return ida.getIndex(token, publisher, indexId);
    }

    /**
     * @dev Calculates the distribution amount based on the amount of tokens desired to distribute.
     * @param token Super token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param amount Amount of tokens desired to distribute.
     * @return actualAmount Amount to be distributed with correct rounding.
     * @return newIndexValue The index value after the distribution would be called.
     */
    function calculateDistribution(ISuperToken token, address publisher, uint32 indexId, uint256 amount)
        internal view
        returns (uint256 actualAmount, uint128 newIndexValue)
    {
        (, IInstantDistributionAgreementV1 ida) = _getHostAndIDA(token);
        return ida.calculateDistribution(token, publisher, indexId, amount);
    }

    /**
     * @dev List all subscriptions of an address
     * @param token Super token used in the indexes listed.
     * @param subscriber Subscriber address.
     * @return publishers Publishers of the indices.
     * @return indexIds IDs of the indices.
     * @return unitsList Units owned of the indices.
     */
    function listSubscriptions(
        ISuperToken token,
        address subscriber
    )
        internal view
        returns (
            address[] memory publishers,
            uint32[] memory indexIds,
            uint128[] memory unitsList
        )
    {
        (, IInstantDistributionAgreementV1 ida) = _getHostAndIDA(token);
        return ida.listSubscriptions(token, subscriber);
    }

    /**
     * @dev Gets subscription by publisher, index id, and subscriber.
     * @param token Super token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber to the index.
     * @return exist True if the subscription exists.
     * @return approved True if the subscription has been approved by the subscriber.
     * @return units Units held by the subscriber
     * @return pendingDistribution If not approved, the amount to be claimed on approval.
     */
    function getSubscription(ISuperToken token, address publisher, uint32 indexId, address subscriber)
        internal view
        returns (bool exist, bool approved, uint128 units, uint256 pendingDistribution)
    {
        (, IInstantDistributionAgreementV1 ida) = _getHostAndIDA(token);
        return ida.getSubscription(token, publisher, indexId, subscriber);
    }

    /*
     * @dev Gets subscription by the agreement ID.
     * @param token Super Token used with the index.
     * @param agreementId Agreement ID, unique to the subscriber and index ID.
     * @return publisher Publisher of the index.
     * @return indexId ID of the index.
     * @return approved True if the subscription has been approved by the subscriber.
     * @return units Units held by the subscriber
     * @return pendingDistribution If not approved, the amount to be claimed on approval.
     */
    function getSubscriptionByID(ISuperToken token, bytes32 agreementId)
        internal view
        returns (
            address publisher,
            uint32 indexId,
            bool approved,
            uint128 units,
            uint256 pendingDistribution
        )
    {
        (, IInstantDistributionAgreementV1 ida) = _getHostAndIDA(token);
        return ida.getSubscriptionByID(token, agreementId);
    }

    /** GDA VIEW FUNCTIONS ************************************* */
    function getFlowDistributionFlowRate(ISuperToken token, address from, ISuperfluidPool to)
        internal
        view
        returns (int96)
    {
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        return gda.getFlowRate(token, from, to);
    }

    function estimateFlowDistributionActualFlowRate(
        ISuperToken token,
        address from,
        ISuperfluidPool to,
        int96 requestedFlowRate
    ) internal view returns (int96 actualFlowRate, int96 totalDistributionFlowRate) {
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        return gda.estimateFlowDistributionActualFlowRate(token, from, to, requestedFlowRate);
    }

    function estimateDistributionActualAmount(
        ISuperToken token,
        address from,
        ISuperfluidPool to,
        uint256 requestedAmount
    ) internal view returns (uint256 actualAmount) {
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        return gda.estimateDistributionActualAmount(token, from, to, requestedAmount);
    }

    function isMemberConnected(ISuperToken token, address pool, address member) internal view returns (bool) {
        (, IGeneralDistributionAgreementV1 gda) = _getHostAndGDA(token);
        return gda.isMemberConnected(ISuperfluidPool(pool), member);
    }


    /** IDA BASE FUNCTIONS ************************************* */


    /**
     * @dev Creates a new index.
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     */
    function createIndex(
        ISuperToken token,
        uint32 indexId
    ) internal returns (bool) {
        return createIndex(token, indexId, new bytes(0));
    }

    /**
     * @dev Creates a new index with userData.
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param userData Arbitrary user data field.
     */
    function createIndex(
        ISuperToken token,
        uint32 indexId,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        host.callAgreement(
            ida,
            abi.encodeCall(
                ida.createIndex,
                (
                    token,
                    indexId,
                    new bytes(0) // ctx placeholder
                )
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Updates an index value. This distributes an amount of tokens equal to
     * `indexValue - lastIndexValue`. See `distribute` for another way to distribute.
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param indexValue New TOTAL index value, this will equal the total amount distributed.
     */
    function updateIndexValue(
        ISuperToken token,
        uint32 indexId,
        uint128 indexValue
    ) internal returns (bool) {
        return updateIndexValue(token, indexId, indexValue, new bytes(0));
    }

    /**
     * @dev Updates an index value with userData. This distributes an amount of tokens equal to
     * `indexValue - lastIndexValue`. See `distribute` for another way to distribute.
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param indexValue New TOTAL index value, this will equal the total amount distributed.
     * @param userData Arbitrary user data field.
     */
    function updateIndexValue(
        ISuperToken token,
        uint32 indexId,
        uint128 indexValue,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        host.callAgreement(
            ida,
            abi.encodeCall(
                ida.updateIndex,
                (
                    token,
                    indexId,
                    indexValue,
                    new bytes(0) // ctx placeholder
                )
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Distributes tokens in a more developer friendly way than `updateIndex`. Instead of
     * passing the new total index value, you pass the amount of tokens desired to be distributed.
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param amount - total number of tokens desired to be distributed
     * NOTE in many cases, there can be some precision loss
     This may cause a slight difference in the amount param specified and the actual amount distributed.
     See below for math:
     //indexDelta = amount the index will be updated by during an internal call to _updateIndex().
     It is calculated like so:
     indexDelta = amount / totalUnits
     (see the distribute() implementatation in ./agreements/InstantDistributionAgreement.sol)
     * NOTE Solidity does not support floating point numbers
     So the indexDelta will be rounded down to the nearest integer.
     This will create a 'remainder' amount of tokens that will not be distributed
     (we'll call this the 'distribution modulo')
     distributionModulo = amount - indexDelta * totalUnits
     * NOTE due to rounding, there may be a small amount of tokens left in the publisher's account
     This amount is equal to the 'distributionModulo' value
     //
     */
    function distribute(
        ISuperToken token,
        uint32 indexId,
        uint256 amount
    ) internal returns (bool) {
        return distribute(token, indexId, amount, new bytes(0));
    }

    /**
     * @dev Distributes tokens in a more developer friendly way than `updateIndex` (w user data). Instead of
     * passing the new total index value, this function will increase the index value by `amount`.
     * This takes arbitrary user data.
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param amount Amount by which the index value should increase.
     * @param userData Arbitrary user data field.
     */
    function distribute(
        ISuperToken token,
        uint32 indexId,
        uint256 amount,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        host.callAgreement(
            ida,
            abi.encodeCall(
                ida.distribute,
                (
                    token,
                    indexId,
                    amount,
                    new bytes(0) // ctx placeholder
                )
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Approves a subscription to an index. The subscriber's real time balance will not update
     * until the subscription is approved, but once approved, the balance will be updated with
     * prior distributions.
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     */
    function approveSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId
    ) internal returns (bool) {
        return approveSubscription(token, publisher, indexId, new bytes(0));
    }

    /**
     * @dev Approves a subscription to an index with user data. The subscriber's real time balance will not update
     * until the subscription is approved, but once approved, the balance will be updated with
     * prior distributions.
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param userData Arbitrary user data field.
     */
    function approveSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        host.callAgreement(
            ida,
            abi.encodeCall(
                ida.approveSubscription,
                (
                    token,
                    publisher,
                    indexId,
                    new bytes(0) // ctx placeholder
                )
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Revokes a previously approved subscription.
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     */
    function revokeSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId
    ) internal returns (bool) {
        return revokeSubscription(token, publisher, indexId, new bytes(0));
    }

    /**
     * @dev Revokes a previously approved subscription. This takes arbitrary user data.
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param userData Arbitrary user data field.
     */
    function revokeSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        host.callAgreement(
            ida,
            abi.encodeCall(
                ida.revokeSubscription,
                (
                    token,
                    publisher,
                    indexId,
                    new bytes(0) // ctx placeholder
                )
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Updates the units of a subscription. This changes the number of shares the subscriber holds
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address whose units are to be updated.
     * @param units New number of units the subscriber holds.
     */
    function updateSubscriptionUnits(
        ISuperToken token,
        uint32 indexId,
        address subscriber,
        uint128 units
    ) internal returns (bool) {
        return updateSubscriptionUnits(token, indexId, subscriber, units, new bytes(0));
    }

    /**
     * @dev Updates the units of a subscription. This changes the number of shares the subscriber
     * holds. This takes arbitrary user data.
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address whose units are to be updated.
     * @param units New number of units the subscriber holds.
     * @param userData Arbitrary user data field.
     */
    function updateSubscriptionUnits(
        ISuperToken token,
        uint32 indexId,
        address subscriber,
        uint128 units,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        host.callAgreement(
         ida,
            abi.encodeCall(
                ida.updateSubscription,
                (
                    token,
                    indexId,
                    subscriber,
                    units,
                    new bytes(0) // ctx placeholder
                )
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Deletes a subscription, setting a subcriber's units to zero
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address whose units are to be deleted.
     */
    function deleteSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber
    ) internal returns (bool) {
        return deleteSubscription(token, publisher, indexId, subscriber, new bytes(0));
    }

    /**
     * @dev Deletes a subscription, setting a subcriber's units to zero. This takes arbitrary userdata.
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address whose units are to be deleted.
     * @param userData Arbitrary user data field.
     */
    function deleteSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        host.callAgreement(
            ida,
            abi.encodeCall(
                ida.deleteSubscription,
                (
                    token,
                    publisher,
                    indexId,
                    subscriber,
                    new bytes(0) // ctx placeholder
                )
            ),
            userData
        );
        return true;
    }

    /**
     * @dev Claims pending distribution. Subscription should not be approved
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address that receives the claim.
     */
    function claim(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber
    ) internal returns (bool) {
        return claim(token, publisher, indexId, subscriber, new bytes(0));
    }

    /**
     * @dev Claims pending distribution. Subscription should not be approved. This takes arbitrary user data.
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address that receives the claim.
     * @param userData Arbitrary user data field.
     */
    function claim(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes memory userData
    ) internal returns (bool) {
         (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        host.callAgreement(
            ida,
            abi.encodeCall(
                ida.claim,
                (
                    token,
                    publisher,
                    indexId,
                    subscriber,
                    new bytes(0) // ctx placeholder
                )
            ),
            userData
        );
        return true;
    }

    /** IDA WITH CTX FUNCTIONS ************************************* */

    /**
     * @dev Creates a new index with ctx.
     * Meant for usage in super app callbacks
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function createIndexWithCtx(
        ISuperToken token,
        uint32 indexId,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        (newCtx, ) = host.callAgreementWithContext(
            ida,
            abi.encodeCall(
                ida.createIndex,
                (
                    token,
                    indexId,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Updates an index value with ctx. This distributes an amount of tokens equal to
     * `indexValue - lastIndexValue`. See `distribute` for another way to distribute.
     * Meant for usage in super app callbakcs
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param indexValue New TOTAL index value, this will equal the total amount distributed.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function updateIndexValueWithCtx(
        ISuperToken token,
        uint32 indexId,
        uint128 indexValue,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        (newCtx, ) = host.callAgreementWithContext(
            ida,
            abi.encodeCall(
                ida.updateIndex,
                (
                    token,
                    indexId,
                    indexValue,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Distributes tokens in a more developer friendly way than `updateIndex`.Instead of
     * passing the new total index value, this function will increase the index value by `amount`.
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param amount Amount by which the index value should increase.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function distributeWithCtx(
        ISuperToken token,
        uint32 indexId,
        uint256 amount,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        (newCtx, ) = host.callAgreementWithContext(
            ida,
            abi.encodeCall(
                ida.distribute,
                (
                    token,
                    indexId,
                    amount,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Approves a subscription to an index. The subscriber's real time balance will not update
     * until the subscription is approved, but once approved, the balance will be updated with
     * prior distributions.
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function approveSubscriptionWithCtx(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        (newCtx, ) = host.callAgreementWithContext(
            ida,
            abi.encodeCall(
                ida.approveSubscription,
                (
                    token,
                    publisher,
                    indexId,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Revokes a previously approved subscription. Meant for usage in super apps
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function revokeSubscriptionWithCtx(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        (newCtx, ) = host.callAgreementWithContext(
            ida,
            abi.encodeCall(
                ida.revokeSubscription,
                (
                    token,
                    publisher,
                    indexId,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Updates the units of a subscription. This changes the number of shares the subscriber
     * holds. Meant for usage in super apps
     * @param token Super Token used with the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address whose units are to be updated.
     * @param units New number of units the subscriber holds.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function updateSubscriptionUnitsWithCtx(
        ISuperToken token,
        uint32 indexId,
        address subscriber,
        uint128 units,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        (newCtx, ) = host.callAgreementWithContext(
         ida,
            abi.encodeCall(
                ida.updateSubscription,
                (
                    token,
                    indexId,
                    subscriber,
                    units,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Deletes a subscription, setting a subcriber's units to zero.
     * Meant for usage in super apps
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address whose units are to be deleted.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function deleteSubscriptionWithCtx(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        (newCtx, ) = host.callAgreementWithContext(
            ida,
            abi.encodeCall(
                ida.deleteSubscription,
                (
                    token,
                    publisher,
                    indexId,
                    subscriber,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Claims pending distribution. Subscription should not be approved.
     * Meant for usage in super app callbacks
     * @param token Super Token used with the index.
     * @param publisher Publisher of the index.
     * @param indexId ID of the index.
     * @param subscriber Subscriber address that receives the claim.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function claimWithCtx(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
         (ISuperfluid host, IInstantDistributionAgreementV1 ida) = _getAndCacheHostAndIDA(token);
        (newCtx, ) = host.callAgreementWithContext(
            ida,
            abi.encodeCall(
                ida.claim,
                (
                    token,
                    publisher,
                    indexId,
                    subscriber,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /** GDA BASE FUNCTIONS ************************************* */


    /**
     * @dev Creates a new Superfluid Pool.
     * @param token The Super Token address.
     * @param admin The pool admin address.
     * @param poolConfig The pool configuration (see PoolConfig in IGeneralDistributionAgreementV1.sol)
     * @return pool The address of the deployed Superfluid Pool
     */
    function createPool(ISuperToken token, address admin, PoolConfig memory poolConfig)
        internal
        returns (ISuperfluidPool pool)
    {
        (, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        pool = gda.createPool(token, admin, poolConfig);
    }

    /**
     * @dev Updates the units of a pool member.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to update.
     * @param memberAddress The address of the member to update.
     * @param newUnits The new units of the member.
     * @return bool A boolean value indicating whether the pool was created successfully.
     */
    function updateMemberUnits(ISuperToken token, ISuperfluidPool pool, address memberAddress, uint128 newUnits)
        internal
        returns (bool)
    {
        return updateMemberUnits(token, pool, memberAddress, newUnits, new bytes(0));
    }

    /**
     * @dev Updates the units of a pool member.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to update.
     * @param memberAddress The address of the member to update.
     * @param newUnits The new units of the member.
     * @param userData User-specific data.
     * @return A boolean value indicating whether the pool was created successfully.
     */
    function updateMemberUnits(
        ISuperToken token,
        ISuperfluidPool pool,
        address memberAddress,
        uint128 newUnits,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        host.callAgreement(
            gda, abi.encodeCall(gda.updateMemberUnits, (pool, memberAddress, newUnits, new bytes(0))), userData
        );

        return true;
    }

    /**
     * @dev Claims all tokens from the pool.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to claim from.
     * @param memberAddress The address of the member to claim for.
     * @return A boolean value indicating whether the claim was successful.
     */
    function claimAll(ISuperToken token, ISuperfluidPool pool, address memberAddress) internal returns (bool) {
        return claimAll(token, pool, memberAddress, new bytes(0));
    }

    /**
     * @dev Claims all tokens from the pool.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to claim from.
     * @param memberAddress The address of the member to claim for.
     * @param userData User-specific data.
     * @return A boolean value indicating whether the claim was successful.
     */
    function claimAll(ISuperToken token, ISuperfluidPool pool, address memberAddress, bytes memory userData)
        internal
        returns (bool)
    {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        host.callAgreement(gda, abi.encodeCall(gda.claimAll, (pool, memberAddress, new bytes(0))), userData);

        return true;
    }


    /**
     * @dev Connects a pool member to `pool`.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to connect.
     * @return A boolean value indicating whether the connection was successful.
     */
    function connectPool(ISuperToken token, ISuperfluidPool pool) internal returns (bool) {
        return connectPool(token, pool, new bytes(0));
    }

    /**
     * @dev Connects a pool member to `pool`.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to connect.
     * @param userData User-specific data.
     * @return A boolean value indicating whether the connection was successful.
     */
    function connectPool(ISuperToken token, ISuperfluidPool pool, bytes memory userData) internal returns (bool) {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        host.callAgreement(gda, abi.encodeCall(gda.connectPool, (pool, new bytes(0))), userData);

        return true;
    }

    /**
     * @dev Disconnects a pool member from `pool`.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to disconnect.
     * @return A boolean value indicating whether the disconnection was successful.
     */
    function disconnectPool(ISuperToken token, ISuperfluidPool pool) internal returns (bool) {
        return disconnectPool(token, pool, new bytes(0));
    }

    /**
     * @dev Disconnects a pool member from `pool`.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to disconnect.
     * @param userData User-specific data.
     * @return A boolean value indicating whether the disconnection was successful.
     */
    function disconnectPool(ISuperToken token, ISuperfluidPool pool, bytes memory userData) internal returns (bool) {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        host.callAgreement(gda, abi.encodeCall(gda.disconnectPool, (pool, new bytes(0))), userData);
        return true;
    }

    /**
     * @dev Tries to distribute `requestedAmount` amount of `token` from `from` to `pool`.
     * @param token The Super Token address.
     * @param from The address from which to distribute tokens.
     * @param pool The Superfluid Pool address.
     * @param requestedAmount The amount of tokens to distribute.
     * @return A boolean value indicating whether the distribution was successful.
     */
    function distributeToPool(ISuperToken token, address from, ISuperfluidPool pool, uint256 requestedAmount)
        internal
        returns (bool)
    {
        return distribute(token, from, pool, requestedAmount, new bytes(0));
    }

    /**
     * @dev Tries to distribute `requestedAmount` amount of `token` from `from` to `pool`.
     * @param token The Super Token address.
     * @param from The address from which to distribute tokens.
     * @param pool The Superfluid Pool address.
     * @param requestedAmount The amount of tokens to distribute.
     * @param userData User-specific data.
     * @return A boolean value indicating whether the distribution was successful.
     */
    function distribute(
        ISuperToken token,
        address from,
        ISuperfluidPool pool,
        uint256 requestedAmount,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        host.callAgreement(
            gda, abi.encodeCall(gda.distribute, (token, from, pool, requestedAmount, new bytes(0))), userData
        );
        return true;
    }

    /**
     * @dev Tries to distribute flow at `requestedFlowRate` of `token` from `from` to `pool`.
     * @param token The Super Token address.
     * @param from The address from which to distribute tokens.
     * @param pool The Superfluid Pool address.
     * @param requestedFlowRate The flow rate of tokens to distribute.
     * @return A boolean value indicating whether the distribution was successful.
     */
    function distributeFlow(ISuperToken token, address from, ISuperfluidPool pool, int96 requestedFlowRate)
        internal
        returns (bool)
    {
        return distributeFlow(token, from, pool, requestedFlowRate, new bytes(0));
    }

    /**
     * @dev Tries to distribute flow at `requestedFlowRate` of `token` from `from` to `pool`.
     * @param token The Super Token address.
     * @param from The address from which to distribute tokens.
     * @param pool The Superfluid Pool address.
     * @param requestedFlowRate The flow rate of tokens to distribute.
     * @param userData User-specific data.
     * @return A boolean value indicating whether the distribution was successful.
     */
    function distributeFlow(
        ISuperToken token,
        address from,
        ISuperfluidPool pool,
        int96 requestedFlowRate,
        bytes memory userData
    ) internal returns (bool) {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        host.callAgreement(
            gda, abi.encodeCall(gda.distributeFlow, (token, from, pool, requestedFlowRate, new bytes(0))), userData
        );
        return true;
    }

    /** GDA WITH CTX FUNCTIONS ************************************* */

    /**
     * @dev Updates the units of a pool member.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to update.
     * @param memberAddress The address of the member to update.
     * @param newUnits The new units of the member.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function updateMemberUnitsWithCtx(
        ISuperToken token,
        ISuperfluidPool pool,
        address memberAddress,
        uint128 newUnits,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        (newCtx,) = host.callAgreementWithContext(
            gda,
            abi.encodeCall(
                gda.updateMemberUnits,
                (
                    pool,
                    memberAddress,
                    newUnits,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Claims all tokens from the pool.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to claim from.
     * @param memberAddress The address of the member to claim for.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function claimAllWithCtx(ISuperToken token, ISuperfluidPool pool, address memberAddress, bytes memory ctx)
        internal
        returns (bytes memory newCtx)
    {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        (newCtx,) = host.callAgreementWithContext(
            gda,
            abi.encodeCall(
                gda.claimAll,
                (
                    pool,
                    memberAddress,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Connects a pool member to `pool`.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to connect.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function connectPoolWithCtx(ISuperToken token, ISuperfluidPool pool, bytes memory ctx)
        internal
        returns (bytes memory newCtx)
    {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        (newCtx,) = host.callAgreementWithContext(
            gda,
            abi.encodeCall(
                gda.connectPool,
                (
                    pool,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Disconnects a pool member from `pool`.
     * @param token The Super Token address.
     * @param pool The Superfluid Pool to disconnect.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function disconnectPoolWithCtx(ISuperToken token, ISuperfluidPool pool, bytes memory ctx)
        internal
        returns (bytes memory newCtx)
    {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        (newCtx,) = host.callAgreementWithContext(
            gda,
            abi.encodeCall(
                gda.disconnectPool,
                (
                    pool,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Tries to distribute `requestedAmount` amount of `token` from `from` to `pool`.
     * @param token The Super Token address.
     * @param from The address from which to distribute tokens.
     * @param pool The Superfluid Pool address.
     * @param requestedAmount The amount of tokens to distribute.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function distributeWithCtx(
        ISuperToken token,
        address from,
        ISuperfluidPool pool,
        uint256 requestedAmount,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        (newCtx,) = host.callAgreementWithContext(
            gda,
            abi.encodeCall(
                gda.distribute,
                (
                    token,
                    from,
                    pool,
                    requestedAmount,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    /**
     * @dev Tries to distribute flow at `requestedFlowRate` of `token` from `from` to `pool`.
     * @param token The Super Token address.
     * @param from The address from which to distribute tokens.
     * @param pool The Superfluid Pool address.
     * @param requestedFlowRate The flow rate of tokens to distribute.
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @return newCtx The updated context after the execution of the agreement function
     */
    function distributeFlowWithCtx(
        ISuperToken token,
        address from,
        ISuperfluidPool pool,
        int96 requestedFlowRate,
        bytes memory ctx
    ) internal returns (bytes memory newCtx) {
        (ISuperfluid host, IGeneralDistributionAgreementV1 gda) = _getAndCacheHostAndGDA(token);
        (newCtx,) = host.callAgreementWithContext(
            gda,
            abi.encodeCall(
                gda.distributeFlow,
                (
                    token,
                    from,
                    pool,
                    requestedFlowRate,
                    new bytes(0) // ctx placeholder
                )
            ),
            "0x",
            ctx
        );
    }

    // ************** private helpers **************

    // @note We must use hardcoded constants here because:
    // Only direct number constants and references to such constants are supported by inline assembly.
    // keccak256("org.superfluid-finance.apps.SuperTokenLibrary.v1.host")
    bytes32 private constant _HOST_SLOT = 0x65599bf746e17a00ea62e3610586992d88101b78eec3cf380706621fb97ea837;
    // keccak256("org.superfluid-finance.apps.SuperTokenLibrary.v1.cfa")
    bytes32 private constant _CFA_SLOT = 0xb969d79d88acd02d04ed7ee7d43b949e7daf093d363abcfbbc43dfdfd1ce969a;
    // keccak256("org.superfluid-finance.apps.SuperTokenLibrary.v1.ida");
    bytes32 private constant _IDA_SLOT = 0xa832ee1924ea960211af2df07d65d166232018f613ac6708043cd8f8773eddeb;
    // keccak256("org.superfluid-finance.apps.SuperTokenLibrary.v1.gda");
    bytes32 private constant _GDA_SLOT = 0xc36f6c05164a669ecb6da53e218d77ae44d51cfc99f91e5a125a18de0949bee4;

    // gets the host and cfa addrs for the token and caches it in storage for gas efficiency
    // to be used in state changing methods
    function _getAndCacheHostAndCFA(ISuperToken token)
        private
        returns (ISuperfluid host, IConstantFlowAgreementV1 cfa)
    {
        // check if already in contract storage...
        assembly {
            // solium-disable-line
            host := sload(_HOST_SLOT)
            cfa := sload(_CFA_SLOT)
        }
        if (address(cfa) == address(0)) {
            // framework contract addrs not yet cached, retrieving now...
            if (address(host) == address(0)) {
                host = ISuperfluid(token.getHost());
            }

            cfa = IConstantFlowAgreementV1(address(ISuperfluid(host).getAgreementClass(
                keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1"))));
            // now that we got them and are in a transaction context, persist in storage
            assembly {
                // solium-disable-line
                sstore(_HOST_SLOT, host)
                sstore(_CFA_SLOT, cfa)
            }
        }
        assert(address(host) != address(0));
        assert(address(cfa) != address(0));
    }

    // gets the host and ida addrs for the token and caches it in storage for gas efficiency
    // to be used in state changing methods
    function _getAndCacheHostAndIDA(ISuperToken token)
        private
        returns (ISuperfluid host, IInstantDistributionAgreementV1 ida)
    {
        // check if already in contract storage...
        assembly {
            // solium-disable-line
            host := sload(_HOST_SLOT)
            ida := sload(_IDA_SLOT)
        }
        if (address(ida) == address(0)) {
            // framework contract addrs not yet cached, retrieving now...
            if (address(host) == address(0)) {
                host = ISuperfluid(token.getHost());
            }
            ida = IInstantDistributionAgreementV1(address(ISuperfluid(host).getAgreementClass(
                keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1"))));
            // now that we got them and are in a transaction context, persist in storage
            assembly {
                // solium-disable-line
                sstore(_HOST_SLOT, host)
                sstore(_IDA_SLOT, ida)
            }
        }
        assert(address(host) != address(0));
        assert(address(ida) != address(0));
    }

    // gets the host and gda addrs for the token and caches it in storage for gas efficiency
    // to be used in state changing methods
    function _getAndCacheHostAndGDA(ISuperToken token)
        private
        returns (ISuperfluid host, IGeneralDistributionAgreementV1 gda)
    {
        // check if already in contract storage...
        assembly {
            // solium-disable-line
            host := sload(_HOST_SLOT)
            gda := sload(_GDA_SLOT)
        }
        if (address(gda) == address(0)) {
            // framework contract addrs not yet cached, retrieving now...
            if (address(host) == address(0)) {
                host = ISuperfluid(token.getHost());
            }
            gda = IGeneralDistributionAgreementV1(
                address(
                    ISuperfluid(host).getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.GeneralDistributionAgreement.v1")
                    )
                )
            );
            // now that we got them and are in a transaction context, persist in storage
            assembly {
                // solium-disable-line
                sstore(_HOST_SLOT, host)
                sstore(_GDA_SLOT, gda)
            }
        }
        assert(address(host) != address(0));
        assert(address(gda) != address(0));
    }

    // gets the host and cfa addrs for the token
    // to be used in non-state changing methods (view functions)
    function _getHostAndCFA(ISuperToken token) private view returns (ISuperfluid host, IConstantFlowAgreementV1 cfa) {
        // check if already in contract storage...
        assembly {
            // solium-disable-line
            host := sload(_HOST_SLOT)
            cfa := sload(_CFA_SLOT)
        }
        if (address(cfa) == address(0)) {
            // framework contract addrs not yet cached in storage, retrieving now...
            if (address(host) == address(0)) {
                host = ISuperfluid(token.getHost());
            }
            cfa = IConstantFlowAgreementV1(address(ISuperfluid(host).getAgreementClass(
                keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1"))));
        }
        assert(address(host) != address(0));
        assert(address(cfa) != address(0));
    }

    // gets the host and ida addrs for the token
    // to be used in non-state changing methods (view functions)
    function _getHostAndIDA(ISuperToken token)
        private
        view
        returns (ISuperfluid host, IInstantDistributionAgreementV1 ida)
    {
        // check if already in contract storage...
        assembly {
            // solium-disable-line
            host := sload(_HOST_SLOT)
            ida := sload(_IDA_SLOT)
        }
        if (address(ida) == address(0)) {
            // framework contract addrs not yet cached in storage, retrieving now...
            if (address(host) == address(0)) {
                host = ISuperfluid(token.getHost());
            }
            ida = IInstantDistributionAgreementV1(address(ISuperfluid(host).getAgreementClass(
                keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1"))));
        }
        assert(address(host) != address(0));
        assert(address(ida) != address(0));
    }

    // gets the host and gda addrs for the token
    // to be used in non-state changing methods (view functions)
    function _getHostAndGDA(ISuperToken token)
        private
        view
        returns (ISuperfluid host, IGeneralDistributionAgreementV1 gda)
    {
        // check if already in contract storage...
        assembly {
            // solium-disable-line
            host := sload(_HOST_SLOT)
            gda := sload(_GDA_SLOT)
        }
        if (address(gda) == address(0)) {
            // framework contract addrs not yet cached in storage, retrieving now...
            if (address(host) == address(0)) {
                host = ISuperfluid(token.getHost());
            }
            gda = IGeneralDistributionAgreementV1(
                address(
                    ISuperfluid(host).getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.GeneralDistributionAgreement.v1")
                    )
                )
            );
        }
        assert(address(host) != address(0));
        assert(address(gda) != address(0));
    }
}
