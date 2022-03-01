// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {ISuperfluid, ISuperfluidToken} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IInstantDistributionAgreementV1
} from "../interfaces/agreements/IInstantDistributionAgreementV1.sol";

import {IDAv1Library} from "../apps/IDAv1Library.sol";

contract IDAv1LibraryMock {
    using IDAv1Library for IDAv1Library.InitData;

    IDAv1Library.InitData internal _idaLib;

    constructor(ISuperfluid host, IInstantDistributionAgreementV1 ida) {
        _idaLib = IDAv1Library.InitData(host, ida);
    }

    /**************************************************************************
     * View Functions
     *************************************************************************/

    function getIndexTest(ISuperfluidToken token, address publisher, uint32 indexId)
        external
        view
        returns (
            bool exist,
            uint128 indexValue,
            uint128 totalUnitsApproved,
            uint128 totalUnitsPending
        )
    {
        return _idaLib.getIndex(token, publisher, indexId);
    }

    function calculateDistributionTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        uint256 amount
    )
        external
        view
        returns (
            uint256 actualAmount,
            uint128 newIndexValue
        )
    {
        return _idaLib.calculateDistribution(token, publisher, indexId, amount);
    }

    function listSubscriptionsTest(ISuperfluidToken token, address subscriber)
        external
        view
        returns (
            address[] memory publishers,
            uint32[] memory indexIds,
            uint128[] memory unitsList
        )
    {
        return _idaLib.listSubscriptions(token, subscriber);
    }

    function getSubscriptionTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber
    )
        external
        view
        returns (
            bool exist,
            bool approved,
            uint128 units,
            uint256 pendingDistribution
        )
    {
        return _idaLib.getSubscription(token, publisher, indexId, subscriber);
    }

    /// @dev agreementId == keccak256(abi.encodePacked("subscription", subscriber, indexId));
    function getSubscriptionByIDTest(ISuperfluidToken token, bytes32 agreementId)
        external
        view
        returns (
            address publisher,
            uint32 indexId,
            bool approved,
            uint128 units,
            uint256 pendingDistribution
        )
    {
        return _idaLib.getSubscriptionByID(token, agreementId);
    }

    /**************************************************************************
     * Index Operations
     *************************************************************************/

    function createIndexTest(ISuperfluidToken token, uint32 indexId) external {
        _idaLib.createIndex(token, indexId);
    }

    function createIndexWithUserDataTest(
        ISuperfluidToken token,
        uint32 indexId,
        bytes memory userData
    ) external {
        _idaLib.createIndex(token, indexId, userData);
    }

    function createIndexWithCtxTest(
        bytes memory ctx,
        ISuperfluidToken token,
        uint32 indexId
    ) external returns (bytes memory) {
        return _idaLib.createIndexWithCtx(ctx, token, indexId);
    }

    function createIndexWithCtxUserDataTest(
        bytes memory ctx,
        ISuperfluidToken token,
        uint32 indexId,
        bytes memory userData
    ) external returns (bytes memory) {
        return _idaLib.createIndexWithCtx(ctx, token, indexId, userData);
    }

    function updateIndexValueTest(
        ISuperfluidToken token,
        uint32 indexId,
        uint128 indexValue
    ) external {
        _idaLib.updateIndexValue(token, indexId, indexValue);
    }

    function updateIndexValueWithUserDataTest(
        ISuperfluidToken token,
        uint32 indexId,
        uint128 indexValue,
        bytes memory userData
    ) external {
        _idaLib.updateIndexValue(token, indexId, indexValue, userData);
    }

    function updateIndexValueWithCtxTest(
        bytes memory ctx,
        ISuperfluidToken token,
        uint32 indexId,
        uint128 indexValue
    ) external returns (bytes memory) {
        return _idaLib.updateIndexValueWithCtx(ctx, token, indexId, indexValue);
    }

    function updateIndexValueWithCtxUserDataTest(
        bytes memory ctx,
        ISuperfluidToken token,
        uint32 indexId,
        uint128 indexValue,
        bytes memory userData
    ) external returns (bytes memory) {
        return _idaLib.updateIndexValueWithCtx(ctx, token, indexId, indexValue, userData);
    }

    function distributeTest(ISuperfluidToken token, uint32 indexId, uint256 amount) external {
        _idaLib.distribute(token, indexId, amount);
    }

    function distributeWithUserDataTest(
        ISuperfluidToken token,
        uint32 indexId,
        uint256 amount,
        bytes memory userData
    ) external {
        _idaLib.distribute(token, indexId, amount, userData);
    }

    function distributeWithCtxTest(
        bytes memory ctx,
        ISuperfluidToken token,
        uint32 indexId,
        uint256 amount
    ) external returns (bytes memory) {
        return _idaLib.distributeWithCtx(ctx, token, indexId, amount);
    }

    function distributeWithCtxUserDataTest(
        bytes memory ctx,
        ISuperfluidToken token,
        uint32 indexId,
        uint256 amount,
        bytes memory userData
    ) external returns (bytes memory) {
        return _idaLib.distributeWithCtx(ctx, token, indexId, amount, userData);
    }

    /**************************************************************************
     * Subscription Operations
     *************************************************************************/
    
    function approveSubscriptionTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId
    ) external {
        _idaLib.approveSubscription(token, publisher, indexId);
    }

    function approveSubscriptionWithUserDataTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        bytes memory userData
    ) external {
        _idaLib.approveSubscription(token, publisher, indexId, userData);
    }

    function approveSubscriptionWithCtxTest(
        bytes memory ctx,
        ISuperfluidToken token,
        address publisher,
        uint32 indexId
    ) external returns (bytes memory) {
        return _idaLib.approveSubscriptionWithCtx(ctx, token, publisher, indexId);
    }

    function approveSubscriptionWithCtxUserDataTest(
        bytes memory ctx,
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        bytes memory userData
    ) external returns (bytes memory) {
        return _idaLib.approveSubscriptionWithCtx(ctx, token, publisher, indexId, userData);
    }

    function revokeSubscriptionTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId
    ) external {
        _idaLib.revokeSubscription(token, publisher, indexId);
    }

    function revokeSubscriptionWithUserDataTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        bytes memory userData
    ) external {
        _idaLib.revokeSubscription(token, publisher, indexId, userData);
    }

    function revokeSubscriptionWithCtxTest(
        bytes memory ctx,
        ISuperfluidToken token,
        address publisher,
        uint32 indexId
    ) external returns (bytes memory) {
        return _idaLib.revokeSubscriptionWithCtx(ctx, token, publisher, indexId);
    }

    function revokeSubscriptionWithCtxUserDataTest(
        bytes memory ctx,
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        bytes memory userData
    ) external returns (bytes memory) {
        return _idaLib.revokeSubscriptionWithCtx(ctx, token, publisher, indexId, userData);
    }

    function updateSubscriptionUnitsTest(
        ISuperfluidToken token,
        uint32 indexId,
        address subscriber,
        uint128 units
    ) external {
        _idaLib.updateSubscriptionUnits(token, indexId, subscriber, units);
    }

    function updateSubscriptionUnitsWithUserDataTest(
        ISuperfluidToken token,
        uint32 indexId,
        address subscriber,
        uint128 units,
        bytes memory userData
    ) external {
        _idaLib.updateSubscriptionUnits(token, indexId, subscriber, units, userData);
    }

    function updateSubscriptionUnitsWithCtxTest(
        bytes memory ctx,
        ISuperfluidToken token,
        uint32 indexId,
        address subscriber,
        uint128 units
    ) external returns (bytes memory) {
        return _idaLib.updateSubscriptionUnitsWithCtx(ctx, token, indexId, subscriber, units);
    }

    function updateSubscriptionUnitsWithCtxUserDataTest(
        bytes memory ctx,
        ISuperfluidToken token,
        uint32 indexId,
        address subscriber,
        uint128 units,
        bytes memory userData
    ) external returns (bytes memory) {
        return _idaLib.updateSubscriptionUnitsWithCtx(
            ctx,
            token,
            indexId,
            subscriber,
            units,
            userData
        );
    }

    function deleteSubscriptionTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber
    ) external {
        _idaLib.deleteSubscription(token, publisher, indexId, subscriber);
    }

    function deleteSubscriptionWithUserDataTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes memory userData
    ) external {
        _idaLib.deleteSubscription(token, publisher, indexId, subscriber, userData);
    }

    function deleteSubscriptionWithCtxTest(
        bytes memory ctx,
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber
    ) external returns (bytes memory) {
        return _idaLib.deleteSubscriptionWithCtx(ctx, token, publisher, indexId, subscriber);
    }

    function deleteSubscriptionWithCtxUserDataTest(
        bytes memory ctx,
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes memory userData
    ) external returns (bytes memory) {
        return _idaLib.deleteSubscriptionWithCtx(
            ctx, 
            token, 
            publisher, 
            indexId, 
            subscriber, 
            userData
        );
    }

    function claimTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber
    ) external {
        _idaLib.claim(token, publisher, indexId, subscriber);
    }

    function claimWithUserDataTest(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes memory userData
    ) external {
        _idaLib.claim(token, publisher, indexId, subscriber, userData);
    }

    function claimWithCtxTest(
        bytes memory ctx,
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber
    ) external returns (bytes memory) {
        return _idaLib.claimWithCtx(ctx, token, publisher, indexId, subscriber);
    }

    function claimWithCtxUserDataTest(
        bytes memory ctx,
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes memory userData
    ) external returns (bytes memory) {
        return _idaLib.claimWithCtx(ctx, token, publisher, indexId, subscriber, userData);
    }
}
