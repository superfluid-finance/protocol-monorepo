// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

import "./ISuperToken.sol";
import "./ISuperAgreement.sol";

/**
 * @title Superfluid's instant distribution agreement interface
 * @author Superfluid
 *
 * Notes:
 *   - indexId is deliberately limited to 32 bits, to avoid the chance for sha-3 collision.
 *     Even if sha-3 collision is only theoratical.
 */
abstract contract IInstantDistributionAgreementV1 is ISuperAgreement {

    function createIndex(
        ISuperToken token,
        uint32 indexId,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    function getIndex(
        ISuperToken token,
        address publisher,
        uint32 indexId)
            external
            view
            virtual
            returns(uint128 indexValue, uint128 totalUnits);

    function updateIndex(
        ISuperToken token,
        uint32 indexId,
        uint128 indexValue,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    function approveSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    function updateSubscription(
        ISuperToken token,
        uint32 indexId,
        address subscriber,
        uint128 units,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    function getSubscriptionUnits(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber)
            external
            view
            virtual
            returns(uint128 units);

    function listSubscriptions(
        ISuperToken token,
        address subscriber)
            external
            view
            virtual
            returns(
                address[] memory publishers,
                uint32[] memory indexIds,
                uint128[] memory unitsList);

    /*
    function deleteSubscription(
        ISuperToken token,
        uint32 indexId,
        address subscriber,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx); */

}
