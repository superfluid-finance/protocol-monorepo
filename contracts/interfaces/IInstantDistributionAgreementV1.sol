// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

import "./ISuperToken.sol";
import "./ISuperAgreement.sol";

/**
 * @title Superfluid's instant distribution agreement interface
 * @author Superfluid
 *
 * Notes:
 *   - `indexId` is deliberately limited to 32 bits, to avoid the chance for sha-3 collision.
 *     Despite knowing sha-3 collision is only theoratical.
 */
abstract contract IInstantDistributionAgreementV1 is ISuperAgreement {

    function agreementType() external override pure returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");
    }

    /**
     * @dev Create a new index for the publisher.
     * @param token Super token address.
     */
    function createIndex(
        ISuperToken token,
        uint32 indexId,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    /**
     * @dev Query the data of a index.
     * @param token Super token address.
     */
    function getIndex(
        ISuperToken token,
        address publisher,
        uint32 indexId)
            external
            view
            virtual
            returns(uint128 indexValue, uint128 totalUnits);

    /**
     * @dev Update index value of an index.
     * @param token Super token address.
     */
    function updateIndex(
        ISuperToken token,
        uint32 indexId,
        uint128 indexValue,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    /**
     * @dev Approve the subscription of an index.
     * @param token Super token address.
     */
    function approveSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    /**
     * @dev Update the nuber of units of a subscription.
     * @param token Super token address.
     */
    function updateSubscription(
        ISuperToken token,
        uint32 indexId,
        address subscriber,
        uint128 units,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    /**
     * @dev Get the number of units of a subscription.
     * @param token Super token address.
     */
    function getSubscriptionUnits(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber)
            external
            view
            virtual
            returns(uint128 units);

    /**
     * @dev List subscriptions of an user.
     * @param token Super token address.
     */
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
