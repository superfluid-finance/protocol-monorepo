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
 *   - A subscriber must approve the index in order to receive distributions from the publisher.
 *   - Total units of the approved subscriptions is `totalUnitsApproved`.
 *   - Total units of the non approved subscription is `totalUnitsPending`.
 *   - Distributions to a non approved subscription stays in the publisher's deposit until
 *     the subscriber approve the subscription.
 */
abstract contract IInstantDistributionAgreementV1 is ISuperAgreement {

    function agreementType() external override pure returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");
    }

    /**
     * @dev Create a new index for the publisher.
     * @param token Super token address.
     * @param indexId Id of the index.
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
     * @param publisher The publisher of the index.
     * @param indexId Id of the index.
     */
    function getIndex(
        ISuperToken token,
        address publisher,
        uint32 indexId)
            external
            view
            virtual
            returns(
                bool exist,
                uint128 indexValue,
                uint128 totalUnitsApproved,
                uint128 totalUnitsPending);

    /**
     * @dev Update index value of an index.
     * @param token Super token address.
     * @param indexId Id of the index.
     * @param indexValue Value of the index.
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
     * @param publisher The publisher of the index.
     * @param indexId Id of the index.
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
     * @param indexId Id of the index.
     * @param subscriber The subscriber of the index.
     * @param units Number of units of the subscription.
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
     * @param publisher The publisher of the index.
     * @param indexId Id of the index.
     * @param subscriber The subscriber of the index.
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
     * @param subscriber The user, a subscriber.
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
