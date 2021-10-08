import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    IndexCreated,
    IndexUpdated,
    SubscriptionApproved,
    SubscriptionRevoked,
    SubscriptionUnitsUpdated,
} from "../../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import {
    IndexCreatedEvent,
    IndexUpdatedEvent,
    SubscriptionApprovedEvent,
    SubscriptionRevokedEvent,
    SubscriptionUnitsUpdatedEvent,
} from "../../../generated/schema";
import {
    createEventID,
    getOrInitIndex,
    getOrInitSubscription,
    getOrInitTokenStatistic,
    updateATSBalanceAndUpdatedAt,
    updateAggregateIDASubscriptionsData,
    BIG_INT_ZERO,
    getSubscriptionID,
    subscriptionExists,
    updateAccountUpdatedAt,
    tokenHasValidHost,
    updateTokenStatsStreamedUntilUpdatedAt,
    updateATSStreamedUntilUpdatedAt,
} from "../../utils";

export function handleIndexCreated(
    event: IndexCreated,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let currentTimestamp = event.block.timestamp;
    let indexCreatedId = createEventID(event);
    let index = getOrInitIndex(
        hostAddress,
        resolverAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        indexCreatedId
    );
    index.save();

    // update streamed until updated at field
    updateTokenStatsStreamedUntilUpdatedAt(
        event.params.token.toHex(),
        event.block
    );

    let tokenStatistic = getOrInitTokenStatistic(
        event.params.token.toHex(),
        event.block
    );
    tokenStatistic.totalNumberOfIndexes =
        tokenStatistic.totalNumberOfIndexes + 1;
    tokenStatistic.updatedAtTimestamp = currentTimestamp;
    tokenStatistic.updatedAtBlockNumber = event.block.number;
    tokenStatistic.save();

    createIndexCreatedEntity(event, index.id);
}

export function handleIndexUpdated(
    event: IndexUpdated,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let totalUnits = event.params.totalUnitsPending.plus(
        event.params.totalUnitsApproved
    );
    let distributionDelta = event.params.newIndexValue
        .minus(event.params.oldIndexValue)
        .times(totalUnits);

    // update Index entity
    let index = getOrInitIndex(
        hostAddress,
        resolverAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );
    let previousTotalAmountDistributed =
        index.totalAmountDistributedUntilUpdatedAt;
    index.oldIndexValue = event.params.oldIndexValue;
    index.newIndexValue = event.params.newIndexValue;
    index.totalUnitsPending = event.params.totalUnitsPending;
    index.totalUnitsApproved = event.params.totalUnitsApproved;
    index.totalUnits = totalUnits;
    index.totalAmountDistributedUntilUpdatedAt =
        previousTotalAmountDistributed.plus(distributionDelta);
    index.save();

    updateTokenStatsStreamedUntilUpdatedAt(
        event.params.token.toHex(),
        event.block
    );

    let tokenStatistic = getOrInitTokenStatistic(
        event.params.token.toHex(),
        event.block
    );

    // Note: only increment active index the first time distribution occurs.
    if (previousTotalAmountDistributed.equals(BIG_INT_ZERO)) {
        tokenStatistic.totalNumberOfActiveIndexes =
            tokenStatistic.totalNumberOfActiveIndexes + 1;
    }

    tokenStatistic.totalAmountDistributedUntilUpdatedAt =
        tokenStatistic.totalAmountDistributedUntilUpdatedAt.plus(
            distributionDelta
        );
    tokenStatistic.updatedAtTimestamp = event.block.timestamp;
    tokenStatistic.updatedAtBlockNumber = event.block.number;
    tokenStatistic.save();

    updateAccountUpdatedAt(hostAddress, event.params.publisher, event.block);

    updateATSStreamedUntilUpdatedAt(
        event.params.publisher.toHex(),
        event.params.token.toHex(),
        event.block
    );
    updateATSBalanceAndUpdatedAt(
        event.params.publisher.toHex(),
        event.params.token.toHex(),
        event.block
    );

    createIndexUpdatedEntity(event, index.id);
}

export function handleSubscriptionApproved(
    event: SubscriptionApproved,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let index = getOrInitIndex(
        hostAddress,
        resolverAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );

    let subscription = getOrInitSubscription(
        hostAddress,
        resolverAddress,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );

    let balanceDelta = index.newIndexValue
        .minus(subscription.indexValueUntilUpdatedAt)
        .times(subscription.units);

    subscription.approved = true;
    subscription.indexValueUntilUpdatedAt = index.newIndexValue;

    let tokenId = event.params.token.toHex();

    let hasSubscription = subscriptionExists(subscription.id);

    // this must be done whether subscription exists or not
    updateATSStreamedUntilUpdatedAt(
        event.params.subscriber.toHex(),
        tokenId,
        event.block
    );
    updateATSBalanceAndUpdatedAt(
        event.params.subscriber.toHex(),
        tokenId,
        event.block
    );

    if (hasSubscription) {
        index.totalUnitsApproved = index.totalUnitsApproved.plus(
            subscription.units
        );
        index.totalUnitsPending = index.totalUnitsPending.minus(
            subscription.units
        );
        index.save();

        subscription.totalAmountReceivedUntilUpdatedAt =
            subscription.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);

        updateATSStreamedUntilUpdatedAt(
            event.params.publisher.toHex(),
            tokenId,
            event.block
        );

        // trade-off of using balanceOf vs. doing calculations locally for most accurate data
        updateATSBalanceAndUpdatedAt(
            event.params.publisher.toHex(),
            tokenId,
            event.block
        );

        // we only update publisher data if hasSubscription is true
        updateAccountUpdatedAt(
            hostAddress,
            event.params.publisher,
            event.block
        );
    }

    subscription.save();

    updateAccountUpdatedAt(hostAddress, event.params.subscriber, event.block);

    updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

    updateAggregateIDASubscriptionsData(
        event.params.subscriber.toHex(),
        event.params.token.toHex(),
        hasSubscription,
        subscription.approved,
        false,
        true,
        event.block
    );

    createSubscriptionApprovedEntity(event, subscription.id);
}

/**
 * This function will be triggered in _revokeOrUpdateSubscription
 * as well as updateSubscription, but we only handle
 * _revokeOrUpdateSubscription - it runs whenever a subscription
 * is revoked or deleted.
 * @param event
 * @param hostAddress
 * @returns
 */
export function handleSubscriptionRevoked(
    event: SubscriptionRevoked,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let tokenId = event.params.token.toHex();
    let subscriberAddress = event.params.subscriber.toHex();

    let index = getOrInitIndex(
        hostAddress,
        resolverAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );

    let subscription = getOrInitSubscription(
        hostAddress,
        resolverAddress,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );

    let balanceDelta = index.newIndexValue
        .minus(subscription.indexValueUntilUpdatedAt)
        .times(subscription.units);

    // we only shift the balance from approved to pending for approved subscriptions
    // when you delete an approved subscription, we run through this and we clear the
    // totalUnitsPending in handleSubscriptionUnitsUpdated
    // when you delete as an unapproved user, we just subtract subscription units
    // in handleSubscriptionUnitsUpdated
    if (subscription.approved) {
        index.totalUnitsApproved = index.totalUnitsApproved.minus(
            subscription.units
        );
        index.totalUnitsPending = index.totalUnitsPending.plus(
            subscription.units
        );
    }
    subscription.indexValueUntilUpdatedAt = index.newIndexValue;

    updateATSStreamedUntilUpdatedAt(subscriberAddress, tokenId, event.block);
    updateATSBalanceAndUpdatedAt(subscriberAddress, tokenId, event.block);
    updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

    updateAggregateIDASubscriptionsData(
        subscriberAddress,
        tokenId,
        true,
        subscription.approved,
        false,
        false,
        event.block
    );
    // mimic ida logic more closely
    if (!subscription.approved) {
        updateATSStreamedUntilUpdatedAt(
            event.params.publisher.toHex(),
            tokenId,
            event.block
        );
        updateATSBalanceAndUpdatedAt(
            event.params.publisher.toHex(),
            tokenId,
            event.block
        );

        updateAccountUpdatedAt(
            hostAddress,
            event.params.publisher,
            event.block
        );
    }

    // occurs on revoke or delete
    subscription.totalAmountReceivedUntilUpdatedAt =
        subscription.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);
    subscription.approved = false;

    index.save();
    subscription.save();

    updateAccountUpdatedAt(hostAddress, event.params.subscriber, event.block);

    createSubscriptionRevokedEntity(event, subscription.id);
}

/**
 * This function will be triggered in _revokeOrUpdateSubscription
 * as well as updateSubscription, but we only handle
 * updateSubscription.
 * @param event
 */
export function handleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdated,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }
    let tokenId = event.params.token.toHex();

    let subscription = getOrInitSubscription(
        hostAddress,
        resolverAddress,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );

    let index = getOrInitIndex(
        hostAddress,
        resolverAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );
    let units = event.params.units;
    let oldUnits = subscription.units;
    let subscriptionId = getSubscriptionID(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    let hasSubscription = subscriptionExists(subscriptionId);

    // we only handle updateSubscription in this function
    // is updateSubscription
    let totalUnitsDelta = units.minus(subscription.units);

    if (hasSubscription && subscription.approved) {
        index.totalUnitsApproved =
            index.totalUnitsApproved.plus(totalUnitsDelta);
        index.totalUnits = index.totalUnits.plus(totalUnitsDelta);
    } else if (hasSubscription) {
        index.totalUnitsPending = index.totalUnitsPending.plus(totalUnitsDelta);
        index.totalUnits = index.totalUnits.plus(totalUnitsDelta);
	// user has no subscription (only occurs on updateUnits, can't occur on revoke or delete)
    } else {
        // create unallocated subscription
        subscription.indexId = event.params.indexId;
        subscription.units = event.params.units;
        subscription.indexValueUntilUpdatedAt = index.newIndexValue;

        index.totalUnitsPending = index.totalUnitsPending.plus(units);
        index.totalUnits = index.totalUnits.plus(units);
        index.totalSubscriptions = index.totalSubscriptions + 1;

        updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

        updateAggregateIDASubscriptionsData(
            event.params.subscriber.toHex(),
            tokenId,
            hasSubscription,
            subscription.approved,
            false,
            false,
            event.block
        );
    }

    let balanceDelta = index.newIndexValue
        .minus(subscription.indexValueUntilUpdatedAt)
        .times(subscription.units);

    // token.settleBalance should be the trigger for updating
    // totalAmountReceivedUntilUpdatedAt and calling
    // updateATSBalanceAndUpdatedAt
    subscription.totalAmountReceivedUntilUpdatedAt =
        subscription.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);

    // We move both of these in here as we handle this in revoke or delete
    // as well, so if we put it outside it will be a duplicate call
    if (!subscription.approved) {
        updateATSStreamedUntilUpdatedAt(
            event.params.publisher.toHex(),
            tokenId,
            event.block
        );
        updateATSBalanceAndUpdatedAt(
            event.params.publisher.toHex(),
            tokenId,
            event.block
        );
        updateAccountUpdatedAt(
            hostAddress,
            event.params.publisher,
            event.block
        );
    }

    updateATSStreamedUntilUpdatedAt(
        event.params.subscriber.toHex(),
        tokenId,
        event.block
    );
    updateATSBalanceAndUpdatedAt(subscription.subscriber, tokenId, event.block);
    updateAccountUpdatedAt(hostAddress, event.params.subscriber, event.block);

    // we only update subscription units in updateSubscription
    // if user hasSubscription
    if (hasSubscription) {
        subscription.indexValueUntilUpdatedAt = index.newIndexValue;
        subscription.units = event.params.units;
    }

    // when units are set to 0, the graph marks this as a deletion
    // and therefore subtracts the number of totalSubscriptions and
    // totalApprovedSubscriptions
    if (units.equals(BIG_INT_ZERO)) {
        updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

        updateAggregateIDASubscriptionsData(
            subscription.subscriber,
            tokenId,
            true,
            subscription.approved,
            true,
            false,
            event.block
        );
        index.totalSubscriptions = index.totalSubscriptions - 1;
    }

    index.save();
    subscription.save();

    createSubscriptionUnitsUpdatedEntity(event, subscription.id, oldUnits);
}

/**************************************************************************
 * Create Event Entity Helper Functions
 *************************************************************************/
function createIndexCreatedEntity(event: IndexCreated, indexId: string): void {
    let ev = new IndexCreatedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.index = indexId;
    ev.save();
}

function createIndexUpdatedEntity(event: IndexUpdated, indexId: string): void {
    let ev = new IndexUpdatedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.oldIndexValue = event.params.oldIndexValue;
    ev.newIndexValue = event.params.newIndexValue;
    ev.totalUnitsPending = event.params.totalUnitsPending;
    ev.totalUnitsApproved = event.params.totalUnitsApproved;
    ev.userData = event.params.userData;
    ev.index = indexId;
    ev.save();
}

function createSubscriptionApprovedEntity(
    event: SubscriptionApproved,
    subscriptionId: string
): void {
    let ev = new SubscriptionApprovedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.subscription = subscriptionId;
    ev.save();
}

function createSubscriptionRevokedEntity(
    event: SubscriptionRevoked,
    subscriptionId: string
): void {
    let ev = new SubscriptionRevokedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.subscription = subscriptionId;
    ev.save();
}

function createSubscriptionUnitsUpdatedEntity(
    event: SubscriptionUnitsUpdated,
    subscriptionId: string,
    oldUnits: BigInt
): void {
    let ev = new SubscriptionUnitsUpdatedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.subscription = subscriptionId;
    ev.oldUnits = oldUnits;
    ev.save();
}
