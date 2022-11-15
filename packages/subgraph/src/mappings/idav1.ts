import { BigInt } from "@graphprotocol/graph-ts";
import {
    IndexCreated,
    IndexDistributionClaimed,
    IndexSubscribed,
    IndexUnitsUpdated,
    IndexUnsubscribed,
    IndexUpdated,
    SubscriptionApproved,
    SubscriptionDistributionClaimed,
    SubscriptionRevoked,
    SubscriptionUnitsUpdated,
} from "../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import {
    IndexCreatedEvent,
    IndexDistributionClaimedEvent,
    IndexSubscribedEvent,
    IndexUnitsUpdatedEvent,
    IndexUnsubscribedEvent,
    IndexUpdatedEvent,
    SubscriptionApprovedEvent,
    SubscriptionDistributionClaimedEvent,
    SubscriptionRevokedEvent,
    SubscriptionUnitsUpdatedEvent,
} from "../../generated/schema";
import {
    BIG_INT_ZERO,
    createEventID,
    getIndexID,
    initializeEventEntity,
    subscriptionExists as subscriptionWithUnitsExists,
    tokenHasValidHost,
} from "../utils";
import {
    _createAccountTokenSnapshotLogEntity,
    _createTokenStatisticLogEntity,
    getOrInitIndex,
    getOrInitSubscription,
    getOrInitTokenStatistic,
    updateAggregateIDASubscriptionsData,
    updateATSStreamedAndBalanceUntilUpdatedAt,
    updateTokenStatsStreamedUntilUpdatedAt,
} from "../mappingHelpers";
import { getHostAddress } from "../addresses";

/******************
 * Event Handlers *
 *****************/
export function handleIndexCreated(event: IndexCreated): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let currentTimestamp = event.block.timestamp;
    let indexCreatedId = createEventID("IndexCreated", event);
    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        indexCreatedId
    );
    index.save();

    // update streamed until updated at field
    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);

    let tokenStatistic = getOrInitTokenStatistic(
        event.params.token,
        event.block
    );
    tokenStatistic.totalNumberOfIndexes =
        tokenStatistic.totalNumberOfIndexes + 1;
    tokenStatistic.updatedAtTimestamp = currentTimestamp;
    tokenStatistic.updatedAtBlockNumber = event.block.number;
    tokenStatistic.save();

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.publisher,
        event.params.token,
        event.block,
        null // will do RPC if any units exist anyways (balance isn't impacted by index creation)
    );

    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.publisher,
        event.params.token,
        "IndexCreated"
    );
    _createTokenStatisticLogEntity(event, event.params.token, "IndexCreated");
    _createIndexCreatedEventEntity(event, index.id);
}

export function handleIndexDistributionClaimed(
    event: IndexDistributionClaimed
): void {
    let indexId = getIndexID(
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    _createIndexDistributionClaimedEventEntity(event, indexId);
}

export function handleIndexUpdated(event: IndexUpdated): void {
    let hostAddress = getHostAddress();
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
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );
    let previousTotalAmountDistributed =
        index.totalAmountDistributedUntilUpdatedAt;
    index.indexValue = event.params.newIndexValue;
    index.totalUnitsPending = event.params.totalUnitsPending;
    index.totalUnitsApproved = event.params.totalUnitsApproved;
    index.totalUnits = totalUnits;
    index.totalAmountDistributedUntilUpdatedAt =
        previousTotalAmountDistributed.plus(distributionDelta);
    index.save();

    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);

    let tokenStatistic = getOrInitTokenStatistic(
        event.params.token,
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

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.publisher,
        event.params.token,
        event.block,
        null // will do RPC if any units exist anyways
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.publisher,
        event.params.token,
        "IndexUpdated"
    );
    _createTokenStatisticLogEntity(event, event.params.token, "IndexUpdated");
    _createIndexUpdatedEventEntity(event, index.id);
}

export function handleIndexSubscribed(event: IndexSubscribed): void {
    let indexId = getIndexID(
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    _createIndexSubscribedEventEntity(event, indexId);
}

export function handleIndexUnitsUpdated(event: IndexUnitsUpdated): void {
    let indexId = getIndexID(
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    let subscription = getOrInitSubscription(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );
    _createIndexUnitsUpdatedEventEntity(event, indexId, subscription.units);
}

export function handleIndexUnsubscribed(event: IndexUnsubscribed): void {
    let indexId = getIndexID(
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    _createIndexUnsubscribedEventEntity(event, indexId);
}

export function handleSubscriptionApproved(event: SubscriptionApproved): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );

    let subscription = getOrInitSubscription(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );

    let balanceDelta = index.indexValue
        .minus(subscription.indexValueUntilUpdatedAt)
        .times(subscription.units);

    subscription.approved = true;
    subscription.indexValueUntilUpdatedAt = index.indexValue;

    let hasSubscriptionWithUnits = subscriptionWithUnitsExists(subscription.id);

    // this must be done whether subscription exists or not
    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.subscriber,
        event.params.token,
        event.block,
        null // will do RPC if any units exist anyways
    );

    if (hasSubscriptionWithUnits) {
        index.totalUnitsApproved = index.totalUnitsApproved.plus(
            subscription.units
        );
        index.totalUnitsPending = index.totalUnitsPending.minus(
            subscription.units
        );

        subscription.totalAmountReceivedUntilUpdatedAt =
            subscription.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);

        updateATSStreamedAndBalanceUntilUpdatedAt(
            event.params.publisher,
            event.params.token,
            event.block,
            null // will do RPC if any units exist anyways
        );
        _createAccountTokenSnapshotLogEntity(
            event,
            event.params.publisher,
            event.params.token,
            "SubscriptionApproved"
        );
    }

    subscription.save();

    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);

    // we only want to increment approved here ALWAYS
    updateAggregateIDASubscriptionsData(
        event.params.subscriber,
        event.params.token,
        hasSubscriptionWithUnits || subscription.approved,
        subscription.approved,
        false, // don't increment subWithUnits
        false, // not revoking
        false, // not deleting
        true, // approving subscription here
        event.block
    );
    index.save();

    _createSubscriptionApprovedEventEntity(event, subscription.id);
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.subscriber,
        event.params.token,
        "SubscriptionApproved"
    );
    _createTokenStatisticLogEntity(
        event,
        event.params.token,
        "SubscriptionApproved"
    );
}

export function handleSubscriptionDistributionClaimed(
    event: SubscriptionDistributionClaimed
): void {
    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );

    let subscription = getOrInitSubscription(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );

    let pendingDistribution = subscription.units.times(
        index.indexValue.minus(subscription.indexValueUntilUpdatedAt)
    );

    subscription.totalAmountReceivedUntilUpdatedAt =
        subscription.totalAmountReceivedUntilUpdatedAt.plus(
            pendingDistribution
        );
    subscription.indexValueUntilUpdatedAt = index.indexValue;
    subscription.save();

    _createSubscriptionDistributionClaimedEventEntity(event, subscription.id);

    // // update streamed until updated at field
    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.publisher,
        event.params.token,
        event.block,
        null // will do RPC call if they have sub w/ units
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.subscriber,
        event.params.token,
        event.block,
        null // will do RPC call if they have sub w/ units
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.publisher,
        event.params.token,
        "SubscriptionDistributionClaimed"
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.subscriber,
        event.params.token,
        "SubscriptionDistributionClaimed"
    );
    _createTokenStatisticLogEntity(
        event,
        event.params.token,
        "SubscriptionDistributionClaimed"
    );
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
export function handleSubscriptionRevoked(event: SubscriptionRevoked): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );

    // This will always execute on an existing subscription
    let subscription = getOrInitSubscription(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );

    let balanceDelta = index.indexValue
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
    subscription.indexValueUntilUpdatedAt = index.indexValue;

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.subscriber,
        event.params.token,
        event.block,
        null // will do RPC call if they have sub w/ units
    );

    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);

    updateAggregateIDASubscriptionsData(
        event.params.subscriber,
        event.params.token,
        true,
        subscription.approved,
        false, // don't increment subWithUnits
        true, // revoking subscription here
        false, // not deleting
        false, // not approving
        event.block
    );
    // mimic ida logic more closely
    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.publisher,
        event.params.token,
        event.block,
        null // will do RPC call if they have sub w/ units
    );

    // occurs on revoke or delete
    subscription.totalAmountReceivedUntilUpdatedAt =
        subscription.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);
    subscription.approved = false;

    index.save();
    subscription.save();

    _createSubscriptionRevokedEventEntity(event, subscription.id);
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.subscriber,
        event.params.token,
        "SubscriptionRevoked"
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.publisher,
        event.params.token,
        "SubscriptionRevoked"
    );
    _createTokenStatisticLogEntity(
        event,
        event.params.token,
        "SubscriptionRevoked"
    );
}

/**
 * This function will be triggered in _revokeOrUpdateSubscription
 * as well as updateSubscription, but we only handle
 * updateSubscription.
 * @param event
 */
export function handleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdated
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let subscription = getOrInitSubscription(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );

    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );
    let units = event.params.units;
    let oldUnits = subscription.units;
    let hasSubscriptionWithUnits = subscriptionWithUnitsExists(subscription.id);

    // we only handle updateSubscription in this function
    // is updateSubscription
    let totalUnitsDelta = units.minus(subscription.units);

    // if you have an approved subscription, you just add to totalUnitsApproved
    if (subscription.approved) {
        index.totalUnitsApproved =
            index.totalUnitsApproved.plus(totalUnitsDelta);
        index.totalUnits = index.totalUnits.plus(totalUnitsDelta);
        // else, you just add to the pending units
    } else {
        index.totalUnitsPending = index.totalUnitsPending.plus(totalUnitsDelta);
        index.totalUnits = index.totalUnits.plus(totalUnitsDelta);
    }

    let balanceDelta = index.indexValue
        .minus(subscription.indexValueUntilUpdatedAt)
        .times(subscription.units);

    // token.settleBalance should be the trigger for updating
    // totalAmountReceivedUntilUpdatedAt and calling
    subscription.totalAmountReceivedUntilUpdatedAt =
        subscription.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);

    // We move both of these in here as we handle this in revoke or delete
    // as well, so if we put it outside it will be a duplicate call
    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.publisher,
        event.params.token,
        event.block,
        null // will do RPC call if they have sub w/ units
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.subscriber,
        event.params.token,
        event.block,
        null // will do RPC call if they have sub w/ units
    );

    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);

    // when units are set to 0, the graph marks this as a deletion
    // and therefore subtracts the number of totalSubscriptionWithUnits and
    // totalApprovedSubscriptions
    if (units.equals(BIG_INT_ZERO)) {
        updateAggregateIDASubscriptionsData(
            event.params.subscriber,
            event.params.token,
            hasSubscriptionWithUnits,
            subscription.approved,
            false, // don't increment subWithUnits
            false, // not revoking subscription
            true, // only place we decrement subWithUnits IF subscriber has subWithUnits
            false, // not approving
            event.block
        );
        index.totalSubscriptionsWithUnits = hasSubscriptionWithUnits
            ? index.totalSubscriptionsWithUnits - 1
            : index.totalSubscriptionsWithUnits;
    }

    subscription.indexValueUntilUpdatedAt = index.indexValue;
    subscription.units = event.params.units;

    // to simplify things, we only tally subscriptions in our stats
    // if you have units - the opposite of subscription deletion
    // this only executes when someone is allocated units the first time
    if (units.gt(BIG_INT_ZERO) && oldUnits.equals(BIG_INT_ZERO)) {
        index.totalSubscriptionsWithUnits =
            index.totalSubscriptionsWithUnits + 1;

        updateAggregateIDASubscriptionsData(
            event.params.subscriber,
            event.params.token,
            hasSubscriptionWithUnits,
            subscription.approved,
            true, // only place we increment subWithUnits
            false, // not revoking
            false, // not deleting
            false, // not approving
            event.block
        );
    }

    index.save();
    subscription.save();

    _createSubscriptionUnitsUpdatedEventEntity(
        event,
        subscription.id,
        oldUnits
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.publisher,
        event.params.token,
        "SubscriptionUnitsUpdated"
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.subscriber,
        event.params.token,
        "SubscriptionUnitsUpdated"
    );
    _createTokenStatisticLogEntity(
        event,
        event.params.token,
        "SubscriptionUnitsUpdated"
    );
}

/****************************************
 * Create Event Entity Helper Functions *
 ***************************************/
function _createIndexCreatedEventEntity(
    event: IndexCreated,
    indexId: string
): void {
    const eventId = createEventID("IndexCreated", event);
    const ev = new IndexCreatedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
    ]);

    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.index = indexId;
    ev.save();
}

function _createIndexDistributionClaimedEventEntity(
    event: IndexDistributionClaimed,
    indexId: string
): void {
    const eventId = createEventID("IndexDistributionClaimed", event);
    const ev = new IndexDistributionClaimedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
        event.params.subscriber,
    ]);

    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.subscriber = event.params.subscriber;
    ev.amount = event.params.amount;
    ev.index = indexId;
    ev.save();
}

function _createIndexUpdatedEventEntity(
    event: IndexUpdated,
    indexId: string
): void {
    const eventId = createEventID("IndexUpdated", event);
    const ev = new IndexUpdatedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
    ]);

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
function _createIndexSubscribedEventEntity(
    event: IndexSubscribed,
    indexId: string
): void {
    const eventId = createEventID("IndexSubscribed", event);
    const ev = new IndexSubscribedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
        event.params.subscriber,
    ]);

    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.subscriber = event.params.subscriber;
    ev.userData = event.params.userData;
    ev.index = indexId;
    ev.save();
}

function _createIndexUnitsUpdatedEventEntity(
    event: IndexUnitsUpdated,
    indexId: string,
    oldUnits: BigInt
): void {
    const eventId = createEventID("IndexUnitsUpdated", event);
    const ev = new IndexUnitsUpdatedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
        event.params.subscriber,
    ]);

    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.oldUnits = oldUnits;
    ev.index = indexId;
    ev.save();
}

function _createIndexUnsubscribedEventEntity(
    event: IndexUnsubscribed,
    indexId: string
): void {
    const eventId = createEventID("IndexUnsubscribed", event);
    const ev = new IndexUnsubscribedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
        event.params.subscriber,
    ]);

    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.index = indexId;
    ev.save();
}

function _createSubscriptionApprovedEventEntity(
    event: SubscriptionApproved,
    subscriptionId: string
): void {
    const eventId = createEventID("SubscriptionApproved", event);
    const ev = new SubscriptionApprovedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
        event.params.subscriber,
    ]);
    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.subscription = subscriptionId;
    ev.save();
}

function _createSubscriptionDistributionClaimedEventEntity(
    event: SubscriptionDistributionClaimed,
    subscriptionId: string
): void {
    const eventId = createEventID("SubscriptionDistributionClaimed", event);
    const ev = new SubscriptionDistributionClaimedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
        event.params.subscriber,
    ]);

    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.amount = event.params.amount;
    ev.subscription = subscriptionId;
    ev.save();
}

function _createSubscriptionRevokedEventEntity(
    event: SubscriptionRevoked,
    subscriptionId: string
): void {
    const eventId = createEventID("SubscriptionRevoked", event);
    const ev = new SubscriptionRevokedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
        event.params.subscriber,
    ]);

    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.subscription = subscriptionId;
    ev.save();
}

function _createSubscriptionUnitsUpdatedEventEntity(
    event: SubscriptionUnitsUpdated,
    subscriptionId: string,
    oldUnits: BigInt
): void {
    const eventId = createEventID("SubscriptionUnitsUpdated", event);
    const ev = new SubscriptionUnitsUpdatedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.publisher,
        event.params.subscriber,
    ]);

    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.subscription = subscriptionId;
    ev.oldUnits = oldUnits;
    ev.save();
}
