import { Address } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import {
    IndexCreated,
    IndexUpdated,
    SubscriptionApproved,
    SubscriptionRevoked,
    SubscriptionUnitsUpdated,
} from "../../generated/schema";
import {
    createEventID,
    getOrInitIndex,
    getOrInitSubscriber,
    getOrInitTokenStatistic,
    updateATSBalance,
    updateAggregateIDASubscriptionsData,
    updateTokenStatisticIDAAccountsData,
    BIG_INT_ZERO,
    getSubscriberID,
    subscriptionExists,
} from "../utils";

export function handleIndexCreated(event: IndexCreatedEvent): void {
    let index = getOrInitIndex(
        event,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );
    index.userData = event.params.userData;
    index.save();

    // TODO: we will only track active indexes, remove this from here, but increment
    // when units are actually distributed below
    // let tokenStatistic = getOrInitTokenStatistic(event.params.token.toHex());
    // tokenStatistic.totalNumberOfIndexes =
    //     tokenStatistic.totalNumberOfIndexes + 1;
    // tokenStatistic.save();

    createIndexCreatedEntity(event);
}

export function handleIndexUpdated(event: IndexUpdatedEvent): void {
    let currentTimestamp = event.block.timestamp;
    let totalUnits = event.params.totalUnitsPending.plus(
        event.params.totalUnitsApproved
    );
    let distributionDelta = event.params.newIndexValue
        .minus(event.params.oldIndexValue)
        .times(totalUnits);

    // update Index entity
    let index = getOrInitIndex(
        event,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        currentTimestamp
    );
    index.userData = event.params.userData;
    index.oldIndexValue = event.params.oldIndexValue;
    index.newIndexValue = event.params.newIndexValue;
    index.totalUnitsPending = event.params.totalUnitsPending;
    index.totalUnitsApproved = event.params.totalUnitsApproved;
    index.totalUnits = totalUnits;
    index.totalAmountDistributed =
        index.totalAmountDistributed.plus(distributionDelta);
    index.save();

    let tokenStatistic = getOrInitTokenStatistic(
        event.params.token.toHex(),
        currentTimestamp
    );
    tokenStatistic.totalAmountDistributed =
        tokenStatistic.totalAmountDistributed.plus(distributionDelta);
    tokenStatistic.updatedAt = currentTimestamp;
    tokenStatistic.save();

    updateATSBalance(
        event.params.publisher.toHex(),
        event.params.token.toHex(),
        currentTimestamp
    );

    createIndexUpdatedEntity(event);
}

export function handleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    let currentTimestamp = event.block.timestamp;
    let index = getOrInitIndex(
        event,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        currentTimestamp
    );

    let subscriber = getOrInitSubscriber(
        event,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        currentTimestamp
    );

    let balanceDelta = index.newIndexValue
        .minus(subscriber.lastIndexValue)
        .times(subscriber.units);

    subscriber.userData = event.params.userData;
    subscriber.approved = true;
    subscriber.lastIndexValue = index.newIndexValue;

    let tokenId = event.params.token.toHex();

    let subscriberId = getSubscriberID(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    let hasSubscription = subscriptionExists(subscriberId);

    if (hasSubscription) {
        index.totalUnitsApproved = index.totalUnitsApproved.plus(
            subscriber.units
        );
        index.totalUnitsPending = index.totalUnitsPending.minus(
            subscriber.units
        );
        index.save();

        updateTokenStatisticIDAAccountsData(
            event.params.token.toHex(),
            subscriber.units,
            subscriber.units.neg(),
            currentTimestamp
        );

        subscriber.totalAmountReceivedUntilUpdatedAt =
            subscriber.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);
        subscriber.save();

        // trade-off of using balanceOf vs. doing calculations locally for most accurate data
        updateATSBalance(
            event.params.publisher.toHex(),
            tokenId,
            currentTimestamp
        );
        updateATSBalance(
            event.params.subscriber.toHex(),
            tokenId,
            currentTimestamp
        );
    }

    updateAggregateIDASubscriptionsData(
        event.params.subscriber.toHex(),
        event.params.token.toHex(),
        hasSubscription,
        false,
        true,
        currentTimestamp
    );

    createSubscriptionApprovedEntity(event);
}

export function handleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    let isRevoke = event.params.subscriber.equals(Address.fromI32(0));
    let currentTimestamp = event.block.timestamp;

    let index = getOrInitIndex(
        event,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        currentTimestamp
    );

    let subscriber = getOrInitSubscriber(
        event,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        currentTimestamp
    );

    let balanceDelta = index.newIndexValue
        .minus(subscriber.lastIndexValue)
        .times(subscriber.units);

    if (isRevoke) {
        index.totalUnitsApproved = index.totalUnitsApproved.minus(
            subscriber.units
        );
        index.totalUnitsPending = index.totalUnitsPending.plus(
            subscriber.units
        );
        subscriber.lastIndexValue = index.newIndexValue;

        updateTokenStatisticIDAAccountsData(
            event.params.token.toHex(),
            subscriber.units.neg(),
            subscriber.units,
            currentTimestamp
        );
        updateAggregateIDASubscriptionsData(
            event.params.subscriber.toHex(),
            event.params.token.toHex(),
            true,
            false,
            false,
            currentTimestamp
        );
    } else {
        // deleting subscription
        updateAggregateIDASubscriptionsData(
            event.params.subscriber.toHex(),
            event.params.token.toHex(),
            true,
            true,
            false,
            currentTimestamp
        );
        index.totalSubscribers = index.totalSubscribers - 1;
    }

    // occurs on revoke or delete
    subscriber.totalAmountReceivedUntilUpdatedAt =
        subscriber.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);
    subscriber.userData = event.params.userData;
    subscriber.approved = false;

    index.save();
    subscriber.save();

    createSubscriptionRevokedEntity(event);
}

/**
 * This always gets called with handleIndexUnitsUpdated.
 * @param event
 */
export function handleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    let currentTimestamp = event.block.timestamp;

    let subscriber = getOrInitSubscriber(
        event,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        currentTimestamp
    );

    let index = getOrInitIndex(
        event,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        currentTimestamp
    );
    let units = event.params.units;
    let isDeleteSubscription = units.equals(BIG_INT_ZERO);
    let subscriberId = getSubscriberID(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    let hasSubscription = subscriptionExists(subscriberId);

    // handle deletion in _revokeOrDeleteSubscription function
    if (isDeleteSubscription) {
        if (subscriber.approved) {
            index.totalUnitsApproved = index.totalUnitsApproved.minus(
                subscriber.units
            );
            updateTokenStatisticIDAAccountsData(
                event.params.token.toHex(),
                subscriber.units.neg(),
                BIG_INT_ZERO,
                currentTimestamp
            );
        } else {
            index.totalUnitsPending = index.totalUnitsPending.minus(
                subscriber.units
            );
            updateTokenStatisticIDAAccountsData(
                event.params.token.toHex(),
                BIG_INT_ZERO,
                subscriber.units.neg(),
                currentTimestamp
            );
        }
        // NOTE: we don't update the totalReceivedUntilLastUpdate in this
        // block of code as handleSubscriptionRevoked does that for
        // revoke and deletion of subscription and SubscriptionRevoked
        // event is emmitted if this block of code runs.
    } else {
        // is updateSubscription
        let totalUnitsDelta = units.minus(subscriber.units);

        if (hasSubscription && subscriber.approved) {
            index.totalUnitsApproved =
                index.totalUnitsApproved.plus(totalUnitsDelta);
            updateTokenStatisticIDAAccountsData(
                event.params.token.toHex(),
                totalUnitsDelta,
                BIG_INT_ZERO,
                currentTimestamp
            );
        } else if (hasSubscription) {
            index.totalUnitsPending =
                index.totalUnitsPending.plus(totalUnitsDelta);
            updateTokenStatisticIDAAccountsData(
                event.params.token.toHex(),
                BIG_INT_ZERO,
                totalUnitsDelta,
                currentTimestamp
            );
        } else {
            index.totalUnitsPending = index.totalUnitsPending.plus(units);
            index.totalSubscribers = index.totalSubscribers + 1;
            updateTokenStatisticIDAAccountsData(
                event.params.token.toHex(),
                BIG_INT_ZERO,
                units,
                currentTimestamp
            );
            updateAggregateIDASubscriptionsData(
                event.params.subscriber.toHex(),
                event.params.token.toHex(),
                hasSubscription,
                false,
                false,
                currentTimestamp
            );
        }

        let balanceDelta = index.newIndexValue
            .minus(subscriber.lastIndexValue)
            .times(subscriber.units);

        // if approved, we increment totalAmountReceivedUntilUpdatedAt
        if (subscriber.approved) {
            subscriber.totalAmountReceivedUntilUpdatedAt =
                subscriber.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);
        }
    }

    // NOTE: this handles the balance updates for both updateSubscription and
    // revokeOrDeleteSubscription functions
    if (!subscriber.approved) {
        updateATSBalance(
            event.params.publisher.toHex(),
            event.params.token.toHex(),
            currentTimestamp
        );
    }
    updateATSBalance(
        subscriber.subscriber,
        event.params.token.toHex(),
        currentTimestamp
    );

    subscriber.lastIndexValue = index.newIndexValue;
    subscriber.units = event.params.units;

    index.save();
    subscriber.save();

    createSubscriptionUnitsUpdatedEntity(event);
}

/**************************************************************************
 * Create Event Entity Helper Functions
 *************************************************************************/
function createIndexCreatedEntity(event: IndexCreatedEvent): void {
    let ev = new IndexCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

function createIndexUpdatedEntity(event: IndexUpdatedEvent): void {
    let ev = new IndexUpdated(createEventID(event));
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
    ev.save();
}

function createSubscriptionApprovedEntity(
    event: SubscriptionApprovedEvent
): void {
    let ev = new SubscriptionApproved(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

function createSubscriptionRevokedEntity(
    event: SubscriptionRevokedEvent
): void {
    let ev = new SubscriptionRevoked(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

function createSubscriptionUnitsUpdatedEntity(
    event: SubscriptionUnitsUpdatedEvent
): void {
    let ev = new SubscriptionUnitsUpdated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.save();
}
