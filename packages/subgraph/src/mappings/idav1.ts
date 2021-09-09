import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../generated/IInstantDistributionAgreementV1/IInstantDistributionAgreementV1";
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
    getOrInitTokenStats,
    updateATSBalance,
    updateAggregateIDASubscriptionsData,
    updateATSIDAUnitsData,
    updateTokenStatsIDAUnitsData,
    BIG_INT_ZERO,
} from "../utils";

export function handleIndexCreated(event: IndexCreatedEvent): void {
    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );
    index.userData = event.params.userData;
    index.save();

    createIndexCreatedEntity(event);
}

export function handleIndexUpdated(event: IndexUpdatedEvent): void {
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
        event.block.timestamp
    );
    index.userData = event.params.userData;
    index.oldIndexValue = event.params.oldIndexValue;
    index.newIndexValue = event.params.newIndexValue;
    index.totalUnitsPending = event.params.totalUnitsPending;
    index.totalUnitsApproved = event.params.totalUnitsApproved;
    index.totalUnits = totalUnits;
    index.totalUnitsDistributed =
        index.totalUnitsDistributed.plus(distributionDelta);
    index.save();

    let tokenStats = getOrInitTokenStats(event.params.token.toHex());
    tokenStats.totalUnitsDistributed =
        tokenStats.totalUnitsDistributed.plus(distributionDelta);

    updateATSBalance(
        event.params.publisher.toHex(),
        event.params.token.toHex()
    );

    createIndexUpdatedEntity(event);
}

export function handleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    // this first part occurs whether or not a subscription exists
    let [subscriber, subscriptionExists] = getOrInitSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    subscriber.userData = event.params.userData;
    subscriber.approved = true;
    subscriber.lastIndexValue = index.newIndexValue;

    // handles the vars.subscriptionExists case
    let tokenId = event.params.token.toHex();

    if (subscriptionExists) {
        index.totalUnitsApproved = index.totalUnitsApproved.plus(
            subscriber.units
        );
        index.totalUnitsPending = index.totalUnitsPending.minus(
            subscriber.units
        );
        index.save();

        updateTokenStatsIDAUnitsData(
            event.params.token.toHex(),
            subscriber.units,
            subscriber.units.neg()
        );

        let totalReceivedDelta = subscriber.totalUnitsPendingApproval;

        subscriber.totalUnitsReceivedUntilUpdatedAt =
            subscriber.totalUnitsReceivedUntilUpdatedAt.plus(
                totalReceivedDelta
            );
        subscriber.totalUnitsPendingApproval = new BigInt(0);
        subscriber.save();

        // trade-off of using balanceOf vs. doing calculations locally for most accurate data
        updateATSBalance(event.params.publisher.toHex(), tokenId);
        updateATSBalance(event.params.subscriber.toHex(), tokenId);
        updateATSIDAUnitsData(
            event.params.subscriber.toHex(),
            event.params.token.toHex(),
            totalReceivedDelta,
            totalReceivedDelta.neg()
        );
    }

    updateAggregateIDASubscriptionsData(
        event.params.subscriber.toHex(),
        event.params.token.toHex(),
        subscriptionExists,
        false,
        true
    );

    createSubscriptionApprovedRevokedEntity(event, true);
}

export function handleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    let isRevoke = event.params.subscriber == Address.fromI32(0);
    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    let [subscriber] = getOrInitSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    if (isRevoke) {
        index.totalUnitsApproved = index.totalUnitsApproved.minus(
            subscriber.units
        );
        index.totalUnitsPending = index.totalUnitsPending.plus(
            subscriber.units
        );
        subscriber.lastIndexValue = index.newIndexValue;

        updateTokenStatsIDAUnitsData(
            event.params.token.toHex(),
            subscriber.units.neg(),
            subscriber.units
        );
        updateAggregateIDASubscriptionsData(
            event.params.subscriber.toHex(),
            event.params.token.toHex(),
            true,
            false,
            false
        );
    } else {
        updateAggregateIDASubscriptionsData(
            event.params.subscriber.toHex(),
            event.params.token.toHex(),
            true,
            true,
            false
        );
    }

    // user should receive any pending/unclaimed units
    // and then we set this to 0
    let totalReceivedDelta = subscriber.totalUnitsPendingApproval;
    subscriber.totalUnitsReceivedUntilUpdatedAt =
        subscriber.totalUnitsReceivedUntilUpdatedAt.plus(
            subscriber.totalUnitsPendingApproval
        );
    subscriber.totalUnitsPendingApproval = new BigInt(0);

    // occurs on revoke or delete
    subscriber.userData = event.params.userData;
    subscriber.approved = false;

    updateATSIDAUnitsData(
        event.params.subscriber.toHex(),
        event.params.token.toHex(),
        totalReceivedDelta,
        totalReceivedDelta.neg()
    );

    index.save();
    subscriber.save();

    createSubscriptionApprovedRevokedEntity(event, false);
}

/**
 * This always gets called with handleIndexUnitsUpdated.
 * @param event
 */
export function handleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    let [subscriber, subscriptionExists] = getOrInitSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    let index = getOrInitIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );
    let units = event.params.units;
    let isDeleteSubscription = units === BIG_INT_ZERO;

    // handle deletion in _revokeOrDeleteSubscription function
    if (isDeleteSubscription) {
        if (subscriber.approved) {
            index.totalUnitsApproved = index.totalUnitsApproved.minus(
                subscriber.units
            );
            updateTokenStatsIDAUnitsData(
                event.params.token.toHex(),
                subscriber.units.neg(),
                BIG_INT_ZERO
            );
        } else {
            index.totalUnitsPending = index.totalUnitsPending.minus(
                subscriber.units
            );
            updateTokenStatsIDAUnitsData(
                event.params.token.toHex(),
                BIG_INT_ZERO,
                subscriber.units.neg()
            );
        }
        // NOTE: we don't update the totalReceivedUntilLastUpdate in this
        // block of code as handleSubscriptionRevoked does that for
        // revoke and deletion of subscription and SubscriptionRevoked
        // event is emmitted if this block of code runs.
    } else {
        // is updateSubscription
        let totalUnitsDelta = units.minus(subscriber.units);

        if (subscriptionExists && subscriber.approved) {
            index.totalUnitsApproved =
                index.totalUnitsApproved.plus(totalUnitsDelta);
            updateTokenStatsIDAUnitsData(
                event.params.token.toHex(),
                totalUnitsDelta,
                BIG_INT_ZERO
            );
        } else if (subscriptionExists) {
            index.totalUnitsPending =
                index.totalUnitsPending.plus(totalUnitsDelta);
            updateTokenStatsIDAUnitsData(
                event.params.token.toHex(),
                BIG_INT_ZERO,
                totalUnitsDelta
            );
        } else {
            index.totalUnitsPending = index.totalUnitsPending.plus(units);
            updateTokenStatsIDAUnitsData(
                event.params.token.toHex(),
                BIG_INT_ZERO,
                units
            );
            updateAggregateIDASubscriptionsData(
                event.params.subscriber.toHex(),
                event.params.token.toHex(),
                subscriptionExists,
                false,
                false
            );
        }

        let balanceDelta = index.newIndexValue
            .minus(subscriber.lastIndexValue)
            .times(subscriber.units);

        // if approved, totalUnitsReceivedUntilUpdatedAt will increment
        // else totalUnitsPendingApproval will increment
        if (subscriber.approved) {
            subscriber.totalUnitsReceivedUntilUpdatedAt =
                subscriber.totalUnitsReceivedUntilUpdatedAt.plus(balanceDelta);
            updateATSIDAUnitsData(
                event.params.subscriber.toHex(),
                event.params.token.toHex(),
                balanceDelta,
                BIG_INT_ZERO
            );
        } else {
            subscriber.totalUnitsPendingApproval =
                subscriber.totalUnitsPendingApproval.plus(balanceDelta);
            updateATSIDAUnitsData(
                event.params.subscriber.toHex(),
                event.params.token.toHex(),
                BIG_INT_ZERO,
                balanceDelta
            );
        }
    }

    // NOTE: this handles the balance updates for both updateSubscription and
    // revokeOrDeleteSubscription functions
    if (!subscriber.approved) {
        updateATSBalance(
            event.params.publisher.toHex(),
            event.params.token.toHex()
        );
    }
    updateATSBalance(subscriber.id, event.params.token.toHex());

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
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

function createIndexUpdatedEntity(event: IndexUpdatedEvent): void {
    let ev = new IndexUpdated(createEventID(event));
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
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

function createSubscriptionApprovedRevokedEntity(
    event: SubscriptionApprovedEvent | SubscriptionRevokedEvent,
    isApproved: boolean
): void {
    let ev = isApproved
        ? new SubscriptionApproved(createEventID(event))
        : new SubscriptionRevoked(createEventID(event));
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
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
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = event.params.token;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.save();
}
