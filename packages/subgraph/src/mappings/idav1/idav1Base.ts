import { Address } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import {
    IndexCreated,
    IndexUpdated,
    SubscriptionApproved,
    SubscriptionRevoked,
    SubscriptionUnitsUpdated,
} from "../../../generated/schema";
import {
    createEventID,
    getOrInitIndex,
    getOrInitSubscriber,
    getOrInitTokenStatistic,
    updateATSBalanceAndUpdatedAt,
    updateAggregateIDASubscriptionsData,
    BIG_INT_ZERO,
    getSubscriberID,
    subscriptionExists,
    updateAccountUpdatedAt,
    tokenHasValidHost,
    updateTokenStatsStreamedUntilUpdatedAt,
    updateATSStreamedUntilUpdatedAt,
} from "../../utils";

export function handleIndexCreated(
    event: IndexCreatedEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let currentTimestamp = event.block.timestamp;
    let indexCreatedId = createEventID(event);
    let index = getOrInitIndex(
        hostAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        indexCreatedId
    );
    index.userData = event.params.userData;
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
    tokenStatistic.updatedAtBlock = event.block.number;
    tokenStatistic.save();

    createIndexCreatedEntity(event, index.id);
}

export function handleIndexUpdated(
    event: IndexUpdatedEvent,
    hostAddress: Address
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
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );
    let previousTotalAmountDistributed =
        index.totalAmountDistributedUntilUpdatedAt;
    index.userData = event.params.userData;
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
    if (previousTotalAmountDistributed === BIG_INT_ZERO) {
        tokenStatistic.totalNumberOfActiveIndexes =
            tokenStatistic.totalNumberOfActiveIndexes + 1;
    }

    tokenStatistic.totalAmountDistributedUntilUpdatedAt =
        tokenStatistic.totalAmountDistributedUntilUpdatedAt.plus(
            distributionDelta
        );
    tokenStatistic.updatedAtTimestamp = event.block.timestamp;
    tokenStatistic.updatedAtBlock = event.block.number;
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
    event: SubscriptionApprovedEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let index = getOrInitIndex(
        hostAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );

    let subscriber = getOrInitSubscriber(
        hostAddress,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
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

        subscriber.totalAmountReceivedUntilUpdatedAt =
            subscriber.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);

        updateATSStreamedUntilUpdatedAt(
            event.params.publisher.toHex(),
            tokenId,
            event.block
        );
        updateATSStreamedUntilUpdatedAt(
            event.params.subscriber.toHex(),
            tokenId,
            event.block
        );

        // trade-off of using balanceOf vs. doing calculations locally for most accurate data
        updateATSBalanceAndUpdatedAt(
            event.params.publisher.toHex(),
            tokenId,
            event.block
        );
        updateATSBalanceAndUpdatedAt(
            event.params.subscriber.toHex(),
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

    subscriber.save();

    updateAccountUpdatedAt(hostAddress, event.params.subscriber, event.block);

    updateATSStreamedUntilUpdatedAt(
        event.params.subscriber.toHex(),
        tokenId,
        event.block
    ); // we should only do this if !hasSubscription

	updateATSBalanceAndUpdatedAt(
		event.params.subscriber.toHex(),
		tokenId,
		event.block
	);

    updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

    updateAggregateIDASubscriptionsData(
        event.params.subscriber.toHex(),
        event.params.token.toHex(),
        hasSubscription,
        false,
        true,
        event.block
    );

    createSubscriptionApprovedEntity(event, subscriber.id);
}

/**
 * This function will be triggered in _revokeOrUpdateSubscription
 * as well as updateSubscription, but we only handle
 * _revokeOrUpdateSubscription.
 * @param event
 * @param hostAddress
 * @returns
 */
export function handleSubscriptionRevoked(
    event: SubscriptionRevokedEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let tokenId = event.params.token.toHex();
    let subscriberAddress = event.params.subscriber.toHex();

    let isRevoke = event.params.subscriber.equals(Address.fromI32(0));

    let index = getOrInitIndex(
        hostAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
    );

    let subscriber = getOrInitSubscriber(
        hostAddress,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
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

        updateATSStreamedUntilUpdatedAt(
            subscriberAddress,
            tokenId,
            event.block
        );
        updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

        updateAggregateIDASubscriptionsData(
            subscriberAddress,
            tokenId,
            true,
            false,
            false,
            event.block
        );
    } else {
        // deleting subscription
        if (subscriber.approved) {
            index.totalUnitsApproved = index.totalUnitsApproved.minus(
                subscriber.units
            );
        } else {
            index.totalUnitsPending = index.totalUnitsPending.minus(
                subscriber.units
            );
        }
		
		// do we set subscriber.units = 0?
		// I thought that they weren't able to set their units
		subscriber.units = BIG_INT_ZERO;

        updateATSStreamedUntilUpdatedAt(
            subscriberAddress,
            tokenId,
            event.block
        );
        updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

        updateAggregateIDASubscriptionsData(
            subscriberAddress,
            tokenId,
            true,
            true,
            false,
            event.block
        );
        index.totalSubscribers = index.totalSubscribers - 1;
    }

    // mimic ida logic more closely
    if (!subscriber.approved) {
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
    subscriber.totalAmountReceivedUntilUpdatedAt =
        subscriber.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);
    subscriber.userData = event.params.userData;
    subscriber.approved = false;

    index.save();
    subscriber.save();

    // not necessary as it's being called within revoke or delete
    // updateATSStreamedUntilUpdatedAt(subscriber.subscriber, tokenId, event.block);
    updateATSBalanceAndUpdatedAt(subscriber.subscriber, tokenId, event.block);

    updateAccountUpdatedAt(hostAddress, event.params.subscriber, event.block);

    createSubscriptionRevokedEntity(event, subscriber.id);
}

/**
 * This function will be triggered in _revokeOrUpdateSubscription
 * as well as updateSubscription, but we only handle
 * updateSubscription.
 * @param event
 */
export function handleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let subscriber = getOrInitSubscriber(
        hostAddress,
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block
    );

    let index = getOrInitIndex(
        hostAddress,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block,
        ""
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

    // we only handle updateSubscription in this function
    if (!isDeleteSubscription) {
        // is updateSubscription
        let totalUnitsDelta = units.minus(subscriber.units);

        if (hasSubscription && subscriber.approved) {
            index.totalUnitsApproved =
                index.totalUnitsApproved.plus(totalUnitsDelta);
        } else if (hasSubscription) {
            index.totalUnitsPending =
                index.totalUnitsPending.plus(totalUnitsDelta);
        } else {
            // create unallocated subscription
            subscriber.indexId = event.params.indexId;
            subscriber.units = event.params.units;
            subscriber.lastIndexValue = index.newIndexValue;

            index.totalUnitsPending = index.totalUnitsPending.plus(units);
            index.totalSubscribers = index.totalSubscribers + 1;

            updateTokenStatsStreamedUntilUpdatedAt(
                event.params.token.toHex(),
                event.block
            );

            updateAggregateIDASubscriptionsData(
                event.params.subscriber.toHex(),
                event.params.token.toHex(),
                hasSubscription,
                false,
                false,
                event.block
            );
        }

        let balanceDelta = index.newIndexValue
            .minus(subscriber.lastIndexValue)
            .times(subscriber.units);

        // token.settleBalance should be the trigger for updating
        // totalAmountReceivedUntilUpdatedAt and calling
        // updateATSBalanceAndUpdatedAt
        subscriber.totalAmountReceivedUntilUpdatedAt =
            subscriber.totalAmountReceivedUntilUpdatedAt.plus(balanceDelta);

        // We move both of these in here as we handle this in revoke or delete
        // as well, so if we put it outside it will be a duplicate call
        if (!subscriber.approved) {
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
            updateAccountUpdatedAt(
                hostAddress,
                event.params.publisher,
                event.block
            );
        }

        updateATSStreamedUntilUpdatedAt(
            event.params.subscriber.toHex(),
            event.params.token.toHex(),
            event.block
        );
        updateATSBalanceAndUpdatedAt(
            subscriber.subscriber,
            event.params.token.toHex(),
            event.block
        );
        updateAccountUpdatedAt(
            hostAddress,
            event.params.subscriber,
            event.block
        );

        // we only update subscription units in updateSubscription
        // if user hasSubscription
        if (hasSubscription) {
            subscriber.lastIndexValue = index.newIndexValue;
            subscriber.units = event.params.units;
        }
    }

    index.save();
    subscriber.save();

    createSubscriptionUnitsUpdatedEntity(event, subscriber.id);
}

/**************************************************************************
 * Create Event Entity Helper Functions
 *************************************************************************/
function createIndexCreatedEntity(
    event: IndexCreatedEvent,
    indexId: string
): void {
    let ev = new IndexCreated(createEventID(event));
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

function createIndexUpdatedEntity(
    event: IndexUpdatedEvent,
    indexId: string
): void {
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
    ev.index = indexId;
    ev.save();
}

function createSubscriptionApprovedEntity(
    event: SubscriptionApprovedEvent,
    subscriberId: string
): void {
    let ev = new SubscriptionApproved(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.subscriber = subscriberId;
    ev.save();
}

function createSubscriptionRevokedEntity(
    event: SubscriptionRevokedEvent,
    subscriberId: string
): void {
    let ev = new SubscriptionRevoked(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.subscriber = subscriberId;
    ev.save();
}

function createSubscriptionUnitsUpdatedEntity(
    event: SubscriptionUnitsUpdatedEvent,
    subscriberId: string
): void {
    let ev = new SubscriptionUnitsUpdated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.subscriber = subscriberId;
    ev.save();
}
