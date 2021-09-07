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
    Subscriber,
    SubscriptionApproved,
    SubscriptionRevoked,
    SubscriptionUnitsUpdated,
} from "../../generated/schema";
import {
    createTxnAndReturn,
    createEventID,
    getOrInitializeIndex,
    getSubscriber,
    updateBalance,
    getSubscriberID,
} from "../utils";

export function handleIndexCreated(event: IndexCreatedEvent): void {
    let index = getOrInitializeIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );
    index.userData = event.params.userData;
    index.save();

    let ev = new IndexCreated(createEventID(event));
    ev.transaction = createTxnAndReturn(event).id;
    ev.token = event.params.token.toHex();
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

export function handleIndexUpdated(event: IndexUpdatedEvent): void {
    let totalUnits = event.params.totalUnitsPending.plus(
        event.params.totalUnitsApproved
    );
    let distributionSinceOldIndex = event.params.newIndexValue
        .minus(event.params.oldIndexValue)
        .times(totalUnits);

    // update Index entity
    let index = getOrInitializeIndex(
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
    index.totalUnitsDistributed = index.totalUnitsDistributed.plus(
        distributionSinceOldIndex
    );
    index.save();

    // create IndexUpdated event entity
    let ev = new IndexUpdated(createEventID(event));
    ev.transaction = createTxnAndReturn(event).id;
    ev.token = event.params.token.toHex();
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.oldIndexValue = event.params.oldIndexValue;
    ev.newIndexValue = event.params.newIndexValue;
    ev.totalUnitsPending = event.params.totalUnitsPending;
    ev.totalUnitsApproved = event.params.totalUnitsApproved;
    ev.userData = event.params.userData;
    ev.save();
}

// it appears we update the users' balance upon approval if a subscription exists
// otherwise the units are set to 0, but there is nowhere to get this information
export function handleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    let subscriberEntityId = getSubscriberID(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    let subscriptionExists = Subscriber.load(subscriberEntityId) != null;

    // handles the !vars.subscriptionExists case
    let subscriber = getSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    subscriber.userData = event.params.userData;
    subscriber.approved = true;
    let index = getOrInitializeIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );
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

        // TODO: maybe this is supposed to be the delta?
        subscriber.totalReceivedUntilLastUpdate =
            subscriber.totalReceivedUntilLastUpdate.plus(
                subscriber.totalPendingApproval
            );
        subscriber.totalPendingApproval = new BigInt(0);
        subscriber.save();

        // trade-off of using balanceOf vs. doing calculations locally for most accurate data
        updateBalance(event.params.publisher.toHex(), tokenId);
        updateBalance(event.params.subscriber.toHex(), tokenId);
    }

    // create SubscriptionApproved event entity
    let ev = new SubscriptionApproved(createEventID(event));
    ev.transaction = createTxnAndReturn(event).id;
    ev.token = tokenId;
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

export function handleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    let isRevoke = event.params.subscriber == Address.fromI32(0);
    let index = getOrInitializeIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    let subscriber = getSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    if (isRevoke) {
        index.totalUnitsApproved.minus(subscriber.units);
        index.totalUnitsPending.plus(subscriber.units);
        subscriber.lastIndexValue = index.newIndexValue;
    }

    if (!subscriber.approved) {
        updateBalance(
            event.params.publisher.toHex(),
            event.params.token.toHex()
        );
    }

    // occurs on revoke or delete
    subscriber.userData = event.params.userData;
    subscriber.approved = false;
    subscriber.totalReceivedUntilLastUpdate =
        subscriber.totalReceivedUntilLastUpdate.plus(
            subscriber.totalPendingApproval
        );
    subscriber.totalPendingApproval = new BigInt(0);

    index.save();
    subscriber.save();

    // update balance for subscriber/publisher
    updateBalance(event.params.subscriber.toHex(), event.params.token.toHex());

    let ev = new SubscriptionRevoked(createEventID(event));
    ev.transaction = createTxnAndReturn(event).id;
    ev.token = event.params.token.toHex();
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

// IsDeleteSubscription on revokeOrDelete
// updateSubscription
/**
 * This always gets called with handleIndexUnitsUpdated.
 * @param event
 */
export function handleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    let subscriber = getSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );

    let index = getOrInitializeIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId,
        event.block.timestamp
    );
    let subscriberEntityId = getSubscriberID(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    let subscriptionExists = Subscriber.load(subscriberEntityId) != null;
    let units = event.params.units;
    let isDeleteSubscription = units === BigInt.fromI32(0);

    if (isDeleteSubscription) {
        if (subscriber.approved) {
            index.totalUnitsApproved = index.totalUnitsApproved.minus(
                subscriber.units
            );
        } else {
            index.totalUnitsPending = index.totalUnitsPending.minus(
                subscriber.units
            );
        }

    // is updateSubscription
    } else {
        if (subscriptionExists && subscriber.approved) {
            index.totalUnitsApproved = index.totalUnitsApproved
                .plus(units)
                .minus(subscriber.units);
        } else if (subscriptionExists) {
            index.totalUnitsPending = index.totalUnitsPending
                .plus(units)
                .minus(subscriber.units);
        } else {
            index.totalUnitsPending = index.totalUnitsPending.plus(units);
        }

        let balanceDelta = index.newIndexValue
            .minus(subscriber.lastIndexValue)
            .times(subscriber.units);

        // if the subscriber is approved, totalPendingApproval will not increment
        // their totalReceivedUnits would incrememnt
        if (subscriber.approved) {
            subscriber.totalReceivedUntilLastUpdate =
                subscriber.totalReceivedUntilLastUpdate.plus(balanceDelta);
        } else {
            subscriber.totalPendingApproval =
                subscriber.totalPendingApproval.plus(balanceDelta);
            updateBalance(
                event.params.publisher.toHex(),
                event.params.token.toHex()
            );
        }

        updateBalance(subscriber.id, event.params.token.toHex());
    }

    subscriber.lastIndexValue = index.newIndexValue;
    subscriber.units = event.params.units;
    index.save();
    subscriber.save();

    let ev = new SubscriptionUnitsUpdated(createEventID(event));
    ev.transaction = createTxnAndReturn(event).id;
    ev.token = event.params.token.toHex();
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.save();
}
