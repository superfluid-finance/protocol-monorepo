import { BigInt } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    IndexUnitsUpdated as IndexUnitsUpdatedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../generated/IInstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import {
    IndexCreated,
    IndexUnitsUpdated,
    IndexUpdated,
    SubscriptionApproved,
    SubscriptionRevoked,
    SubscriptionUnitsUpdated,
} from "../../generated/schema";
import {
    createAndReturnTxn,
    createEventID,
    fetchIndex,
    fetchSubscriber,
} from "../utils";

export function handleIndexCreated(event: IndexCreatedEvent): void {
    let index = fetchIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    index.token = event.params.token;
    index.userData = event.params.userData;
    index.indexId = event.params.indexId;
    index.publisher = event.params.publisher;
    index.save();

    let ev = new IndexCreated(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = event.params.token.toHex();
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

export function handleIndexUpdated(event: IndexUpdatedEvent): void {
    let thisDistribution = event.params.newIndexValue
        .minus(event.params.oldIndexValue)
        .times(
            event.params.totalUnitsPending.plus(event.params.totalUnitsApproved)
        );

    let index = fetchIndex(
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    index.newIndexValue = event.params.newIndexValue;
    index.oldIndexValue = event.params.oldIndexValue;
    index.totalUnitsApproved = event.params.totalUnitsApproved;
    index.totalUnitsPending = event.params.totalUnitsPending;
    index.totalUnits = event.params.totalUnitsApproved.plus(
        event.params.totalUnitsPending
    );
    index.userData = event.params.userData;
    index.totalDistribution = index.totalDistribution.plus(thisDistribution);
    index.save();

    // NOTE: Deleted commented out code for calculating distribution for each subscriber due to it not being scalable.
    let ev = new IndexUpdated(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
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

export function handleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    let entity = fetchSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    entity.approved = true;
    entity.userData = event.params.userData;
    let totalPendingApproval = entity.totalPendingApproval;
    entity.totalReceivedUntilLastUpdate =
        entity.totalReceivedUntilLastUpdate.plus(
            totalPendingApproval as BigInt
        );
    entity.totalPendingApproval = new BigInt(0);
    entity.save();

    let ev = new SubscriptionApproved(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = event.params.token.toHex();
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

export function handleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    let entity = fetchSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    entity.userData = event.params.userData;
    entity.approved = false;
    entity.save();

    let ev = new SubscriptionRevoked(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = event.params.token.toHex();
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.userData = event.params.userData;
    ev.save();
}

export function handleIndexUnitsUpdated(event: IndexUnitsUpdatedEvent): void {
    let ev = new IndexUnitsUpdated(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = event.params.token.toHex();
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.subscriber = event.params.subscriber;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.save();
}

// whenever units change for a subscriber we want to update the totalReceived and totalPendingApproval
// not when the index is updated - however we msut return all the data necessary to calculate this on the
// front end (lastIndexValue, currentIndexValue)
export function handleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    let entity = fetchSubscriber(
        event.params.subscriber,
        event.params.publisher,
        event.params.token,
        event.params.indexId
    );
    entity.units = event.params.units;
    entity.save();

    let ev = new SubscriptionUnitsUpdated(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = event.params.token.toHex();
    ev.subscriber = event.params.subscriber;
    ev.publisher = event.params.publisher;
    ev.indexId = event.params.indexId;
    ev.units = event.params.units;
    ev.userData = event.params.userData;
    ev.save();
}
