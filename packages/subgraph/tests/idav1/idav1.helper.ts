import { BigInt, Bytes } from "@graphprotocol/graph-ts";
import { newMockEvent } from "matchstick-as";
import {
    IndexCreated,
    IndexDistributionClaimed,
    IndexUpdated,
    IndexSubscribed,
    IndexUnitsUpdated,
    IndexUnsubscribed,
    SubscriptionApproved,
    SubscriptionDistributionClaimed,
    SubscriptionRevoked,
    SubscriptionUnitsUpdated,
} from "../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import { getAddressEventParam, getBigIntEventParam, getBytesEventParam } from "../converters";

export function createIndexCreatedEvent(
    token: string,
    publisher: string,
    indexId: BigInt,
    userData: Bytes
): IndexCreated {
    const newIndexCreatedEvent = changetype<IndexCreated>(newMockEvent());
    newIndexCreatedEvent.parameters = new Array();
    newIndexCreatedEvent.parameters.push(getAddressEventParam("token", token));
    newIndexCreatedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newIndexCreatedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newIndexCreatedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newIndexCreatedEvent;
}

export function createIndexDistributionClaimedEvent(
    token: string,
    publisher: string,
    indexId: BigInt,
    subscriber: string,
    amount: BigInt
): IndexDistributionClaimed {
    const newIndexDistributionClaimedEvent =
        changetype<IndexDistributionClaimed>(newMockEvent());
    newIndexDistributionClaimedEvent.parameters = new Array();
    newIndexDistributionClaimedEvent.parameters.push(getAddressEventParam("token", token));
    newIndexDistributionClaimedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newIndexDistributionClaimedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newIndexDistributionClaimedEvent.parameters.push(getAddressEventParam("subscriber", subscriber));
    newIndexDistributionClaimedEvent.parameters.push(getBigIntEventParam("amount", amount));

    return newIndexDistributionClaimedEvent;
}

export function createIndexUpdatedEvent(
    token: string,
    publisher: string,
    indexId: BigInt,
    oldIndexValue: BigInt,
    newIndexValue: BigInt,
    totalUnitsPending: BigInt,
    totalUnitsApproved: BigInt,
    userData: Bytes
): IndexUpdated {
    const newIndexUpdatedEvent = changetype<IndexUpdated>(newMockEvent());
    newIndexUpdatedEvent.parameters = new Array();
    newIndexUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newIndexUpdatedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newIndexUpdatedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newIndexUpdatedEvent.parameters.push(getBigIntEventParam("oldIndexValue", oldIndexValue));
    newIndexUpdatedEvent.parameters.push(getBigIntEventParam("newIndexValue", newIndexValue));
    newIndexUpdatedEvent.parameters.push(getBigIntEventParam("totalUnitsPending", totalUnitsPending));
    newIndexUpdatedEvent.parameters.push(getBigIntEventParam("totalUnitsApproved", totalUnitsApproved));
    newIndexUpdatedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newIndexUpdatedEvent;
}

export function createIndexSubscribedEvent(
    token: string,
    publisher: string,
    indexId: BigInt,
    subscriber: string,
    userData: Bytes
): IndexSubscribed {
    const newIndexSubscribedEvent = changetype<IndexSubscribed>(newMockEvent());
    newIndexSubscribedEvent.parameters = new Array();
    newIndexSubscribedEvent.parameters.push(getAddressEventParam("token", token));
    newIndexSubscribedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newIndexSubscribedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newIndexSubscribedEvent.parameters.push(getAddressEventParam("subscriber", subscriber));
    newIndexSubscribedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newIndexSubscribedEvent;
}

export function createIndexUnitsUpdatedEvent(
    token: string,
    publisher: string,
    indexId: BigInt,
    subscriber: string,
    units: BigInt,
    userData: Bytes
): IndexUnitsUpdated {
    const newIndexUnitsUpdatedEvent = changetype<IndexUnitsUpdated>(
        newMockEvent()
    );
    newIndexUnitsUpdatedEvent.parameters = new Array();
    newIndexUnitsUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newIndexUnitsUpdatedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newIndexUnitsUpdatedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newIndexUnitsUpdatedEvent.parameters.push(getAddressEventParam("subscriber", subscriber));
    newIndexUnitsUpdatedEvent.parameters.push(getBigIntEventParam("units", units));
    newIndexUnitsUpdatedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newIndexUnitsUpdatedEvent;
}

export function createIndexUnsubscribedEvent(
    token: string,
    publisher: string,
    indexId: BigInt,
    subscriber: string,
    userData: Bytes
): IndexUnsubscribed {
    const newIndexUnsubscribedEvent = changetype<IndexUnsubscribed>(
        newMockEvent()
    );
    newIndexUnsubscribedEvent.parameters = new Array();

    newIndexUnsubscribedEvent.parameters.push(getAddressEventParam("token", token));
    newIndexUnsubscribedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newIndexUnsubscribedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newIndexUnsubscribedEvent.parameters.push(getAddressEventParam("subscriber", subscriber));
    newIndexUnsubscribedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newIndexUnsubscribedEvent;
}

export function createSubscriptionApprovedEvent(
    token: string,
    subscriber: string,
    publisher: string,
    indexId: BigInt,
    userData: Bytes
): SubscriptionApproved {
    const newSubscriptionApprovedEvent = changetype<SubscriptionApproved>(
        newMockEvent()
    );
    newSubscriptionApprovedEvent.parameters = new Array();

    newSubscriptionApprovedEvent.parameters.push(getAddressEventParam("token", token));
    newSubscriptionApprovedEvent.parameters.push(getAddressEventParam("subscriber", subscriber));
    newSubscriptionApprovedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newSubscriptionApprovedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newSubscriptionApprovedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newSubscriptionApprovedEvent;
}

export function createSubscriptionDistributionClaimedEvent(
    token: string,
    subscriber: string,
    publisher: string,
    indexId: BigInt,
    amount: BigInt
): SubscriptionDistributionClaimed {
    const newSubscriptionDistributionClaimedEvent =
        changetype<SubscriptionDistributionClaimed>(newMockEvent());
    newSubscriptionDistributionClaimedEvent.parameters = new Array();

    newSubscriptionDistributionClaimedEvent.parameters.push(getAddressEventParam("token", token));
    newSubscriptionDistributionClaimedEvent.parameters.push(getAddressEventParam("subscriber", subscriber));
    newSubscriptionDistributionClaimedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newSubscriptionDistributionClaimedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newSubscriptionDistributionClaimedEvent.parameters.push(getBigIntEventParam("amount", amount));

    return newSubscriptionDistributionClaimedEvent;
}

export function createSubscriptionRevokedEvent(
    token: string,
    subscriber: string,
    publisher: string,
    indexId: BigInt,
    userData: Bytes
): SubscriptionRevoked {
    const newSubscriptionRevokedEvent = changetype<SubscriptionRevoked>(
        newMockEvent()
    );
    newSubscriptionRevokedEvent.parameters = new Array();

    newSubscriptionRevokedEvent.parameters.push(getAddressEventParam("token", token));
    newSubscriptionRevokedEvent.parameters.push(getAddressEventParam("subscriber", subscriber));
    newSubscriptionRevokedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newSubscriptionRevokedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newSubscriptionRevokedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newSubscriptionRevokedEvent;
}

export function createSubscriptionUnitsUpdatedEvent(
    token: string,
    subscriber: string,
    publisher: string,
    indexId: BigInt,
    units: BigInt,
    userData: Bytes
): SubscriptionUnitsUpdated {
    const newSubscriptionUnitsUpdatedEvent =
        changetype<SubscriptionUnitsUpdated>(newMockEvent());
    newSubscriptionUnitsUpdatedEvent.parameters = new Array();

    newSubscriptionUnitsUpdatedEvent.parameters = new Array();
    newSubscriptionUnitsUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newSubscriptionUnitsUpdatedEvent.parameters.push(getAddressEventParam("subscriber", subscriber));
    newSubscriptionUnitsUpdatedEvent.parameters.push(getAddressEventParam("publisher", publisher));
    newSubscriptionUnitsUpdatedEvent.parameters.push(getBigIntEventParam("indexId", indexId));
    newSubscriptionUnitsUpdatedEvent.parameters.push(getBigIntEventParam("units", units));
    newSubscriptionUnitsUpdatedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newSubscriptionUnitsUpdatedEvent;
}
