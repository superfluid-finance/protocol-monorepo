import { ethers } from "ethers";
import { IEventQueryData } from "../interfaces";
import {
    getIndexCreatedEvents,
    getIndexDistributionClaimedEvents,
    getIndexSubscribedEvents,
    getIndexUnitsUpdatedEvents,
    getIndexUnsubscribedEvents,
    getIndexUpdatedEvents,
    getSubscriptionApprovedEvents,
    getSubscriptionDistributionClaimedEvents,
    getSubscriptionRevokedEvents,
    getSubscriptionUnitsUpdatedEvents,
} from "../queries/eventQueries";
import { toBN } from "./helpers";

export const enum FlowActionType {
    Create,
    Update,
    Delete,
}

export const enum IDAEventType {
    IndexCreated,
    IndexDistributionClaimed,
    IndexUpdated,
    IndexSubscribed,
    IndexUnitsUpdated,
    IndexUnsubscribed,
    SubscriptionApproved,
    SubscriptionDistributionClaimed,
    SubscriptionUnitsUpdated,
    SubscriptionRevoked,

    // There isn't an event for this, but we need to test for this case.
}

export const actionTypeToActiveStreamsDeltaMap = new Map([
    [FlowActionType.Create, 1],
    [FlowActionType.Update, 0],
    [FlowActionType.Delete, -1],
]);

export const actionTypeToClosedStreamsDeltaMap = new Map([
    [FlowActionType.Create, 0],
    [FlowActionType.Update, 0],
    [FlowActionType.Delete, 1],
]);

export const actionTypeToPeriodRevisionIndexDeltaMap = new Map([
    [FlowActionType.Create, 0],
    [FlowActionType.Update, 1],
    [FlowActionType.Delete, 1],
]);

export const subscriptionEventTypeToIndexEventType = new Map([
    [IDAEventType.SubscriptionApproved, IDAEventType.IndexSubscribed],
    [IDAEventType.SubscriptionUnitsUpdated, IDAEventType.IndexUnitsUpdated],
    [IDAEventType.SubscriptionRevoked, IDAEventType.IndexUnsubscribed],
    [
        IDAEventType.SubscriptionDistributionClaimed,
        IDAEventType.IndexDistributionClaimed,
    ],
]);

export const idaEventTypeToEventQueryDataMap = new Map<
    IDAEventType,
    IEventQueryData
>([
    [
        IDAEventType.IndexCreated,
        {
            query: getIndexCreatedEvents,
            queryName: "IndexCreatedEvent",
        },
    ],
    [
        IDAEventType.IndexDistributionClaimed,
        {
            query: getIndexDistributionClaimedEvents,
            queryName: "IndexDistributionClaimedEvent",
        },
    ],
    [
        IDAEventType.IndexUpdated,
        {
            query: getIndexUpdatedEvents,
            queryName: "IndexUpdatedEvent",
        },
    ],
    [
        IDAEventType.IndexSubscribed,
        {
            query: getIndexSubscribedEvents,
            queryName: "IndexSubscribedEvent",
        },
    ],
    [
        IDAEventType.IndexUnitsUpdated,
        {
            query: getIndexUnitsUpdatedEvents,
            queryName: "IndexUnitsUpdatedEvent",
        },
    ],
    [
        IDAEventType.IndexUnsubscribed,
        {
            query: getIndexUnsubscribedEvents,
            queryName: "IndexUnsubscribedEvent",
        },
    ],
    [
        IDAEventType.SubscriptionApproved,
        {
            query: getSubscriptionApprovedEvents,
            queryName: "SubscriptionApprovedEvent",
        },
    ],
    [
        IDAEventType.SubscriptionDistributionClaimed,
        {
            query: getSubscriptionDistributionClaimedEvents,
            queryName: "SubscriptionDistributionClaimedEvent",
        },
    ],
    [
        IDAEventType.SubscriptionRevoked,
        {
            query: getSubscriptionRevokedEvents,
            queryName: "SubscriptionRevokedEvent",
        },
    ],
    [
        IDAEventType.SubscriptionUnitsUpdated,
        {
            query: getSubscriptionUnitsUpdatedEvents,
            queryName: "SubscriptionUnitsUpdatedEvent",
        },
    ],
]);

export const ALLOW_CREATE = 1 << 0;
export const ALLOW_UPDATE = 1 << 1;
export const ALLOW_DELETE = 1 << 2;
export const FULL_CONTROL = ALLOW_CREATE | ALLOW_UPDATE | ALLOW_DELETE;

export const MAX_FLOW_RATE = toBN(2).pow(toBN(95)).sub(toBN(1));
