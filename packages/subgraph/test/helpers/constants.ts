import { IEventQueryData } from "../interfaces";
import {
    getIndexCreatedEvents,
    getIndexSubscribedEvents,
    getIndexUnitsUpdatedEvents,
    getIndexUnsubscribedEvents,
    getIndexUpdatedEvents,
    getSubscriptionApprovedEvents,
    getSubscriptionRevokedEvents,
    getSubscriptionUnitsUpdatedEvents,
} from "../queries/eventQueries";

export const enum FlowActionType {
    Create,
    Update,
    Delete,
}

export const enum IDAEventType {
    IndexCreated,
    IndexUpdated,
    IndexSubscribed,
    IndexUnitsUpdated,
    IndexUnsubscribed,
    SubscriptionApproved,
    SubscriptionUnitsUpdated,
    SubscriptionRevoked,

    // There isn't an event for this, but we need to test for this case.
    Claim,
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

export const subscriptionEventTypeToIndexEventType = new Map([
    [IDAEventType.SubscriptionApproved, IDAEventType.IndexSubscribed],
    [IDAEventType.SubscriptionUnitsUpdated, IDAEventType.IndexUnitsUpdated],
    [IDAEventType.SubscriptionRevoked, IDAEventType.IndexUnsubscribed],
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
