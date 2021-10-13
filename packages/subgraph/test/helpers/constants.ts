import { IEventQueryData } from "../interfaces";
import {
    getIndexCreatedEvents,
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

export const idaEventTypeToEventQueryDataMap = new Map<
    IDAEventType,
    IEventQueryData
>([
    [
        IDAEventType.IndexCreated,
        {
            query: getIndexCreatedEvents,
            queryName: "IndexCreatedEvents",
        },
    ],
    [
        IDAEventType.IndexUpdated,
        {
            query: getIndexUpdatedEvents,
            queryName: "IndexUpdatedEvents",
        },
    ],
    [
        IDAEventType.SubscriptionApproved,
        {
            query: getSubscriptionApprovedEvents,
            queryName: "SubscriptionApprovedEvents",
        },
    ],
    [
        IDAEventType.SubscriptionRevoked,
        {
            query: getSubscriptionRevokedEvents,
            queryName: "SubscriptionRevokedEvents",
        },
    ],
    [
        IDAEventType.SubscriptionUnitsUpdated,
        {
            query: getSubscriptionUnitsUpdatedEvents,
            queryName: "SubscriptionUnitsUpdatedEvents",
        },
    ],
]);
