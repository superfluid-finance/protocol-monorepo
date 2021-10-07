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
            queryResultName: "indexCreatedEvents",
            queryName: "IndexCreatedEvents",
        },
    ],
    [
        IDAEventType.IndexUpdated,
        {
            query: getIndexUpdatedEvents,
            queryResultName: "indexUpdatedEvents",
            queryName: "IndexUpdatedEvents",
        },
    ],
    [
        IDAEventType.SubscriptionApproved,
        {
            query: getSubscriptionApprovedEvents,
            queryResultName: "subscriptionApprovedEvents",
            queryName: "SubscriptionApprovedEvents",
        },
    ],
    [
        IDAEventType.SubscriptionRevoked,
        {
            query: getSubscriptionRevokedEvents,
            queryResultName: "subscriptionRevokedEvents",
            queryName: "SubscriptionRevokedEvents",
        },
    ],
    [
        IDAEventType.SubscriptionUnitsUpdated,
        {
            query: getSubscriptionUnitsUpdatedEvents,
            queryResultName: "subscriptionUnitsUpdatedEvents",
            queryName: "SubscriptionUnitsUpdatedEvents",
        },
    ],
]);
