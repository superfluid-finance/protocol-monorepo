import { IEventQueryData } from "../interfaces";
import {
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
        IDAEventType.IndexUpdated,
        {
            query: getIndexUpdatedEvents,
            queryResultName: "indexUpdateds",
            queryName: "IndexUpdated",
        },
    ],
    [
        IDAEventType.SubscriptionApproved,
        {
            query: getSubscriptionApprovedEvents,
            queryResultName: "subscriptionApproveds",
            queryName: "SubscriptionApproved",
        },
    ],
    [
        IDAEventType.SubscriptionUnitsUpdated,
        {
            query: getSubscriptionUnitsUpdatedEvents,
            queryResultName: "subscriptionUnitsUpdateds",
            queryName: "SubscriptionUnitsUpdated",
        },
    ],
    [
        IDAEventType.SubscriptionRevoked,
        {
            query: getSubscriptionRevokedEvents,
            queryResultName: "subscriptionRevokeds",
            queryName: "SubscriptionRevoked",
        },
    ],
]);
