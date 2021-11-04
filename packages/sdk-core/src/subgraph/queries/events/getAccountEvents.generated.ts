import * as Types from "../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
import { FlowUpdatedEventFieldsFragmentDoc } from "./fragments/flowUpdatedEventFields.generated";
import { EventFieldsFragmentDoc } from "./fragments/eventFields.generated";
import { IndexDistributionClaimedEventFieldsFragmentDoc } from "./fragments/indexDistributionClaimedEventFields.generated";
import { IndexSubscribedEventFieldsFragmentDoc } from "./fragments/indexSubscribedEventFields.generated";
import { IndexUnitUpdatedEventFieldsFragmentDoc } from "./fragments/indexUnitUpdatedEventFields.generated";
import { IndexUnsubscribedEventFieldsFragmentDoc } from "./fragments/indexUnsubscribedEventFields.generated";
import { TransferEventFieldsFragmentDoc } from "./fragments/transferEventFields.generated";
export type GetAccountEventsQueryVariables = Types.Exact<{
    accountBytes: Types.Scalars["Bytes"];
    accountString: Types.Scalars["String"];
    timestamp_gte: Types.Scalars["BigInt"];
}>;

export type GetAccountEventsQuery = {
    flowUpdatedEvents_receiver: Array<{
        __typename: "FlowUpdatedEvent";
        token: string;
        sender: string;
        receiver: string;
        flowRate: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    flowUpdatedEvents_sender: Array<{
        __typename: "FlowUpdatedEvent";
        token: string;
        sender: string;
        receiver: string;
        flowRate: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexCreatedEvents: Array<{
        __typename: "IndexCreatedEvent";
        token: string;
        publisher: string;
        indexId: string;
        userData: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexDistributionClaimedEvents_subscriber: Array<{
        __typename: "IndexDistributionClaimedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        amount: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexDistributionClaimedEvents_publisher: Array<{
        __typename: "IndexDistributionClaimedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        amount: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexSubscribedEvents_subscriber: Array<{
        __typename: "IndexSubscribedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexSubscribedEvents_publisher: Array<{
        __typename: "IndexSubscribedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexUnitsUpdatedEvents_subscriber: Array<{
        __typename: "IndexUnitsUpdatedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        units: string;
        userData: string;
        oldUnits: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexUnitsUpdatedEvents_publisher: Array<{
        __typename: "IndexUnitsUpdatedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        units: string;
        userData: string;
        oldUnits: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexUnsubscribedEvents_subscriber: Array<{
        __typename: "IndexUnsubscribedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        userData: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexUnsubscribedEvents_publisher: Array<{
        __typename: "IndexUnsubscribedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        userData: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexUpdatedEvents: Array<{
        __typename: "IndexUpdatedEvent";
        token: string;
        publisher: string;
        indexId: string;
        oldIndexValue: string;
        newIndexValue: string;
        totalUnitsPending: string;
        totalUnitsApproved: string;
        userData: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    tokenDowngradedEvents: Array<{
        __typename: "TokenDowngradedEvent";
        token: string;
        amount: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    tokenUpgradedEvents: Array<{
        __typename: "TokenUpgradedEvent";
        token: string;
        amount: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    transferEvents_to: Array<{
        __typename: "TransferEvent";
        value: string;
        token: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
        from: { id: string };
        to: { id: string };
    }>;
    transferEvents_from: Array<{
        __typename: "TransferEvent";
        value: string;
        token: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
        from: { id: string };
        to: { id: string };
    }>;
    subscriptionApprovedEvents_subscriber: Array<{
        __typename: "SubscriptionApprovedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionApprovedEvents_publisher: Array<{
        __typename: "SubscriptionApprovedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionDistributionClaimedEvents_subscriber: Array<{
        __typename: "SubscriptionDistributionClaimedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionDistributionClaimedEvents_publisher: Array<{
        __typename: "SubscriptionDistributionClaimedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionRevokedEvents_subscriber: Array<{
        __typename: "SubscriptionRevokedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionRevokedEvents_publisher: Array<{
        __typename: "SubscriptionRevokedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionUnitsUpdatedEvents_subscriber: Array<{
        __typename: "SubscriptionUnitsUpdatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionUnitsUpdatedEvents_publisher: Array<{
        __typename: "SubscriptionUnitsUpdatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
};

export const GetAccountEventsDocument = {
    kind: "Document",
    definitions: [
        {
            kind: "OperationDefinition",
            operation: "query",
            name: { kind: "Name", value: "getAccountEvents" },
            variableDefinitions: [
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "accountBytes" },
                    },
                    type: {
                        kind: "NonNullType",
                        type: {
                            kind: "NamedType",
                            name: { kind: "Name", value: "Bytes" },
                        },
                    },
                },
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "accountString" },
                    },
                    type: {
                        kind: "NonNullType",
                        type: {
                            kind: "NamedType",
                            name: { kind: "Name", value: "String" },
                        },
                    },
                },
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "timestamp_gte" },
                    },
                    type: {
                        kind: "NonNullType",
                        type: {
                            kind: "NamedType",
                            name: { kind: "Name", value: "BigInt" },
                        },
                    },
                },
            ],
            selectionSet: {
                kind: "SelectionSet",
                selections: [
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "flowUpdatedEvents_receiver",
                        },
                        name: { kind: "Name", value: "flowUpdatedEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "receiver",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "flowUpdatedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "flowUpdatedEvents_sender",
                        },
                        name: { kind: "Name", value: "flowUpdatedEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "sender",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "flowUpdatedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "indexCreatedEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "token" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "publisher" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "indexId" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "userData" },
                                },
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "indexDistributionClaimedEvents_subscriber",
                        },
                        name: {
                            kind: "Name",
                            value: "indexDistributionClaimedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "subscriber",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "indexDistributionClaimedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "indexDistributionClaimedEvents_publisher",
                        },
                        name: {
                            kind: "Name",
                            value: "indexDistributionClaimedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "indexDistributionClaimedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "indexSubscribedEvents_subscriber",
                        },
                        name: { kind: "Name", value: "indexSubscribedEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "subscriber",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "indexSubscribedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "indexSubscribedEvents_publisher",
                        },
                        name: { kind: "Name", value: "indexSubscribedEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "indexSubscribedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "indexUnitsUpdatedEvents_subscriber",
                        },
                        name: {
                            kind: "Name",
                            value: "indexUnitsUpdatedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "subscriber",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "indexUnitUpdatedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "indexUnitsUpdatedEvents_publisher",
                        },
                        name: {
                            kind: "Name",
                            value: "indexUnitsUpdatedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "indexUnitUpdatedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "indexUnsubscribedEvents_subscriber",
                        },
                        name: {
                            kind: "Name",
                            value: "indexUnsubscribedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "subscriber",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "indexUnsubscribedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "indexUnsubscribedEvents_publisher",
                        },
                        name: {
                            kind: "Name",
                            value: "indexUnsubscribedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "indexUnsubscribedEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "indexUpdatedEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "token" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "publisher" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "indexId" },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "oldIndexValue",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "newIndexValue",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "totalUnitsPending",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "totalUnitsApproved",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "userData" },
                                },
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "tokenDowngradedEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "account",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountString",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "token" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "amount" },
                                },
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "tokenUpgradedEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "account",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountString",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "token" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "amount" },
                                },
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: { kind: "Name", value: "transferEvents_to" },
                        name: { kind: "Name", value: "transferEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: { kind: "Name", value: "to" },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountString",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "transferEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: { kind: "Name", value: "transferEvents_from" },
                        name: { kind: "Name", value: "transferEvents" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "from",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountString",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "transferEventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "subscriptionApprovedEvents_subscriber",
                        },
                        name: {
                            kind: "Name",
                            value: "subscriptionApprovedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "subscriber",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "subscriptionApprovedEvents_publisher",
                        },
                        name: {
                            kind: "Name",
                            value: "subscriptionApprovedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "subscriptionDistributionClaimedEvents_subscriber",
                        },
                        name: {
                            kind: "Name",
                            value: "subscriptionDistributionClaimedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "subscriber",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "subscriptionDistributionClaimedEvents_publisher",
                        },
                        name: {
                            kind: "Name",
                            value: "subscriptionDistributionClaimedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "subscriptionRevokedEvents_subscriber",
                        },
                        name: {
                            kind: "Name",
                            value: "subscriptionRevokedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "subscriber",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "subscriptionRevokedEvents_publisher",
                        },
                        name: {
                            kind: "Name",
                            value: "subscriptionRevokedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "subscriptionUnitsUpdatedEvents_subscriber",
                        },
                        name: {
                            kind: "Name",
                            value: "subscriptionUnitsUpdatedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "subscriber",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        alias: {
                            kind: "Name",
                            value: "subscriptionUnitsUpdatedEvents_publisher",
                        },
                        name: {
                            kind: "Name",
                            value: "subscriptionUnitsUpdatedEvents",
                        },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "ObjectValue",
                                    fields: [
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "publisher",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "accountBytes",
                                                },
                                            },
                                        },
                                        {
                                            kind: "ObjectField",
                                            name: {
                                                kind: "Name",
                                                value: "timestamp_gte",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "timestamp_gte",
                                                },
                                            },
                                        },
                                    ],
                                },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "FragmentSpread",
                                    name: {
                                        kind: "Name",
                                        value: "eventFields",
                                    },
                                },
                            ],
                        },
                    },
                ],
            },
        },
        ...FlowUpdatedEventFieldsFragmentDoc.definitions,
        ...EventFieldsFragmentDoc.definitions,
        ...IndexDistributionClaimedEventFieldsFragmentDoc.definitions,
        ...IndexSubscribedEventFieldsFragmentDoc.definitions,
        ...IndexUnitUpdatedEventFieldsFragmentDoc.definitions,
        ...IndexUnsubscribedEventFieldsFragmentDoc.definitions,
        ...TransferEventFieldsFragmentDoc.definitions,
    ],
} as unknown as DocumentNode<
    GetAccountEventsQuery,
    GetAccountEventsQueryVariables
>;
