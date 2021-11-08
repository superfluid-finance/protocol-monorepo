import * as Types from "../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
import { EventFieldsFragmentDoc } from "./fragments/eventFields.generated";
import { FlowUpdatedEventFieldsFragmentDoc } from "./fragments/flowUpdatedEventFields.generated";
import { IndexDistributionClaimedEventFieldsFragmentDoc } from "./fragments/indexDistributionClaimedEventFields.generated";
import { IndexSubscribedEventFieldsFragmentDoc } from "./fragments/indexSubscribedEventFields.generated";
import { IndexUnitUpdatedEventFieldsFragmentDoc } from "./fragments/indexUnitUpdatedEventFields.generated";
import { IndexUnsubscribedEventFieldsFragmentDoc } from "./fragments/indexUnsubscribedEventFields.generated";
import { TransferEventFieldsFragmentDoc } from "./fragments/transferEventFields.generated";
export type GetAllEventsQueryVariables = Types.Exact<{
    ids: Array<Types.Scalars["ID"]> | Types.Scalars["ID"];
}>;

export type GetAllEventsQuery = {
    agreementClassRegisteredEvents: Array<{
        __typename: "AgreementClassRegisteredEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    agreementClassUpdatedEvents: Array<{
        __typename: "AgreementClassUpdatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    agreementLiquidatedByEvents: Array<{
        __typename: "AgreementLiquidatedByEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    appRegisteredEvents: Array<{
        __typename: "AppRegisteredEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    burnedEvents: Array<{
        __typename: "BurnedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    cfav1LiquidationPeriodChangedEvents: Array<{
        __typename: "CFAv1LiquidationPeriodChangedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    configChangedEvents: Array<{
        __typename: "ConfigChangedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    customSuperTokenCreatedEvents: Array<{
        __typename: "CustomSuperTokenCreatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    flowUpdatedEvents: Array<{
        __typename: "FlowUpdatedEvent";
        token: string;
        sender: string;
        receiver: string;
        flowRate: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    governanceReplacedEvents: Array<{
        __typename: "GovernanceReplacedEvent";
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
    indexDistributionClaimedEvents: Array<{
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
    indexSubscribedEvents: Array<{
        __typename: "IndexSubscribedEvent";
        token: string;
        publisher: string;
        indexId: string;
        subscriber: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    indexUnitsUpdatedEvents: Array<{
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
    indexUnsubscribedEvents: Array<{
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
    jailEvents: Array<{
        __typename: "JailEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    mintedEvents: Array<{
        __typename: "MintedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    rewardAddressChangedEvents: Array<{
        __typename: "RewardAddressChangedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    roleAdminChangedEvents: Array<{
        __typename: "RoleAdminChangedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    roleGrantedEvents: Array<{
        __typename: "RoleGrantedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    roleRevokedEvents: Array<{
        __typename: "RoleRevokedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    sentEvents: Array<{
        __typename: "SentEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionApprovedEvents: Array<{
        __typename: "SubscriptionApprovedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionDistributionClaimedEvents: Array<{
        __typename: "SubscriptionDistributionClaimedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionRevokedEvents: Array<{
        __typename: "SubscriptionRevokedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    subscriptionUnitsUpdatedEvents: Array<{
        __typename: "SubscriptionUnitsUpdatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    superTokenCreatedEvents: Array<{
        __typename: "SuperTokenCreatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    superTokenFactoryUpdatedEvents: Array<{
        __typename: "SuperTokenFactoryUpdatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    superTokenLogicCreatedEvents: Array<{
        __typename: "SuperTokenLogicCreatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    superTokenLogicUpdatedEvents: Array<{
        __typename: "SuperTokenLogicUpdatedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    tokenDowngradedEvents: Array<{
        __typename: "TokenDowngradedEvent";
        amount: string;
        token: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    transferEvents: Array<{
        __typename: "TransferEvent";
        value: string;
        token: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
        from: { id: string };
        to: { id: string };
    }>;
    trustedForwarderChangedEvents: Array<{
        __typename: "TrustedForwarderChangedEvent";
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
    tokenUpgradedEvents: Array<{
        __typename: "TokenUpgradedEvent";
        amount: string;
        token: string;
        blockNumber: string;
        transactionHash: string;
        timestamp: string;
    }>;
};

export const GetAllEventsDocument = {
    kind: "Document",
    definitions: [
        {
            kind: "OperationDefinition",
            operation: "query",
            name: { kind: "Name", value: "getAllEvents" },
            variableDefinitions: [
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "ids" },
                    },
                    type: {
                        kind: "NonNullType",
                        type: {
                            kind: "ListType",
                            type: {
                                kind: "NonNullType",
                                type: {
                                    kind: "NamedType",
                                    name: { kind: "Name", value: "ID" },
                                },
                            },
                        },
                    },
                },
            ],
            selectionSet: {
                kind: "SelectionSet",
                selections: [
                    {
                        kind: "Field",
                        name: {
                            kind: "Name",
                            value: "agreementClassRegisteredEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "agreementClassUpdatedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "agreementLiquidatedByEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: { kind: "Name", value: "appRegisteredEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: { kind: "Name", value: "burnedEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "cfav1LiquidationPeriodChangedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: { kind: "Name", value: "configChangedEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "customSuperTokenCreatedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "governanceReplacedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                            ],
                        },
                    },
                    {
                        kind: "Field",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "jailEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: { kind: "Name", value: "mintedEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "rewardAddressChangedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: { kind: "Name", value: "roleAdminChangedEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: { kind: "Name", value: "roleGrantedEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: { kind: "Name", value: "roleRevokedEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: { kind: "Name", value: "sentEvents" },
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "superTokenCreatedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "superTokenFactoryUpdatedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "superTokenLogicCreatedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "superTokenLogicUpdatedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "amount" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "token" },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                        name: {
                            kind: "Name",
                            value: "trustedForwarderChangedEvents",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                                value: "id_in",
                                            },
                                            value: {
                                                kind: "Variable",
                                                name: {
                                                    kind: "Name",
                                                    value: "ids",
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
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "amount" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "token" },
                                },
                            ],
                        },
                    },
                ],
            },
        },
        ...EventFieldsFragmentDoc.definitions,
        ...FlowUpdatedEventFieldsFragmentDoc.definitions,
        ...IndexDistributionClaimedEventFieldsFragmentDoc.definitions,
        ...IndexSubscribedEventFieldsFragmentDoc.definitions,
        ...IndexUnitUpdatedEventFieldsFragmentDoc.definitions,
        ...IndexUnsubscribedEventFieldsFragmentDoc.definitions,
        ...TransferEventFieldsFragmentDoc.definitions,
    ],
} as unknown as DocumentNode<GetAllEventsQuery, GetAllEventsQueryVariables>;
