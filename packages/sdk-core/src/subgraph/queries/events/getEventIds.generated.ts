import * as Types from "../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
export type GetEventIdsQueryVariables = Types.Exact<{
    skip: Types.Scalars["Int"];
    first: Types.Scalars["Int"];
}>;

export type GetEventIdsQuery = {
    events: Array<
        | { __typename: "AgreementClassRegisteredEvent"; id: string }
        | { __typename: "AgreementClassUpdatedEvent"; id: string }
        | { __typename: "AgreementLiquidatedByEvent"; id: string }
        | { __typename: "AppRegisteredEvent"; id: string }
        | { __typename: "BurnedEvent"; id: string }
        | { __typename: "CFAv1LiquidationPeriodChangedEvent"; id: string }
        | { __typename: "ConfigChangedEvent"; id: string }
        | { __typename: "CustomSuperTokenCreatedEvent"; id: string }
        | { __typename: "FlowUpdatedEvent"; id: string }
        | { __typename: "GovernanceReplacedEvent"; id: string }
        | { __typename: "IndexCreatedEvent"; id: string }
        | { __typename: "IndexDistributionClaimedEvent"; id: string }
        | { __typename: "IndexSubscribedEvent"; id: string }
        | { __typename: "IndexUnitsUpdatedEvent"; id: string }
        | { __typename: "IndexUnsubscribedEvent"; id: string }
        | { __typename: "IndexUpdatedEvent"; id: string }
        | { __typename: "JailEvent"; id: string }
        | { __typename: "MintedEvent"; id: string }
        | { __typename: "RewardAddressChangedEvent"; id: string }
        | { __typename: "RoleAdminChangedEvent"; id: string }
        | { __typename: "RoleGrantedEvent"; id: string }
        | { __typename: "RoleRevokedEvent"; id: string }
        | { __typename: "SentEvent"; id: string }
        | { __typename: "SubscriptionApprovedEvent"; id: string }
        | { __typename: "SubscriptionDistributionClaimedEvent"; id: string }
        | { __typename: "SubscriptionRevokedEvent"; id: string }
        | { __typename: "SubscriptionUnitsUpdatedEvent"; id: string }
        | { __typename: "SuperTokenCreatedEvent"; id: string }
        | { __typename: "SuperTokenFactoryUpdatedEvent"; id: string }
        | { __typename: "SuperTokenLogicCreatedEvent"; id: string }
        | { __typename: "SuperTokenLogicUpdatedEvent"; id: string }
        | { __typename: "TokenDowngradedEvent"; id: string }
        | { __typename: "TokenUpgradedEvent"; id: string }
        | { __typename: "TransferEvent"; id: string }
        | { __typename: "TrustedForwarderChangedEvent"; id: string }
    >;
};

export const GetEventIdsDocument = {
    kind: "Document",
    definitions: [
        {
            kind: "OperationDefinition",
            operation: "query",
            name: { kind: "Name", value: "getEventIds" },
            variableDefinitions: [
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "skip" },
                    },
                    type: {
                        kind: "NonNullType",
                        type: {
                            kind: "NamedType",
                            name: { kind: "Name", value: "Int" },
                        },
                    },
                },
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "first" },
                    },
                    type: {
                        kind: "NonNullType",
                        type: {
                            kind: "NamedType",
                            name: { kind: "Name", value: "Int" },
                        },
                    },
                },
            ],
            selectionSet: {
                kind: "SelectionSet",
                selections: [
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "events" },
                        arguments: [
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "skip" },
                                value: {
                                    kind: "Variable",
                                    name: { kind: "Name", value: "skip" },
                                },
                            },
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "first" },
                                value: {
                                    kind: "Variable",
                                    name: { kind: "Name", value: "first" },
                                },
                            },
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "orderBy" },
                                value: {
                                    kind: "EnumValue",
                                    value: "blockNumber",
                                },
                            },
                            {
                                kind: "Argument",
                                name: { kind: "Name", value: "orderDirection" },
                                value: { kind: "EnumValue", value: "asc" },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "__typename" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "id" },
                                },
                            ],
                        },
                    },
                ],
            },
        },
    ],
} as unknown as DocumentNode<GetEventIdsQuery, GetEventIdsQueryVariables>;
