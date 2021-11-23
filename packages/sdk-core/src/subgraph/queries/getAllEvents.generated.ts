import * as Types from "../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
export type GetAllEventsQueryVariables = Types.Exact<{
    where?: Types.Maybe<Types.Event_Filter>;
    skip?: Types.Maybe<Types.Scalars["Int"]>;
    first?: Types.Maybe<Types.Scalars["Int"]>;
}>;

export type GetAllEventsQuery = {
    events: Array<
        | {
              __typename: "AgreementClassRegisteredEvent";
              code: string;
              agreementType: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "AgreementClassUpdatedEvent";
              code: string;
              agreementType: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "AgreementLiquidatedByEvent";
              token: string;
              rewardAmount: string;
              penaltyAccount: string;
              liquidatorAccount: string;
              bondAccount: string;
              bailoutAmount: string;
              agreementId: string;
              agreementClass: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "AppRegisteredEvent";
              blockNumber: string;
              app: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "BurnedEvent";
              operatorData: string;
              operator: string;
              from: string;
              data: string;
              amount: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "CFAv1LiquidationPeriodChangedEvent";
              superToken: string;
              liquidationPeriod: string;
              host: string;
              isSet: boolean;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "ConfigChangedEvent";
              value: string;
              superToken: string;
              key: string;
              isSet: boolean;
              host: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "CustomSuperTokenCreatedEvent";
              token: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "FlowUpdatedEvent";
              userData: string;
              type: number;
              token: string;
              sender: string;
              receiver: string;
              flowRate: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "GovernanceReplacedEvent";
              oldGovernance: string;
              newGovernance: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "IndexCreatedEvent";
              userData: string;
              token: string;
              publisher: string;
              indexId: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              index: { id: string };
          }
        | {
              __typename: "IndexDistributionClaimedEvent";
              token: string;
              subscriber: string;
              publisher: string;
              indexId: string;
              amount: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              index: { id: string };
          }
        | {
              __typename: "IndexSubscribedEvent";
              id: string;
              userData: string;
              token: string;
              subscriber: string;
              publisher: string;
              indexId: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              index: { id: string };
          }
        | {
              __typename: "IndexUnitsUpdatedEvent";
              userData: string;
              units: string;
              token: string;
              subscriber: string;
              publisher: string;
              oldUnits: string;
              indexId: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              index: { id: string };
          }
        | {
              __typename: "IndexUnsubscribedEvent";
              userData: string;
              token: string;
              subscriber: string;
              publisher: string;
              indexId: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              index: { id: string };
          }
        | {
              __typename: "IndexUpdatedEvent";
              userData: string;
              totalUnitsPending: string;
              totalUnitsApproved: string;
              token: string;
              publisher: string;
              oldIndexValue: string;
              newIndexValue: string;
              indexId: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              index: { id: string };
          }
        | {
              __typename: "JailEvent";
              reason: string;
              app: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "MintedEvent";
              operatorData: string;
              operator: string;
              data: string;
              amount: string;
              to: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "RewardAddressChangedEvent";
              superToken: string;
              rewardAddress: string;
              isSet: boolean;
              host: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "RoleAdminChangedEvent";
              role: string;
              previousAdminRole: string;
              newAdminRole: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "RoleGrantedEvent";
              sender: string;
              role: string;
              account: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "RoleRevokedEvent";
              sender: string;
              role: string;
              account: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "SentEvent";
              to: string;
              operatorData: string;
              operator: string;
              data: string;
              amount: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "SubscriptionApprovedEvent";
              userData: string;
              token: string;
              subscriber: string;
              publisher: string;
              indexId: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              subscription: { id: string };
          }
        | {
              __typename: "SubscriptionDistributionClaimedEvent";
              token: string;
              subscriber: string;
              publisher: string;
              indexId: string;
              amount: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              subscription: { id: string };
          }
        | {
              __typename: "SubscriptionRevokedEvent";
              userData: string;
              subscriber: string;
              publisher: string;
              indexId: string;
              token: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              subscription: { id: string };
          }
        | {
              __typename: "SubscriptionUnitsUpdatedEvent";
              userData: string;
              units: string;
              token: string;
              subscriber: string;
              publisher: string;
              oldUnits: string;
              indexId: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              subscription: { id: string };
          }
        | {
              __typename: "SuperTokenCreatedEvent";
              token: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "SuperTokenFactoryUpdatedEvent";
              newFactory: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "SuperTokenLogicCreatedEvent";
              tokenLogic: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "SuperTokenLogicUpdatedEvent";
              token: string;
              code: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
        | {
              __typename: "TokenDowngradedEvent";
              token: string;
              blockNumber: string;
              amount: string;
              transactionHash: string;
              timestamp: string;
              account: { id: string };
          }
        | {
              __typename: "TokenUpgradedEvent";
              amount: string;
              token: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              account: { id: string };
          }
        | {
              __typename: "TransferEvent";
              value: string;
              token: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
              to: { id: string };
              from: { id: string };
          }
        | {
              __typename: "TrustedForwarderChangedEvent";
              isSet: boolean;
              host: string;
              forwarder: string;
              enabled: boolean;
              superToken: string;
              blockNumber: string;
              transactionHash: string;
              timestamp: string;
          }
    >;
};

export type EventFields_AgreementClassRegisteredEvent_Fragment = {
    __typename: "AgreementClassRegisteredEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_AgreementClassUpdatedEvent_Fragment = {
    __typename: "AgreementClassUpdatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_AgreementLiquidatedByEvent_Fragment = {
    __typename: "AgreementLiquidatedByEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_AppRegisteredEvent_Fragment = {
    __typename: "AppRegisteredEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_BurnedEvent_Fragment = {
    __typename: "BurnedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_CfAv1LiquidationPeriodChangedEvent_Fragment = {
    __typename: "CFAv1LiquidationPeriodChangedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_ConfigChangedEvent_Fragment = {
    __typename: "ConfigChangedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_CustomSuperTokenCreatedEvent_Fragment = {
    __typename: "CustomSuperTokenCreatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_FlowUpdatedEvent_Fragment = {
    __typename: "FlowUpdatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_GovernanceReplacedEvent_Fragment = {
    __typename: "GovernanceReplacedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_IndexCreatedEvent_Fragment = {
    __typename: "IndexCreatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_IndexDistributionClaimedEvent_Fragment = {
    __typename: "IndexDistributionClaimedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_IndexSubscribedEvent_Fragment = {
    __typename: "IndexSubscribedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_IndexUnitsUpdatedEvent_Fragment = {
    __typename: "IndexUnitsUpdatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_IndexUnsubscribedEvent_Fragment = {
    __typename: "IndexUnsubscribedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_IndexUpdatedEvent_Fragment = {
    __typename: "IndexUpdatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_JailEvent_Fragment = {
    __typename: "JailEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_MintedEvent_Fragment = {
    __typename: "MintedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_RewardAddressChangedEvent_Fragment = {
    __typename: "RewardAddressChangedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_RoleAdminChangedEvent_Fragment = {
    __typename: "RoleAdminChangedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_RoleGrantedEvent_Fragment = {
    __typename: "RoleGrantedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_RoleRevokedEvent_Fragment = {
    __typename: "RoleRevokedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SentEvent_Fragment = {
    __typename: "SentEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SubscriptionApprovedEvent_Fragment = {
    __typename: "SubscriptionApprovedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SubscriptionDistributionClaimedEvent_Fragment = {
    __typename: "SubscriptionDistributionClaimedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SubscriptionRevokedEvent_Fragment = {
    __typename: "SubscriptionRevokedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SubscriptionUnitsUpdatedEvent_Fragment = {
    __typename: "SubscriptionUnitsUpdatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SuperTokenCreatedEvent_Fragment = {
    __typename: "SuperTokenCreatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SuperTokenFactoryUpdatedEvent_Fragment = {
    __typename: "SuperTokenFactoryUpdatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SuperTokenLogicCreatedEvent_Fragment = {
    __typename: "SuperTokenLogicCreatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_SuperTokenLogicUpdatedEvent_Fragment = {
    __typename: "SuperTokenLogicUpdatedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_TokenDowngradedEvent_Fragment = {
    __typename: "TokenDowngradedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_TokenUpgradedEvent_Fragment = {
    __typename: "TokenUpgradedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_TransferEvent_Fragment = {
    __typename: "TransferEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFields_TrustedForwarderChangedEvent_Fragment = {
    __typename: "TrustedForwarderChangedEvent";
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export type EventFieldsFragment =
    | EventFields_AgreementClassRegisteredEvent_Fragment
    | EventFields_AgreementClassUpdatedEvent_Fragment
    | EventFields_AgreementLiquidatedByEvent_Fragment
    | EventFields_AppRegisteredEvent_Fragment
    | EventFields_BurnedEvent_Fragment
    | EventFields_CfAv1LiquidationPeriodChangedEvent_Fragment
    | EventFields_ConfigChangedEvent_Fragment
    | EventFields_CustomSuperTokenCreatedEvent_Fragment
    | EventFields_FlowUpdatedEvent_Fragment
    | EventFields_GovernanceReplacedEvent_Fragment
    | EventFields_IndexCreatedEvent_Fragment
    | EventFields_IndexDistributionClaimedEvent_Fragment
    | EventFields_IndexSubscribedEvent_Fragment
    | EventFields_IndexUnitsUpdatedEvent_Fragment
    | EventFields_IndexUnsubscribedEvent_Fragment
    | EventFields_IndexUpdatedEvent_Fragment
    | EventFields_JailEvent_Fragment
    | EventFields_MintedEvent_Fragment
    | EventFields_RewardAddressChangedEvent_Fragment
    | EventFields_RoleAdminChangedEvent_Fragment
    | EventFields_RoleGrantedEvent_Fragment
    | EventFields_RoleRevokedEvent_Fragment
    | EventFields_SentEvent_Fragment
    | EventFields_SubscriptionApprovedEvent_Fragment
    | EventFields_SubscriptionDistributionClaimedEvent_Fragment
    | EventFields_SubscriptionRevokedEvent_Fragment
    | EventFields_SubscriptionUnitsUpdatedEvent_Fragment
    | EventFields_SuperTokenCreatedEvent_Fragment
    | EventFields_SuperTokenFactoryUpdatedEvent_Fragment
    | EventFields_SuperTokenLogicCreatedEvent_Fragment
    | EventFields_SuperTokenLogicUpdatedEvent_Fragment
    | EventFields_TokenDowngradedEvent_Fragment
    | EventFields_TokenUpgradedEvent_Fragment
    | EventFields_TransferEvent_Fragment
    | EventFields_TrustedForwarderChangedEvent_Fragment;

export const EventFieldsFragmentDoc = ({
    kind: "Document",
    definitions: [
        {
            kind: "FragmentDefinition",
            name: { kind: "Name", value: "eventFields" },
            typeCondition: {
                kind: "NamedType",
                name: { kind: "Name", value: "Event" },
            },
            selectionSet: {
                kind: "SelectionSet",
                selections: [
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "__typename" },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "blockNumber" },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "transactionHash" },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "timestamp" },
                    },
                ],
            },
        },
    ],
} as unknown) as DocumentNode<EventFieldsFragment, unknown>;
export const GetAllEventsDocument = ({
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
                        name: { kind: "Name", value: "where" },
                    },
                    type: {
                        kind: "NamedType",
                        name: { kind: "Name", value: "Event_filter" },
                    },
                    defaultValue: { kind: "ObjectValue", fields: [] },
                },
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "skip" },
                    },
                    type: {
                        kind: "NamedType",
                        name: { kind: "Name", value: "Int" },
                    },
                    defaultValue: { kind: "IntValue", value: "10" },
                },
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "first" },
                    },
                    type: {
                        kind: "NamedType",
                        name: { kind: "Name", value: "Int" },
                    },
                    defaultValue: { kind: "IntValue", value: "10" },
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
                                name: { kind: "Name", value: "where" },
                                value: {
                                    kind: "Variable",
                                    name: { kind: "Name", value: "where" },
                                },
                            },
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
                                value: { kind: "EnumValue", value: "desc" },
                            },
                        ],
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "FlowUpdatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "type",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "sender",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "receiver",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "flowRate",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "IndexCreatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "index",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "IndexDistributionClaimedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscriber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "index",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "amount",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "IndexUpdatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
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
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
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
                                                    value: "indexId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "index",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "IndexSubscribedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "id",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscriber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "index",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "IndexUnitsUpdatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "units",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscriber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "oldUnits",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "index",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "IndexUnsubscribedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscriber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "index",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "SubscriptionApprovedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscription",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscriber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "SubscriptionDistributionClaimedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscription",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscriber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "amount",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "SubscriptionRevokedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscription",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscriber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "SubscriptionUnitsUpdatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "userData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "units",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscription",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "subscriber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "publisher",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "oldUnits",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "indexId",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "TransferEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "value",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "to",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "from",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "TokenUpgradedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "amount",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "account",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "TokenDowngradedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "blockNumber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "amount",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "account",
                                                },
                                                selectionSet: {
                                                    kind: "SelectionSet",
                                                    selections: [
                                                        {
                                                            kind: "Field",
                                                            name: {
                                                                kind: "Name",
                                                                value: "id",
                                                            },
                                                        },
                                                    ],
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "AgreementClassRegisteredEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "code",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "agreementType",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "AgreementClassUpdatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "code",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "agreementType",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "AppRegisteredEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "blockNumber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "app",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "GovernanceReplacedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "oldGovernance",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "newGovernance",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "JailEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "reason",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "app",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "SuperTokenFactoryUpdatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "newFactory",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "SuperTokenLogicUpdatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "code",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "RoleAdminChangedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "role",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "previousAdminRole",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "newAdminRole",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "RoleGrantedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "sender",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "role",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "account",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "RoleRevokedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "sender",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "role",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "account",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "CFAv1LiquidationPeriodChangedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "superToken",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "liquidationPeriod",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "isSet",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "ConfigChangedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "value",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "superToken",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "key",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "isSet",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "host",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "RewardAddressChangedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "superToken",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "rewardAddress",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "isSet",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "host",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "TrustedForwarderChangedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "isSet",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "host",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "forwarder",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "enabled",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "superToken",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "AgreementLiquidatedByEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "rewardAmount",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "penaltyAccount",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "liquidatorAccount",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "bondAccount",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "bailoutAmount",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "agreementId",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "agreementClass",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "BurnedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "operatorData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "operator",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "from",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "data",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "amount",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "MintedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "operatorData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "operator",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "data",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "amount",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "to",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "SentEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "to",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "operatorData",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "operator",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "data",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "amount",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "CustomSuperTokenCreatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value: "SuperTokenCreatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "token",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "InlineFragment",
                                    typeCondition: {
                                        kind: "NamedType",
                                        name: {
                                            kind: "Name",
                                            value:
                                                "SuperTokenLogicCreatedEvent",
                                        },
                                    },
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
                                                name: {
                                                    kind: "Name",
                                                    value: "tokenLogic",
                                                },
                                            },
                                        ],
                                    },
                                },
                            ],
                        },
                    },
                ],
            },
        },
        ...EventFieldsFragmentDoc.definitions,
    ],
} as unknown) as DocumentNode<GetAllEventsQuery, GetAllEventsQueryVariables>;
