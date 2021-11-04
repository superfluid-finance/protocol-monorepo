import * as Types from "../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
export type GetFlowUpdatedEventsQueryVariables = Types.Exact<{
    where: Types.FlowUpdatedEvent_Filter;
    skip: Types.Scalars["Int"];
    first: Types.Scalars["Int"];
}>;

export type GetFlowUpdatedEventsQuery = {
    result: Array<{
        id: string;
        blockNumber: string;
        timestamp: string;
        transactionHash: string;
        token: string;
        sender: string;
        receiver: string;
        flowRate: string;
        totalSenderFlowRate: string;
        totalReceiverFlowRate: string;
        userData: string;
        oldFlowRate: string;
        type: number;
        totalAmountStreamedUntilTimestamp: string;
    }>;
};

export const GetFlowUpdatedEventsDocument = {
    kind: "Document",
    definitions: [
        {
            kind: "OperationDefinition",
            operation: "query",
            name: { kind: "Name", value: "getFlowUpdatedEvents" },
            variableDefinitions: [
                {
                    kind: "VariableDefinition",
                    variable: {
                        kind: "Variable",
                        name: { kind: "Name", value: "where" },
                    },
                    type: {
                        kind: "NonNullType",
                        type: {
                            kind: "NamedType",
                            name: {
                                kind: "Name",
                                value: "FlowUpdatedEvent_filter",
                            },
                        },
                    },
                },
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
                        alias: { kind: "Name", value: "result" },
                        name: { kind: "Name", value: "flowUpdatedEvents" },
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
                                    value: "timestamp",
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
                                    name: { kind: "Name", value: "id" },
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
                                    name: { kind: "Name", value: "timestamp" },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "transactionHash",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "token" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "sender" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "receiver" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "flowRate" },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "totalSenderFlowRate",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "totalReceiverFlowRate",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "userData" },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "oldFlowRate",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "type" },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "totalAmountStreamedUntilTimestamp",
                                    },
                                },
                            ],
                        },
                    },
                ],
            },
        },
    ],
} as unknown as DocumentNode<
    GetFlowUpdatedEventsQuery,
    GetFlowUpdatedEventsQueryVariables
>;
