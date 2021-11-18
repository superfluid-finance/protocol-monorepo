import * as Types from "../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
export type GetStreamsQueryVariables = Types.Exact<{
    where: Types.Stream_Filter;
    skip: Types.Scalars["Int"];
    first: Types.Scalars["Int"];
}>;

export type GetStreamsQuery = {
    result: Array<{
        id: string;
        createdAtTimestamp: string;
        createdAtBlockNumber: string;
        updatedAtTimestamp: string;
        updatedAtBlockNumber: string;
        currentFlowRate: string;
        streamedUntilUpdatedAt: string;
        token: {
            id: string;
            createdAtTimestamp: string;
            createdAtBlockNumber: string;
            name: string;
            symbol: string;
            isListed: boolean;
            underlyingAddress: string;
        };
        sender: { id: string };
        receiver: { id: string };
        flowUpdatedEvents: Array<{
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
    }>;
};

export const GetStreamsDocument = ({
    kind: "Document",
    definitions: [
        {
            kind: "OperationDefinition",
            operation: "query",
            name: { kind: "Name", value: "getStreams" },
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
                            name: { kind: "Name", value: "Stream_filter" },
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
                        name: { kind: "Name", value: "streams" },
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
                                    value: "createdAtBlockNumber",
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
                                    kind: "Field",
                                    name: { kind: "Name", value: "id" },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "createdAtTimestamp",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "createdAtBlockNumber",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "updatedAtTimestamp",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "updatedAtBlockNumber",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "currentFlowRate",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "streamedUntilUpdatedAt",
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "token" },
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
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "createdAtTimestamp",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value:
                                                        "createdAtBlockNumber",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "name",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "symbol",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "isListed",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value: "underlyingAddress",
                                                },
                                            },
                                        ],
                                    },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "sender" },
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
                                    name: { kind: "Name", value: "receiver" },
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
                                        value: "flowUpdatedEvents",
                                    },
                                    arguments: [
                                        {
                                            kind: "Argument",
                                            name: {
                                                kind: "Name",
                                                value: "orderBy",
                                            },
                                            value: {
                                                kind: "EnumValue",
                                                value: "timestamp",
                                            },
                                        },
                                        {
                                            kind: "Argument",
                                            name: {
                                                kind: "Name",
                                                value: "orderDirection",
                                            },
                                            value: {
                                                kind: "EnumValue",
                                                value: "asc",
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
                                                    value: "id",
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
                                                    value: "timestamp",
                                                },
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
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value:
                                                        "totalSenderFlowRate",
                                                },
                                            },
                                            {
                                                kind: "Field",
                                                name: {
                                                    kind: "Name",
                                                    value:
                                                        "totalReceiverFlowRate",
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
                                                    value: "oldFlowRate",
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
                                                    value:
                                                        "totalAmountStreamedUntilTimestamp",
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
    ],
} as unknown) as DocumentNode<GetStreamsQuery, GetStreamsQueryVariables>;
