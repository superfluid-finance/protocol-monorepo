import * as Types from "../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
export type GetIndexesQueryVariables = Types.Exact<{
    where: Types.Index_Filter;
    skip: Types.Scalars["Int"];
    first: Types.Scalars["Int"];
}>;

export type GetIndexesQuery = {
    result: Array<{
        id: string;
        createdAtTimestamp: string;
        createdAtBlockNumber: string;
        updatedAtTimestamp: string;
        updatedAtBlockNumber: string;
        indexId: string;
        indexValue: string;
        totalSubscriptionsWithUnits: number;
        totalUnitsPending: string;
        totalUnitsApproved: string;
        totalUnits: string;
        totalAmountDistributedUntilUpdatedAt: string;
        token: {
            id: string;
            createdAtTimestamp: string;
            createdAtBlockNumber: string;
            name: string;
            symbol: string;
            isListed: boolean;
            underlyingAddress: string;
        };
        publisher: { id: string };
    }>;
};

export const GetIndexesDocument = ({
    kind: "Document",
    definitions: [
        {
            kind: "OperationDefinition",
            operation: "query",
            name: { kind: "Name", value: "getIndexes" },
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
                            name: { kind: "Name", value: "Index_filter" },
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
                        name: { kind: "Name", value: "indexes" },
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
                                    name: { kind: "Name", value: "indexId" },
                                },
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "indexValue" },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value: "totalSubscriptionsWithUnits",
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
                                    name: { kind: "Name", value: "totalUnits" },
                                },
                                {
                                    kind: "Field",
                                    name: {
                                        kind: "Name",
                                        value:
                                            "totalAmountDistributedUntilUpdatedAt",
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
                                    name: { kind: "Name", value: "publisher" },
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
                ],
            },
        },
    ],
} as unknown) as DocumentNode<GetIndexesQuery, GetIndexesQueryVariables>;
