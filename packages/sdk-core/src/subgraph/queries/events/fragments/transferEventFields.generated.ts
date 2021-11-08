import * as Types from "../../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
import { EventFieldsFragmentDoc } from "./eventFields.generated";
export type TransferEventFieldsFragment = {
    __typename: "TransferEvent";
    value: string;
    token: string;
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
    from: { id: string };
    to: { id: string };
};

export const TransferEventFieldsFragmentDoc = {
    kind: "Document",
    definitions: [
        {
            kind: "FragmentDefinition",
            name: { kind: "Name", value: "transferEventFields" },
            typeCondition: {
                kind: "NamedType",
                name: { kind: "Name", value: "TransferEvent" },
            },
            selectionSet: {
                kind: "SelectionSet",
                selections: [
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "from" },
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "id" },
                                },
                            ],
                        },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "to" },
                        selectionSet: {
                            kind: "SelectionSet",
                            selections: [
                                {
                                    kind: "Field",
                                    name: { kind: "Name", value: "id" },
                                },
                            ],
                        },
                    },
                    { kind: "Field", name: { kind: "Name", value: "value" } },
                    { kind: "Field", name: { kind: "Name", value: "token" } },
                    {
                        kind: "FragmentSpread",
                        name: { kind: "Name", value: "eventFields" },
                    },
                ],
            },
        },
        ...EventFieldsFragmentDoc.definitions,
    ],
} as unknown as DocumentNode<TransferEventFieldsFragment, unknown>;
