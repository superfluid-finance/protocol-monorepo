import * as Types from "../../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
import { EventFieldsFragmentDoc } from "./eventFields.generated";
export type IndexUnsubscribedEventFieldsFragment = {
    __typename: "IndexUnsubscribedEvent";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    userData: string;
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export const IndexUnsubscribedEventFieldsFragmentDoc = {
    kind: "Document",
    definitions: [
        {
            kind: "FragmentDefinition",
            name: { kind: "Name", value: "indexUnsubscribedEventFields" },
            typeCondition: {
                kind: "NamedType",
                name: { kind: "Name", value: "IndexUnsubscribedEvent" },
            },
            selectionSet: {
                kind: "SelectionSet",
                selections: [
                    { kind: "Field", name: { kind: "Name", value: "token" } },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "publisher" },
                    },
                    { kind: "Field", name: { kind: "Name", value: "indexId" } },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "subscriber" },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "userData" },
                    },
                    {
                        kind: "FragmentSpread",
                        name: { kind: "Name", value: "eventFields" },
                    },
                ],
            },
        },
        ...EventFieldsFragmentDoc.definitions,
    ],
} as unknown as DocumentNode<IndexUnsubscribedEventFieldsFragment, unknown>;
