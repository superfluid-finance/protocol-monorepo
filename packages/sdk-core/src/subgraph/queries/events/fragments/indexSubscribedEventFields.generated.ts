
import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
import { EventFieldsFragmentDoc } from "./eventFields.generated";
export type IndexSubscribedEventFieldsFragment = {
    __typename: "IndexSubscribedEvent";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export const IndexSubscribedEventFieldsFragmentDoc = {
    kind: "Document",
    definitions: [
        {
            kind: "FragmentDefinition",
            name: { kind: "Name", value: "indexSubscribedEventFields" },
            typeCondition: {
                kind: "NamedType",
                name: { kind: "Name", value: "IndexSubscribedEvent" },
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
                        kind: "FragmentSpread",
                        name: { kind: "Name", value: "eventFields" },
                    },
                ],
            },
        },
        ...EventFieldsFragmentDoc.definitions,
    ],
} as unknown as DocumentNode<IndexSubscribedEventFieldsFragment, unknown>;
