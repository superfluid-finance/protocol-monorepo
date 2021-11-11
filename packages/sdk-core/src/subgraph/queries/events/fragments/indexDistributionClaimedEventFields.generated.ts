import * as Types from "../../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
import { EventFieldsFragmentDoc } from "./eventFields.generated";
export type IndexDistributionClaimedEventFieldsFragment = {
    __typename: "IndexDistributionClaimedEvent";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    amount: string;
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export const IndexDistributionClaimedEventFieldsFragmentDoc = {
    kind: "Document",
    definitions: [
        {
            kind: "FragmentDefinition",
            name: {
                kind: "Name",
                value: "indexDistributionClaimedEventFields",
            },
            typeCondition: {
                kind: "NamedType",
                name: { kind: "Name", value: "IndexDistributionClaimedEvent" },
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
                    { kind: "Field", name: { kind: "Name", value: "amount" } },
                    {
                        kind: "FragmentSpread",
                        name: { kind: "Name", value: "eventFields" },
                    },
                ],
            },
        },
        ...EventFieldsFragmentDoc.definitions,
    ],
} as unknown as DocumentNode<
    IndexDistributionClaimedEventFieldsFragment,
    unknown
>;
