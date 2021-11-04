import * as Types from "../../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
import { EventFieldsFragmentDoc } from "./eventFields.generated";
export type IndexUnitUpdatedEventFieldsFragment = {
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
};

export const IndexUnitUpdatedEventFieldsFragmentDoc = {
    kind: "Document",
    definitions: [
        {
            kind: "FragmentDefinition",
            name: { kind: "Name", value: "indexUnitUpdatedEventFields" },
            typeCondition: {
                kind: "NamedType",
                name: { kind: "Name", value: "IndexUnitsUpdatedEvent" },
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
                    { kind: "Field", name: { kind: "Name", value: "units" } },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "userData" },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "oldUnits" },
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
} as unknown as DocumentNode<IndexUnitUpdatedEventFieldsFragment, unknown>;
