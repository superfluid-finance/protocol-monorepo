import * as Types from "../../../schema.generated";

import { TypedDocumentNode as DocumentNode } from "@graphql-typed-document-node/core";
import { EventFieldsFragmentDoc } from "./eventFields.generated";
export type FlowUpdatedEventFieldsFragment = {
    __typename: "FlowUpdatedEvent";
    token: string;
    sender: string;
    receiver: string;
    flowRate: string;
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export const FlowUpdatedEventFieldsFragmentDoc = {
    kind: "Document",
    definitions: [
        {
            kind: "FragmentDefinition",
            name: { kind: "Name", value: "flowUpdatedEventFields" },
            typeCondition: {
                kind: "NamedType",
                name: { kind: "Name", value: "FlowUpdatedEvent" },
            },
            selectionSet: {
                kind: "SelectionSet",
                selections: [
                    { kind: "Field", name: { kind: "Name", value: "token" } },
                    { kind: "Field", name: { kind: "Name", value: "sender" } },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "receiver" },
                    },
                    {
                        kind: "Field",
                        name: { kind: "Name", value: "flowRate" },
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
} as unknown as DocumentNode<FlowUpdatedEventFieldsFragment, unknown>;
