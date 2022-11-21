import { TypedDocumentNode } from "@graphql-typed-document-node/core";
import { DocumentNode } from "graphql";
import { request } from "graphql-request";

type RequestDocument = string | DocumentNode;

export declare type Variables = {
    [key: string]: unknown;
};

export declare type BatchRequestDocument<V = Variables> = {
    document: RequestDocument;
    variables?: V;
};

export class SubgraphClient {
    constructor(readonly subgraphUrl: string) {}

    async request<T = unknown, V extends Variables = Variables>(
        document: RequestDocument | TypedDocumentNode<T, V>,
        variables?: V
    ): Promise<T> {
        return await request<T, V>(
            this.subgraphUrl,
            document,
            variables ? cleanVariables<V>(variables) : undefined
        );
    }
}

// Inspired by: https://stackoverflow.com/a/38340730
// Remove properties with null, undefined, empty string values.
function cleanVariables<V extends Variables = Variables>(variables: V): V {
    return Object.fromEntries(
        Object.entries(variables)
            .filter(
                ([, value]) =>
                    value !== "" && value !== null && value !== undefined
            )
            .map(([key, value]) => [
                key,
                value === Object(value) && !Array.isArray(value)
                    ? cleanVariables(value as Variables)
                    : value,
            ])
    ) as V;
}
