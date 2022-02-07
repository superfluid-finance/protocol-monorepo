import { DocumentNode } from "graphql";
import { batchRequests, request } from "graphql-request";

import { SFError } from "../SFError";

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

    async request<T = unknown, V = Variables>(
        document: RequestDocument,
        variables?: V
    ): Promise<T> {
        try {
            return await request<T, V>(
                this.subgraphUrl,
                document,
                cleanVariables(variables)
            );
        } catch (err) {
            throw new SFError({
                type: "SUBGRAPH_ERROR",
                customMessage: `Failed call to subgraph with query ${document}`,
                errorObject: err,
            });
        }
    }

    async batchRequests<T = unknown, V = Variables>(
        documents: BatchRequestDocument<V>[]
    ): Promise<T> {
        try {
            return await batchRequests<T, V>(this.subgraphUrl, documents);
        } catch (err) {
            throw new SFError({
                type: "SUBGRAPH_ERROR",
                customMessage: "Failed call to subgraph with query...", // TODO(KK): Better error message
                errorObject: err,
            });
        }
    }
}

// Inspired by: https://stackoverflow.com/a/38340730
// Remove properties with null, undefined, empty string values.
function cleanVariables<V = Variables>(variables: V): V {
    return Object.fromEntries(
        Object.entries(variables)
            .filter(
                ([, value]) =>
                    value !== "" && value !== null && value !== undefined
            )
            .map(([key, value]) => [
                key,
                value === Object(value) && !Array.isArray(value)
                    ? cleanVariables(value)
                    : value,
            ])
    ) as V;
}
