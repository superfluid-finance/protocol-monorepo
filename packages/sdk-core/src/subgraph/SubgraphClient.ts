import { TypedDocumentNode } from "@graphql-typed-document-node/core";
import { DocumentNode, print } from "graphql";
import { request } from "graphql-request";
import { isString } from "lodash";

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
        document: RequestDocument | TypedDocumentNode<T, V>,
        variables?: V
    ): Promise<T> {
        try {
            return await request<T, V>(
                this.subgraphUrl,
                document,
                cleanVariables(variables)
            );
        } catch (err) {
            const queryString: string = isString(document)
                ? document
                : print(document);
            throw new SFError({
                type: "SUBGRAPH_ERROR",
                message: `Failed call to subgraph with: 
${queryString}`,
                cause: err,
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
