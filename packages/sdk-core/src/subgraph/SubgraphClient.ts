import { DocumentNode } from "graphql";
import { request, batchRequests } from "graphql-request";
import { handleError } from "../errorHelper";

type RequestDocument = string | DocumentNode;

export declare type Variables = {
    [key: string]: any;
};

export declare type BatchRequestDocument<V = Variables> = {
    document: RequestDocument;
    variables?: V;
};

export class SubgraphClient {
    constructor(readonly subgraphUrl: string) {}

    // export declare function request<T = any, V = Variables>(url: string, document: RequestDocument, variables?: V, requestHeaders?: Dom.RequestInit['headers']): Promise<T>;

    async request<T = any, V = Variables>(
        document: RequestDocument,
        variables?: V
    ): Promise<T> {
        try {
            return await request<T, V>(this.subgraphUrl, document, variables);
        } catch (err) {
            return handleError(
                "SUBGRAPH_ERROR",
                `Failed call to subgraph with query ${document}`,
                JSON.stringify(err)
            );
        }
    }

    // export declare function batchRequests<T extends any = any, V = Variables>(url: string, documents: BatchRequestDocument<V>[], requestHeaders?: Dom.RequestInit['headers']): Promise<T>;

    async batchRequests<T = any, V = Variables>(
        documents: BatchRequestDocument<V>[]
    ): Promise<T> {
        try {
            return await batchRequests<T, V>(this.subgraphUrl, documents);
        } catch (err) {
            return handleError(
                "SUBGRAPH_ERROR",
                `Failed call to subgraph with query...`, // TODO(KK): Better error message
                JSON.stringify(err)
            );
        }
    }
}
