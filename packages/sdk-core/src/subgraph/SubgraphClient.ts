import {DocumentNode} from "graphql";
import {request} from "graphql-request";
import {handleError} from "../errorHelper";

type RequestDocument = string | DocumentNode

export declare type Variables = {
    [key: string]: any;
};

export class SubgraphClient {
    constructor(readonly subgraphUrl: string) {
    }

    async request<T = any, V = Variables>(document: RequestDocument, variables?: V): Promise<T> {
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
}
