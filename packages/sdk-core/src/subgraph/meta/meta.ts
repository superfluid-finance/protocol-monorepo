import { SubgraphClient } from "../SubgraphClient";
import { Block_Height } from "../schema.generated";

import { MetaDocument, MetaQuery, MetaQueryVariables } from "./meta.generated";

export interface Meta {
    blockNumber: number;
    blockHash?: string;
    deployment: string;
    hasIndexingErrors: boolean;
}

export type MetaGetQuery = {
    block?: Block_Height;
};

export class MetaQueryHandler {
    async get(
        subgraphClient: SubgraphClient,
        query: MetaGetQuery
    ): Promise<Meta | null> {
        const response = await subgraphClient.request<
            MetaQuery,
            MetaQueryVariables
        >(MetaDocument, query);

        if (!response._meta) {
            return null;
        }

        return {
            blockNumber: response._meta.block.number,
            blockHash: response._meta.block.hash,
            deployment: response._meta.deployment,
            hasIndexingErrors: response._meta.hasIndexingErrors,
        };
    }
}
