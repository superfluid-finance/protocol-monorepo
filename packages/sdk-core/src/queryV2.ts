import { RequestDocument } from "graphql-request";

import { typeGuard } from "./Query";
import { ILightEntity } from "./interfaces";
import { OrderDirection, Ordering } from "./ordering";
import {
    createPagedResult,
    PagedResult,
    Paging,
    takePlusOne,
} from "./pagination";
import { SubgraphClient } from "./subgraph/SubgraphClient";
import { InputMaybe, Scalars } from "./subgraph/schema.generated";

export type BlockNumber = number;
export type Timestamp = number;
export type SubgraphId = string;
export type Address = string;
export type BigNumber = string;

export type BlockNumberInput = string;
export type TimestampInput = string;
export type SubgraphIdInput = string;
export type AddressInput = string;
export type BigNumberInput = string;

export type UpdatedAt = {
    updatedAtTimestamp: Timestamp;
    updatedAtBlockNumber: BlockNumber;
};

export type CreatedAt = {
    createdAtBlockNumber: BlockNumber;
    createdAtTimestamp: Timestamp;
};

export type EntityBase = UpdatedAt &
    CreatedAt & {
        id: SubgraphId;
    };

export interface SubgraphListQuery<
    _TResult extends ILightEntity,
    TFilter,
    TOrderBy extends string
> {
    filter: TFilter;
    pagination: Paging;
    order?: Ordering<TOrderBy>;
}

export interface SubgraphGetQuery<_TResult extends ILightEntity> {
    id: SubgraphId;
}

export interface EntityFilterBase {
    createdAtBlockNumber?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_gt?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_gte?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    createdAtBlockNumber_lt?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_lte?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_not?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    createdAtTimestamp?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_gt?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_gte?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    createdAtTimestamp_lt?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_lte?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_not?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    // id?: InputMaybe<Scalars['ID']>;
    // id_gt?: InputMaybe<Scalars['ID']>;
    // id_gte?: InputMaybe<Scalars['ID']>;
    // id_in?: InputMaybe<Array<Scalars['ID']>>;
    // id_lt?: InputMaybe<Scalars['ID']>;
    // id_lte?: InputMaybe<Scalars['ID']>;
    // id_not?: InputMaybe<Scalars['ID']>;
    // id_not_in?: InputMaybe<Array<Scalars['ID']>>;
    updatedAtBlockNumber?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_gt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_gte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtBlockNumber_lt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_lte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_not?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtTimestamp?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_gt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_gte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtTimestamp_lt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_lte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_not?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
}

export abstract class SubgraphQueryHandler<
    TResult extends ILightEntity,
    TFilter,
    TOrderBy extends string,
    TSubgraphQuery,
    TSubgraphFilter extends {
        id?: string | null;
        id_gt?: string | null | undefined;
    },
    TSubgraphQueryVariables extends {
        first?: InputMaybe<number>;
        orderBy?: InputMaybe<TOrderBy>;
        orderDirection?: InputMaybe<OrderDirection>;
        skip?: InputMaybe<number>;
        where?: InputMaybe<TSubgraphFilter>;
    }
> {
    constructor(protected readonly subgraphClient: SubgraphClient) {}

    abstract requestDocument: RequestDocument;

    // abstract validateFilter(filter: TFilter): void;
    abstract mapFromSubgraphResponse(response: TSubgraphQuery): TResult[];
    abstract convertToSubgraphFilter(filter: TFilter): TSubgraphFilter;

    // TODO(KK): getCacheTags

    async get(query: SubgraphGetQuery<TResult>): Promise<TResult | undefined> {
        // @ts-ignore TODO(KK): Couldn't figure out the "could be instantiated with a different subtype of constraint ..." error.
        const response = await this.querySubgraph({
            where: {
                id: query.id,
            },
            take: 1,
        });
        return this.mapFromSubgraphResponse(response)[0];
    }

    async list(
        query: SubgraphListQuery<TResult, TFilter, TOrderBy>
    ): Promise<PagedResult<TResult>> {
        // this.validateFilter(query.filter);

        const subgraphFilter = typeGuard<TSubgraphFilter>({
            ...this.convertToSubgraphFilter(query.filter),
            id_gt: query.pagination.lastId,
        });

        // @ts-ignore TODO(KK): Couldn't figure out the "could be instantiated with a different subtype of constraint ..." error.
        const subgraphQueryVariables = typeGuard<TSubgraphQueryVariables>({
            where: subgraphFilter,
            orderBy: query.order?.orderBy,
            orderDirection: query.order?.orderDirection,
            first: takePlusOne(query.pagination),
            skip: query.pagination.skip,
        });

        const subgraphResponse = await this.querySubgraph(
            subgraphQueryVariables
        );
        const mappedResult = this.mapFromSubgraphResponse(subgraphResponse);

        return createPagedResult<TResult>(mappedResult, query.pagination);
    }

    protected async querySubgraph(variables: TSubgraphQueryVariables) {
        return await this.subgraphClient.request<
            TSubgraphQuery,
            TSubgraphQueryVariables
        >(this.requestDocument, variables);
    }
}
