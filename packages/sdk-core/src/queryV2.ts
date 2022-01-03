import { RequestDocument } from "graphql-request";

import { typeGuard } from "./Query";
import { ILightEntity } from "./interfaces";
import { OrderDirection, Ordering } from "./ordering";
import {
    createPagedResult,
    createSkipPaging,
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

export interface SubgraphListQuery<TFilter, TOrderBy> {
    filter?: TFilter;
    pagination?: Paging;
    order?: Ordering<TOrderBy>;
}

export interface SubgraphGetQuery {
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

export interface SubgraphGetQueryHandler<TResult extends ILightEntity> {
    get(
        subgraphClient: SubgraphClient,
        query: SubgraphGetQuery
    ): Promise<TResult | undefined>;
}

export interface SubgraphListQueryHandler<
    TResult extends ILightEntity,
    TQuery extends SubgraphListQuery<TFilter, TOrderBy>,
    TFilter = NonNullable<TQuery["filter"]>,
    TOrderBy = NonNullable<TQuery["order"]>["orderBy"]
> {
    list(
        subgraphClient: SubgraphClient,
        query: SubgraphListQuery<TFilter, TOrderBy>
    ): Promise<PagedResult<TResult>>;
}

export interface RelevantAddressesIntermediate {
    tokens: (InputMaybe<Address> | InputMaybe<Address>[])[];
    accounts: (InputMaybe<Address> | InputMaybe<Address>[])[];
}

export interface RelevantAddresses {
    tokens: Address[];
    accounts: Address[];
}

export interface RelevantAddressProviderFromFilter<TFilter> {
    getRelevantAddressesFromFilter(filter?: TFilter): RelevantAddresses;
}

export interface RelevantAddressProviderFromResult<TResult> {
    getRelevantAddressesFromResult(result?: TResult): RelevantAddresses;
}

export abstract class SubgraphQueryHandler<
    TResult extends ILightEntity,
    TListQuery extends SubgraphListQuery<TFilter, TOrderBy>,
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
    },
    TFilter = NonNullable<TListQuery["filter"]>,
    TOrderBy = NonNullable<TListQuery["order"]>["orderBy"]
> implements
        SubgraphGetQueryHandler<TResult>,
        SubgraphListQueryHandler<TResult, TListQuery, TFilter, TOrderBy>,
        RelevantAddressProviderFromFilter<TFilter>,
        RelevantAddressProviderFromResult<TResult>
{
    abstract convertToSubgraphFilter(filter: TFilter): TSubgraphFilter;

    protected abstract getRelevantAddressesFromFilterCore(
        filter: TFilter
    ): RelevantAddressesIntermediate;

    protected abstract getRelevantAddressesFromResultCore(
        result: TResult
    ): RelevantAddressesIntermediate;

    getRelevantAddressesFromFilter(filter?: TFilter): RelevantAddresses {
        // TODO(KK): toLower, deDuplicate

        if (!filter) {
            return {
                tokens: [],
                accounts: [],
            };
        }

        const intermediate = this.getRelevantAddressesFromFilterCore(filter);
        return {
            tokens: intermediate.tokens.flat().filter((x): x is Address => !!x),
            accounts: intermediate.accounts
                .flat()
                .filter((x): x is Address => !!x),
        };
    }

    getRelevantAddressesFromResult(result?: TResult): RelevantAddresses {
        // TODO(KK): toLower, deDuplicate

        if (!result) {
            return {
                tokens: [],
                accounts: [],
            };
        }

        const intermediate = this.getRelevantAddressesFromResultCore(result);
        return {
            tokens: intermediate.tokens.flat().filter((x): x is Address => !!x),
            accounts: intermediate.accounts
                .flat()
                .filter((x): x is Address => !!x),
        };
    }

    // abstract validateFilter(filter: TFilter): void;
    abstract mapFromSubgraphResponse(response: TSubgraphQuery): TResult[];

    async get(
        subgraphClient: SubgraphClient,
        query: SubgraphGetQuery
    ): Promise<TResult | undefined> {
        // @ts-ignore TODO(KK): Couldn't figure out the "could be instantiated with a different subtype of constraint ..." error.
        const response = await this.querySubgraph(subgraphClient, {
            where: {
                id: query.id,
            },
            take: 1,
        });
        return this.mapFromSubgraphResponse(response)[0];
    }

    async list(
        subgraphClient: SubgraphClient,
        query: SubgraphListQuery<TFilter, TOrderBy>
    ): Promise<PagedResult<TResult>> {
        // this.validateFilter(query.filter);

        const pagination: Paging = query.pagination ?? createSkipPaging();

        const subgraphFilter = typeGuard<TSubgraphFilter>({
            ...this.convertToSubgraphFilter(query.filter ?? ({} as TFilter)),
            id_gt: pagination.lastId,
        });

        // @ts-ignore TODO(KK): Couldn't figure out the "could be instantiated with a different subtype of constraint ..." error.
        const subgraphQueryVariables = typeGuard<TSubgraphQueryVariables>({
            where: subgraphFilter,
            orderBy: query.order?.orderBy,
            orderDirection: query.order?.orderDirection,
            first: takePlusOne(pagination),
            skip: pagination.skip,
        });

        const subgraphResponse = await this.querySubgraph(
            subgraphClient,
            subgraphQueryVariables
        );
        const mappedResult = this.mapFromSubgraphResponse(subgraphResponse);

        return createPagedResult<TResult>(mappedResult, pagination);
    }

    protected async querySubgraph(
        subgraphClient: SubgraphClient,
        variables: TSubgraphQueryVariables
    ) {
        return await subgraphClient.request<
            TSubgraphQuery,
            TSubgraphQueryVariables
        >(this.requestDocument, variables);
    }

    abstract requestDocument: RequestDocument;
}
