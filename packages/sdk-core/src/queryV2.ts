import { RequestDocument } from "graphql-request";

import { typeGuard } from "./Query";
import { ILightEntity } from "./interfaces";
import { normalizeSubgraphFilter } from "./normalizeSubgraphFilter";
import { OrderDirection, Ordering } from "./ordering";
import {
    createPagedResult,
    createSkipPaging,
    PagedResult,
    Paging,
    takePlusOne,
} from "./pagination";
import { SubgraphClient } from "./subgraph/SubgraphClient";
import { Exact, InputMaybe, Scalars } from "./subgraph/schema.generated";

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

export type SubgraphFilterOmitFieldList =
    | "id_gt"
    | "id_gte"
    | "id_lt"
    | "id_lte";

export interface SubgraphListQuery<
    TFilter extends { [key: string]: unknown },
    TOrderBy extends string
> {
    filter?: TFilter;
    pagination?: Paging;
    order?: Ordering<TOrderBy>;
}

export interface SubgraphGetQuery {
    id: SubgraphId;
}

export interface SubgraphGetQueryHandler<TResult extends ILightEntity> {
    get(
        subgraphClient: SubgraphClient,
        query: SubgraphGetQuery
    ): Promise<TResult | null>;
}

export interface SubgraphListQueryHandler<
    TResult extends ILightEntity,
    TQuery extends SubgraphListQuery<TFilter, TOrderBy>,
    TFilter extends { [key: string]: unknown } = NonNullable<TQuery["filter"]>,
    TOrderBy extends string = NonNullable<TQuery["order"]>["orderBy"]
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
    getRelevantAddressesFromResult(result?: TResult | null): RelevantAddresses;
}

export abstract class SubgraphQueryHandler<
    TResult extends ILightEntity,
    TListQuery extends SubgraphListQuery<TFilter, TOrderBy>,
    TSubgraphQuery,
    TSubgraphFilter extends {
        id?: InputMaybe<Scalars["ID"]>;
        id_gt?: InputMaybe<Scalars["ID"]>;
    } & TFilter,
    TSubgraphQueryVariables extends Exact<{
        first?: InputMaybe<Scalars["Int"]>;
        orderBy?: InputMaybe<TOrderBy>;
        orderDirection?: InputMaybe<OrderDirection>;
        skip?: InputMaybe<Scalars["Int"]>;
        where?: InputMaybe<TSubgraphFilter>;
    }>,
    TFilter extends { [key: string]: any } = NonNullable<TListQuery["filter"]>,
    TOrderBy extends string = NonNullable<TListQuery["order"]>["orderBy"]
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

    getRelevantAddressesFromResult(result?: TResult | null): RelevantAddresses {
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
    ): Promise<TResult | null> {
        if (!query.id) {
            return null;
        }

        const response = await this.querySubgraph(subgraphClient, {
            where: {
                id: query.id.toLowerCase(),
            },
            skip: 0,
            take: 1,
        } as unknown as TSubgraphQueryVariables);

        return this.mapFromSubgraphResponse(response)[0] ?? null;
    }

    async list(
        subgraphClient: SubgraphClient,
        query: SubgraphListQuery<TFilter, TOrderBy>
    ): Promise<PagedResult<TResult>> {
        // this.validateFilter(query.filter);

        const pagination: Paging = query.pagination ?? createSkipPaging();

        const subgraphFilter = typeGuard<TSubgraphFilter>(
            normalizeSubgraphFilter({
                ...this.convertToSubgraphFilter(
                    query.filter ?? ({} as TFilter)
                ),
                id_gt: pagination.lastId,
            })
        );

        const subgraphQueryVariables = typeGuard<TSubgraphQueryVariables>({
            where: normalizeSubgraphFilter(subgraphFilter),
            orderBy: query.order?.orderBy,
            orderDirection: query.order?.orderDirection,
            first: takePlusOne(pagination),
            skip: pagination.skip,
        } as unknown as TSubgraphQueryVariables);

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
