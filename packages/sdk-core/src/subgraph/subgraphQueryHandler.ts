import { RequestDocument } from "graphql-request";
import _ from "lodash";

import { ILightEntity } from "../interfaces";
import { Ordering } from "../ordering";
import {
    createPagedResult,
    createSkipPaging,
    PagedResult,
    Paging,
    takePlusOne,
} from "../pagination";
import { typeGuard } from "../utils";

import { SubgraphClient } from "./SubgraphClient";
import { Address, SubgraphId } from "./mappedSubgraphTypes";
import { normalizeSubgraphFilter } from "./normalizeSubgraphFilter";
import { Exact, InputMaybe, OrderDirection, Scalars } from "./schema.generated";

/**
 * An argument object type that is used for paginated Subgraph queries.
 */
export interface SubgraphListQuery<
    TFilter extends {
        id?: InputMaybe<Scalars["ID"]>;
        id_gt?: InputMaybe<Scalars["ID"]>;
    },
    TOrderBy extends string
> {
    filter?: TFilter;
    pagination?: Paging;
    order?: Ordering<TOrderBy>;
}

/**
 * An argument object type that is used for single object queries by ID from Subgraph.
 */
export interface SubgraphGetQuery {
    id: SubgraphId;
}

/**
 * Capable of handling the {@link SubgraphGetQuery} request.
 */
export interface SubgraphGetQueryHandler<TResult extends ILightEntity> {
    get(
        subgraphClient: SubgraphClient,
        query: SubgraphGetQuery
    ): Promise<TResult | null>;
}

/**
 * Capable of handling the {@link SubgraphGetQuery} request.
 */
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

/**
 * "Relevant" means that it's connected.
 * NOTE: Currently, the relevancy is used to create a caching logic on the SDK-Redux layer.
 */
export interface RelevantAddresses {
    tokens: Address[];
    accounts: Address[];
}

/**
 * Intermediate data object to pass relevant addresses from query results with less boilerplate code.
 */
export interface RelevantAddressesIntermediate {
    tokens: (Address | Address[] | null | undefined)[];
    accounts: (Address | Address[] | null | undefined)[];
}

/**
 * Provides relevant address from a inputted filter.
 */
export interface RelevantAddressProviderFromFilter<TFilter> {
    getRelevantAddressesFromFilter(filter?: TFilter): RelevantAddresses;
}

/**
 * Provides relevant address from a query result.
 */
export interface RelevantAddressProviderFromResult<TResult> {
    getRelevantAddressesFromResult(result?: TResult | null): RelevantAddresses;
}

/**
 * A base class to handle common Subgraph query logic.
 */
export abstract class SubgraphQueryHandler<
    TResult extends ILightEntity,
    TListQuery extends SubgraphListQuery<TFilter, TOrderBy>,
    TSubgraphQuery,
    TSubgraphQueryVariables extends Exact<{
        first?: InputMaybe<Scalars["Int"]>;
        orderBy?: InputMaybe<TOrderBy>;
        orderDirection?: InputMaybe<OrderDirection>;
        skip?: InputMaybe<Scalars["Int"]>;
        where?: InputMaybe<TFilter>;
    }>,
    TFilter extends {
        id?: InputMaybe<Scalars["ID"]>;
        id_gt?: InputMaybe<Scalars["ID"]>;
    } = NonNullable<TListQuery["filter"]>,
    TOrderBy extends string = NonNullable<TListQuery["order"]>["orderBy"]
> implements
        SubgraphGetQueryHandler<TResult>,
        SubgraphListQueryHandler<TResult, TListQuery, TFilter, TOrderBy>,
        RelevantAddressProviderFromFilter<TFilter>,
        RelevantAddressProviderFromResult<TResult>
{
    abstract getAddressFieldKeysFromFilter(): {
        accountKeys: (keyof TFilter)[];
        tokenKeys: (keyof TFilter)[];
    };

    getRelevantAddressesFromFilter(filter?: TFilter): RelevantAddresses {
        if (!filter) {
            return {
                tokens: [],
                accounts: [],
            };
        }

        const addressFieldKeys = this.getAddressFieldKeysFromFilter();

        const tokenAddresses =
            this.getRelevantAddressesFromFilterByAddressFieldKeys(
                filter,
                addressFieldKeys.tokenKeys
            );

        const accountAddresses =
            this.getRelevantAddressesFromFilterByAddressFieldKeys(
                filter,
                addressFieldKeys.accountKeys
            );

        return {
            tokens: _.uniq(tokenAddresses),
            accounts: _.uniq(accountAddresses),
        };
    }

    /**
     * For every primary address field key there are more fields generated which it should look for addresses.
     * NOTE: The implementation is a bit "magical" but it rids of a bunch of boilerplate and creates a single point for editing.
     */
    private getRelevantAddressesFromFilterByAddressFieldKeys = (
        filter: TFilter,
        addressFieldKeys: (keyof TFilter)[]
    ) =>
        addressFieldKeys
            .map(
                (key) =>
                    [
                        filter[key],
                        filter[`${key}_in` as keyof TFilter],
                        filter[`${key}_not` as keyof TFilter],
                        filter[`${key}_not_in` as keyof TFilter],
                    ]
                        .filter((x) => typeof x !== "undefined")
                        .flat()
                        .filter((x) => !!x) as Address[]
            )
            .flat();

    protected abstract getRelevantAddressesFromResultCore(
        result: TResult
    ): RelevantAddressesIntermediate;

    getRelevantAddressesFromResult(result?: TResult | null): RelevantAddresses {
        if (!result) {
            return {
                tokens: [],
                accounts: [],
            };
        }

        const intermediate = this.getRelevantAddressesFromResultCore(result);
        return {
            tokens: _.uniq(
                intermediate.tokens.flat().filter((x): x is Address => !!x)
            ),
            accounts: _.uniq(
                intermediate.accounts.flat().filter((x): x is Address => !!x)
            ),
        };
    }

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
        const pagination: Paging = query.pagination ?? createSkipPaging();

        const subgraphFilter = typeGuard<TFilter>(
            normalizeSubgraphFilter({
                ...(query.filter ?? ({} as TFilter)),
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
