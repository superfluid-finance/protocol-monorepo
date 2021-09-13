import { createApi} from "@reduxjs/toolkit/query/react";

import {fakeBaseQuery} from "../../lib/fakeBaseQuery";

import {
    Account,
    AccountsResponse,
    fetchAccount,
    fetchAccountByAddress, fetchAccountFlows,
    fetchAccountWithFlows, fetchFlowEvents, fetchIdas,
    fetchTokenByAddress,
    fetchTokenById,
    fetchTokenFlows, FlowEvent, searchEntities, SearchResult, Subscriber, Token, TokenFlow
} from "./lib";
import {networks} from "./lib/networks";

export interface FetchAccountArg {
    accountAddress: string;
    chainId: number;
}

export interface FetchTokenByAddressArg {
    chainId: number,
    tokenAddress: string,
    devSubgraph: boolean
}

export interface FetchTokenByIdArg {
    chainId: number,
    tokenAddress: string,
    devSubgraph: boolean
}

export interface FetchTokenFlowsArg {
    tokenAddress: string,
    chainId: string,
    lastId: string,
    gt: boolean,
    limit: number
}

export interface FetchAccountTokensQueryArg {
    accountAddress: string,
    chainId: string,
    numTokens: number
}

export interface FetchAccountWithFlowsArg {
    accountAddress: string,
    chainId: string,
    tokenAddress: string,
    ownedLastUpdate?: string,
    receivedLastUpdate?: string,
    excludeInactive?: boolean,
    lt?: boolean,
    limit?: number
}

export interface FetchAccountFlowsArg {
    accountAddress: string,
    chainId: string,
    tokenAddress: string,
    ownedLastId?: string,
    receivedLastId?: string,
    excludeInactive?: boolean,
    limit?: number
}

export interface FetchFlowEventsArg {
    chainId: string,
    flowId: string,
    lastId?: string,
    gt?: boolean
}

export interface FetchIdasArg {
    chainId: string,
    accountAddress: string,
    lastId?: string,
    gt?: boolean,
    limit?: number,
    approved?: null | boolean
}

export interface SearchEntitiesArg {
    chainId: string,
    address: string
}

export interface SearchAddressArg {
    address: string
}

export const consoleDataApi = createApi({
    reducerPath: "consoleDataApi",
    baseQuery: fakeBaseQuery<string>(),
    endpoints: (builder) => ({
        fetchAccountByAddress: builder.query<Account | null, FetchAccountArg>({
            queryFn: async (arg) => {
                const fetchedAccounts = await fetchAccountByAddress(arg.accountAddress, arg.chainId.toString());
                return {data: fetchedAccounts.length ? fetchedAccounts[0] : null}
            }
        }),
        fetchTokenByAddress: builder.query<Token[], FetchTokenByAddressArg>({
            queryFn: async (arg) => {
                const fetchedTokens = await fetchTokenByAddress(arg.tokenAddress, arg.chainId.toString());
                return {data: fetchedTokens}
            }
        }),
        fetchTokenById: builder.query<Token | null, FetchTokenByIdArg>({
            queryFn: async (arg) => {
                return {data: await fetchTokenById(arg.tokenAddress, arg.chainId.toString(), arg.devSubgraph)}
            }
        }),
        fetchTokenFlows: builder.query<TokenFlow[], FetchTokenFlowsArg>({
            queryFn: async (arg) => {
                return {data: await fetchTokenFlows(arg.tokenAddress, arg.chainId, arg.lastId, arg.gt, arg.limit)}
            }
        }),
        fetchAccount: builder.query<Account[], FetchAccountTokensQueryArg>({
            queryFn: async (arg) => {
                return {data: await fetchAccount(arg.accountAddress, arg.chainId, arg.numTokens)}
            }
        }),
        fetchAccountWithFlows: builder.query<AccountsResponse, FetchAccountWithFlowsArg>({
            queryFn: async (arg) => {
                return {data: await fetchAccountWithFlows(arg.accountAddress, arg.chainId, arg.tokenAddress, arg.ownedLastUpdate, arg.receivedLastUpdate, arg.excludeInactive, arg.lt, arg.limit)}
            }
        }),
        fetchAccountFlows: builder.query<TokenFlow[], FetchAccountFlowsArg>({
            queryFn: async (arg) => {
                return {data: await fetchAccountFlows(arg.accountAddress, arg.chainId, arg.tokenAddress, arg.ownedLastId, arg.receivedLastId, arg.excludeInactive, arg.limit)}
            }
        }),
        fetchFlowEvents: builder.query<FlowEvent[], FetchFlowEventsArg>({
            queryFn: async (arg) => {
                return {data: await fetchFlowEvents(arg.chainId, arg.flowId, arg.lastId, arg.gt)}
            }
        }),
        fetchIdas: builder.query<Subscriber[], FetchIdasArg>({
            queryFn: async (arg) => {
                return {data: await fetchIdas(arg.accountAddress, arg.chainId, arg.lastId, arg.gt, arg.limit, arg.approved)}
            }
        }),
        searchEntities: builder.query<SearchResult[], SearchEntitiesArg>({
            queryFn: async (arg) => {
                return {data: await searchEntities(arg.address, arg.chainId)}
            }
        }),
        searchAddress: builder.query<SearchResult[], SearchAddressArg>({
            queryFn: async (arg) => {
                if (!arg.address)
                    return {data: []}

                const promises: Promise<SearchResult[]>[] = [];
                for (const chainId in networks) {
                    const searchPromise = searchEntities(arg.address, chainId);
                    promises.push(searchPromise);
                }

                const results = await Promise.all(promises);
                const searchResults: SearchResult[] = [];
                for (const index in results) {
                    const result = results[index];
                    if (result[0] && result[0].isTestnet) {
                        searchResults.push(...result);
                    } else {
                        searchResults.unshift(...result);
                    }
                }

                return {data: searchResults}
            }
        })
    })
});
