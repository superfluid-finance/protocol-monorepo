// noinspection GraphQLUnresolvedReference

import {gql, request} from 'graphql-request';

import {getNetworkByChainId} from './networks';

// from useAddressSearch.tsx
export type EntityType = 'Token' | 'Account' | 'SuperApp';
export type ChainType =
    | 'ropsten'
    | 'xdai'
    | 'rinkeby'
    | 'goerli'
    | 'kovan'
    | 'matic'
    | 'mumbai';

export interface SuperfluidEntity {
    id: string;
}

export type SearchResult = {
    type: EntityType;
    entity: SuperfluidEntity;
    isTestnet: boolean;
    network: string;
};

export type Search = {
    results: SearchResult[];
    loading: boolean;
};

export const searchEntities = async (
    address: string,
    chainId: string
): Promise<SearchResult[]> => {
    const entities: SearchResult[] = [];
    const network = getNetworkByChainId(chainId);
    const accountsPromise = fetchAccountByAddress(address, chainId);
    const tokensPromise = fetchTokenByAddress(address, chainId);

    const results = await Promise.all([accountsPromise, tokensPromise]);
    for (const account of results[0]) {
        entities.push({
            type: 'Account',
            entity: account,
            isTestnet: network.testnet,
            network: network.name,
        });
    }
    for (const token of results[1]) {
        entities.push({
            type: 'Token',
            entity: token,
            isTestnet: network.testnet,
            network: network.name,
        });
    }
    return entities;
};

// end from useAddressSearch.tsx

export type AccountWithToken = {
    id: string;
    balance: string;
    token: Token;
};

export type Account = {
    id: string;
    accountWithToken?: AccountWithToken[];
    flowsOwned?: TokenFlow[];
    flowsReceived?: TokenFlow[];
    events?: FlowEvent[];
};

export type AccountsResponse = {
    accounts: Account[];
};

export type FlowEvent = {
    id: string;
    sum: string;
    oldFlowRate: string;
    flowRate: string;
    transaction: Transaction;
};

type Transaction = {
    timestamp: number;
};

type UnitUpdate = {
    id: string;
    units: string;
    transaction: Transaction;
};

type Index = {
    totalUnits: string;
};

export type Subscriber = {
    id: string;
    publisher: string;
    token: string;
    units: string;
    totalReceived: string;
    index: Index;
    subscriptionUnitsUpdated: UnitUpdate[];
};

export type Token = {
    id: string;
    name: string;
    // TODO: These  should be optional or be handled differently
    flows: TokenFlow[];
    accountWithToken: Account[];
    symbol: string;
    underlyingAddress: string;
};

type Recipient = {
    id: string;
};

type Owner = {
    id: string;
};

export type TokenFlow = {
    id: string;
    flowRate: string;
    sum: number;
    recipient: Recipient;
    owner: Owner;
    lastUpdate: number;
};

export type ModifiedFlow = FlowEvent & {
    startDate: number;
    endDate: number;
};

const subgraphRequest = async (
    url: string,
    query: string,
    args: any,
    retries: number
): Promise<any> => {
    try {
        const resp = await request(url, query, args);
        return resp;
    } catch (err) {
        console.error(
            `Failed call to subgraph with query ${query} and error ${err}`
        );
        if (retries <= 0) {
            return;
        }
        subgraphRequest(url, query, args, retries - 1);
    }
};

export const fetchAccountByAddress = async (
    address: string,
    chainId: string
): Promise<Account[]> => {
    const fetchAccountQuery = gql`
        query getAccountById($accountAddress: String) {
            accounts(where: { id: $accountAddress }) {
                id
            }
        }
    `;
    const network = getNetworkByChainId(chainId);
    const resp = await subgraphRequest(
        network.subgraphUrl,
        fetchAccountQuery,
        {accountAddress: address.toLowerCase()},
        1
    );
    if (resp && resp.accounts && resp.accounts.length > 0) {
        return resp.accounts;
    }
    return [];
};

export const fetchTokenByAddress = async (
    address: string,
    chainId: string,
    devSubgraph = false
): Promise<Token[]> => {
    const fetchAccountQuery = gql`
        query getTokenById($tokenAddress: String) {
            tokens(where: { underlyingAddress: $tokenAddress }) {
                id
                name
                symbol
                underlyingAddress
                flows(first: 1000, where: { flowRate_not: "0" }) {
                    id
                    flowRate
                    recipient {
                        id
                    }
                    owner {
                        id
                    }
                }
                accountWithToken(first: 1000) {
                    id
                }
            }
        }
    `;
    const network = getNetworkByChainId(chainId);
    const subgraphUrl = devSubgraph
        ? network.devSubgraphUrl
        : network.subgraphUrl;
    const resp = await subgraphRequest(
        subgraphUrl,
        fetchAccountQuery,
        {tokenAddress: address.toLowerCase()},
        1
    );
    if (resp && resp.tokens && resp.tokens.length > 0) {
        return resp.tokens;
    }
    return [];
};

export const fetchTokenById = async (
    id: string,
    chainId: string,
    devSubgraph = false
): Promise<Token | null> => {
    const fetchAccountQuery = gql`
        query getTokenById($tokenId: String) {
            tokens(where: { id: $tokenId }) {
                id
                name
                symbol
                underlyingAddress
                flows(first: 1000, where: { flowRate_not: "0" }) {
                    id
                    flowRate
                    recipient {
                        id
                    }
                    owner {
                        id
                    }
                }
                accountWithToken(first: 1000) {
                    id
                }
            }
        }
    `;
    const network = getNetworkByChainId(chainId);
    const subgraphUrl = devSubgraph
        ? network.devSubgraphUrl
        : network.subgraphUrl;
    const resp = await subgraphRequest(
        subgraphUrl,
        fetchAccountQuery,
        {tokenId: id},
        1
    );
    if (resp && resp.tokens && resp.tokens.length > 0) {
        return resp.tokens[0];
    }
    return null;
};

export const fetchTokenFlows = async (
    address: string,
    chainId: string,
    lastId = '',
    gt = true,
    limit = 20
): Promise<TokenFlow[]> => {
    const paginationDir = gt ? 'gt' : 'lt';
    const orderExp = {
        gt: 'orderDirection: desc',
        lt: 'orderDirection: asc',
    };
    const fetchFlowsQuery = gql`
        query getTokenById($tokenAddress: String, $lastId: String) {
            tokens(where: { underlyingAddress: $tokenAddress }) {
                flows(first: ${limit}, orderBy: id, ${orderExp[paginationDir]}, where: { flowRate_not: "0", id_${paginationDir}: $lastId }) {
                    id
                    flowRate
                    sum
                    lastUpdate
                    recipient {
                        id
                    }
                    owner {
                        id
                    }
                }
            }
        }
    `;
    const network = getNetworkByChainId(chainId);
    const resp = await subgraphRequest(
        network.subgraphUrl,
        fetchFlowsQuery,
        {tokenAddress: address.toLowerCase(), lastId: lastId},
        1
    );
    if (resp && resp.tokens && resp.tokens[0] !== undefined && resp.tokens[0].flows.length > 0) {
        return resp.tokens[0].flows;
    }
    return [];
};

export const fetchAccount = async (
    address: string,
    chainId: string,
    numTokens = 20
): Promise<Account[]> => {
    const fetchAccountTokensQuery = gql`
        query getAccountByAddress($tokenAddress: String) {
            accounts(first: ${numTokens}, where: { id: $tokenAddress }) {
                id
                accountWithToken {
                    id
                    balance
                    token {
                        id
                        name
                        symbol
                        underlyingAddress
                    }
                }
            }
        }
    `;
    const network = getNetworkByChainId(chainId);
    const resp = await subgraphRequest(
        network.subgraphUrl,
        fetchAccountTokensQuery,
        {tokenAddress: address.toLowerCase()},
        1
    );
    if (resp && resp.accounts) {
        return resp.accounts;
    }
    return [];
};

export const fetchAccountWithFlows = async (
    address: string,
    chainId: string,
    tokenAddress: string,
    ownedLastUpdate = Math.round(new Date().getTime() / 1000).toString(),
    receivedLastUpdate = Math.round(new Date().getTime() / 1000).toString(),
    excludeInactive = false,
    lt = true,
    limit = 20
): Promise<AccountsResponse> => {
    let excludeExp = '';
    const paginationDir = lt ? 'lt' : 'gt';
    if (excludeInactive) {
        excludeExp = ', flowRate_not: "0"';
    }
    const fetchAccountTokensQuery = gql`
        query getAccountTokenFlows(
            $address: String
            $tokenAddress: String
            $ownedLastId: String
            $receivedLastId: String
        ) {
            accounts(where: { id: $address }) {
                id
                flowsOwned(first: ${limit}, orderBy: lastUpdate, orderDirection: desc, where: { token: $tokenAddress, lastUpdate_${paginationDir}: $ownedLastId${excludeExp} }) {
                    id
                    flowRate
                    sum
                    lastUpdate
                    recipient {
                        id
                    }
                    owner {
                        id
                    }
                    events(first: 1, orderBy: id, orderDirection: desc) {
                        sum
                        oldFlowRate
                        flowRate
                        transaction {
                            timestamp
                        }
                    }
                }
                flowsReceived(first: ${limit}, orderBy: lastUpdate, orderDirection: desc, where: { token: $tokenAddress, lastUpdate_${paginationDir}: $receivedLastId${excludeExp} }) {
                    id
                    flowRate
                    sum
                    lastUpdate
                    recipient {
                        id
                    }
                    owner {
                        id
                    }
                    events(first: 1, orderBy: id, orderDirection: desc) {
                        sum
                        oldFlowRate
                        flowRate
                        transaction {
                            timestamp
                        }
                    }

                }
            }
        }
    `;
    const network = getNetworkByChainId(chainId);
    const resp = await subgraphRequest(
        network.subgraphUrl,
        fetchAccountTokensQuery,
        {
            tokenAddress: tokenAddress.toLowerCase(),
            address: address.toLowerCase(),
            ownedLastId: ownedLastUpdate,
            receivedLastId: receivedLastUpdate,
        },
        1
    );
    return resp;
};

export const fetchAccountFlows = async (
    address: string,
    chainId: string,
    tokenAddress: string,
    ownedLastId = Math.round(new Date().getTime() / 1000).toString(),
    receivedLastId = Math.round(new Date().getTime() / 1000).toString(),
    excludeInactive = false,
    limit = 20
): Promise<TokenFlow[]> => {
    try {
        const resp = await fetchAccountWithFlows(
            address,
            chainId,
            tokenAddress,
            ownedLastId,
            receivedLastId,
            excludeInactive,
            true,
            limit
        );
        if (resp && resp.accounts && resp.accounts[0]) {
            const account = resp.accounts[0] as any;
            return [...account.flowsOwned, ...account.flowsReceived].sort((a, b) => {
                // At the graph level lastUpdate does not actually reflect lastUpdate
                // But actually only has the timestamp of creation
                return b.lastUpdate - a.lastUpdate;
            });
        }
    } catch (err) {
        console.error(err);
        console.error('Failed to fetch account flows');
        return [];
    }
    return [];
};

export const fetchFlowEvents = async (
    chainId: string,
    flowId: string,
    lastId = '',
    gt = true
): Promise<FlowEvent[]> => {
    const paginationDir = gt ? 'gt' : 'lt';
    const fetchAccountTokensQuery = gql`
        query getFlowEvents($flowId: String, $lastId: String) {
            flowUpdateds(first: 20, where: { id_${paginationDir}: $lastId, flow: $flowId }) {
                id
                oldFlowRate
                flowRate
                sum
                transaction {
                    timestamp
                }
            }
        }
    `;
    const network = getNetworkByChainId(chainId);
    const resp = await subgraphRequest(
        network.subgraphUrl,
        fetchAccountTokensQuery,
        {
            flowId: flowId,
            lastId: lastId,
        },
        1
    );
    if (resp && resp.flowUpdateds.length > 0) {
        return resp.flowUpdateds;
    }
    return [];
};

export const fetchIdas = async (
    accountAddress: string,
    chainId: string,
    lastId = '',
    gt = true,
    limit = 20,
    approved: null | boolean = null
): Promise<Subscriber[]> => {
    const paginationDir = gt ? 'gt' : 'lt';
    const orderExp = {
        gt: 'orderDirection: desc',
        lt: 'orderDirection: asc',
    };
    let approvedExp = '';
    if (approved !== null) {
        approvedExp = `, approved: ${approved}`;
    }

    const fetchIdaQuery = gql`
        query getIdas($accountAddress: String, $lastId: String) {
            subscribers(first: ${limit}, orderBy: id, ${orderExp[paginationDir]}, where: { subscriber: $accountAddress, id_${paginationDir}: $lastId${approvedExp}}) {
                id
                publisher
                token
                units
                totalReceived
                index {
                    totalUnits
                }
                subscriptionUnitsUpdated(orderBy: id, orderDirection: desc) {
                    id
                    units
                    transaction {
                        timestamp
                    }
                }
            }
        }
    `;
    const network = getNetworkByChainId(chainId);
    const resp = await subgraphRequest(
        network.devSubgraphUrl,
        fetchIdaQuery,
        {
            accountAddress: accountAddress,
            lastId: lastId,
        },
        1
    );
    if (resp && resp.subscribers.length > 0) {
        return resp.subscribers;
    }
    return [];
};
