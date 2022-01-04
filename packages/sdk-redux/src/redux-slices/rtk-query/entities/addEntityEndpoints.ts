import {
    Account,
    AccountListQuery,
    AccountQueryHandler,
    AccountTokenSnapshot,
    AccountTokenSnapshotListQuery,
    AccountTokenSnapshotQueryHandler,
    ILightEntity,
    Index,
    IndexListQuery,
    IndexQueryHandler,
    IndexSubscription,
    IndexSubscriptionQueryHandler,
    IndexSubscriptionsListQuery,
    PagedResult,
    RelevantAddresses,
    RelevantAddressProviderFromFilter,
    RelevantAddressProviderFromResult,
    Stream,
    StreamListQuery,
    StreamPeriod,
    StreamPeriodListQuery,
    StreamPeriodQueryHandler,
    StreamQueryHandler,
    SubgraphGetQueryHandler,
    SubgraphListQuery,
    SubgraphListQueryHandler,
    Token,
    TokenListQuery,
    TokenQueryHandler,
    TokenStatistic,
    TokenStatisticListQuery,
    TokenStatisticQueryHandler,
} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseGetQuery, BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';
import {CacheTagTypes} from '../cacheTags/CacheTagTypes';

export type GetAccountTokenSnapshot = BaseGetQuery<AccountTokenSnapshot>;

export interface ListAccountTokenSnapshots extends BaseQuery2, AccountTokenSnapshotListQuery {}

export type GetAccount = BaseGetQuery<Account>;

export interface ListAccounts extends BaseQuery2, AccountListQuery {}

export type GetIndex = BaseGetQuery<Index>;

export interface ListIndexes extends BaseQuery2, IndexListQuery {}

export type GetIndexSubscription = BaseGetQuery<IndexSubscription>;

export interface ListIndexSubscriptions extends BaseQuery2, IndexSubscriptionsListQuery {}

export type GetStream = BaseGetQuery<Stream>;

export interface ListStreams extends BaseQuery2, StreamListQuery {}

export type GetStreamPeriod = BaseGetQuery<StreamPeriod>;

export interface ListStreamPeriods extends BaseQuery2, StreamPeriodListQuery {}

export type GetToken = BaseGetQuery<Token>;

export interface ListTokens extends BaseQuery2, TokenListQuery {}

export type GetTokenStatistic = BaseGetQuery<TokenStatistic>;

export interface ListTokenStatistics extends BaseQuery2, TokenStatisticListQuery {}

export const addEntityEndpoints = (builder: SfEndpointBuilder) => {
    function get<TReturn extends ILightEntity, TQuery extends BaseGetQuery<TReturn>>(
        queryHandler: SubgraphGetQueryHandler<TReturn> & RelevantAddressProviderFromResult<TReturn>,
        tag: CacheTagTypes
    ) {
        return builder.query<TReturn | null, TQuery>({
            queryFn: async (arg) => {
                const framework = await getFramework(arg.chainId);
                const {chainId, ...query} = arg;
                const result = await queryHandler.get(framework.query.subgraphClient, query);
                return {
                    data: result,
                };
            },
            providesTags: (result, _error, arg) => {
                if (!result) {
                    return [];
                }

                const relevantAddresses = queryHandler.getRelevantAddressesFromResult(result);
                return provideTagsFromRelevantAddresses(arg.chainId, relevantAddresses, tag);
            },
        });
    }

    function list<
        TReturn extends ILightEntity,
        TQuery extends BaseQuery2 & SubgraphListQuery<TFilter, TOrderBy>,
        TFilter = NonNullable<TQuery['filter']>,
        TOrderBy = NonNullable<TQuery['order']>['orderBy']
    >(
        queryHandler: SubgraphListQueryHandler<TReturn, TQuery, TFilter> & RelevantAddressProviderFromFilter<TFilter>,
        tag: CacheTagTypes
    ) {
        return builder.query<PagedResult<TReturn>, TQuery>({
            queryFn: async (arg) => {
                const framework = await getFramework(arg.chainId);
                const {chainId, ...query} = arg;
                const result = await queryHandler.list(framework.query.subgraphClient, query);
                return {
                    data: result,
                };
            },
            providesTags: (_result, _error, arg) => {
                const relevantAddresses = queryHandler.getRelevantAddressesFromFilter(arg.filter);
                return provideTagsFromRelevantAddresses(arg.chainId, relevantAddresses, tag);
            },
        });
    }

    return {
        account: get<Account, GetAccount>(new AccountQueryHandler(), 'Event'),
        accounts: list<Account, ListAccounts>(new AccountQueryHandler(), 'Event'),
        accountTokenSnapshot: get<AccountTokenSnapshot, GetAccountTokenSnapshot>(
            new AccountTokenSnapshotQueryHandler(),
            'Event'
        ),
        accountTokenSnapshots: list<AccountTokenSnapshot, ListAccountTokenSnapshots>(
            new AccountTokenSnapshotQueryHandler(),
            'Token'
        ),
        index: get<Index, GetIndex>(new IndexQueryHandler(), 'Index'),
        indexes: list<Index, ListIndexes>(new IndexQueryHandler(), 'Index'),
        indexSubscription: get<IndexSubscription, GetIndexSubscription>(new IndexSubscriptionQueryHandler(), 'Index'),
        indexSubscriptions: list<IndexSubscription, ListIndexSubscriptions>(
            new IndexSubscriptionQueryHandler(),
            'Index'
        ),
        stream: get<Stream, GetStream>(new StreamQueryHandler(), 'Stream'),
        streams: list<Stream, ListStreams>(new StreamQueryHandler(), 'Stream'),
        streamPeriod: get<StreamPeriod, GetStreamPeriod>(new StreamPeriodQueryHandler(), 'Stream'),
        streamPeriods: list<StreamPeriod, ListStreamPeriods>(new StreamPeriodQueryHandler(), 'Stream'),
        token: get<Token, GetToken>(new TokenQueryHandler(), 'Token'),
        tokens: list<Token, ListTokens>(new TokenQueryHandler(), 'Token'),
        tokenStatistic: get<TokenStatistic, GetTokenStatistic>(new TokenStatisticQueryHandler(), 'Token'),
        tokenStatistics: list<TokenStatistic, ListTokenStatistics>(new TokenStatisticQueryHandler(), 'Token'),
    };
};

function provideTagsFromRelevantAddresses(chainId: number, relevantAddresses: RelevantAddresses, tag: CacheTagTypes) {
    if (relevantAddresses.tokens) {
        return relevantAddresses.tokens
            .map((tokenAddress: string) =>
                relevantAddresses.accounts.map((accountAddress: string) => ({
                    type: tag,
                    id: `${chainId}_${tokenAddress}_${accountAddress}`,
                }))
            )
            .flat();
    } else {
        return relevantAddresses.accounts.map((accountAddress: string) => ({
            type: tag,
            id: `${chainId}_${accountAddress}`,
        }));
    }
}
