import {
    Account,
    AccountQueryHandler,
    AccountTokenSnapshot,
    AccountTokenSnapshotQueryHandler,
    ILightEntity,
    Index,
    IndexQueryHandler,
    IndexSubscription,
    IndexSubscriptionQueryHandler,
    PagedResult,
    RelevantAddressProviderFromFilter,
    RelevantAddressProviderFromResult,
    Stream,
    StreamPeriod,
    StreamPeriodQueryHandler,
    StreamQueryHandler,
    SubgraphGetQuery,
    SubgraphGetQueryHandler,
    SubgraphListQuery,
    SubgraphListQueryHandler,
    Token,
    TokenQueryHandler,
    TokenStatistic,
    TokenStatisticQueryHandler,
} from '@superfluid-finance/sdk-core';

import {getSubgraphClient} from '../../../../sdkReduxConfig';
import {CacheTagTypes} from '../../cacheTags/CacheTagTypes';
import {CacheTime} from '../../cacheTime';
import {provideCacheTagsFromRelevantAddresses} from '../provideCacheTagsFromRelevantAddresses';
import {SubgraphEndpointBuilder} from '../subgraphEndpointBuilder';

import {
    AccountQuery,
    AccountsQuery,
    AccountTokenSnapshotQuery,
    AccountTokenSnapshotsQuery,
    IndexesQuery,
    IndexQuery,
    IndexSubscriptionQuery,
    IndexSubscriptionsQuery,
    StreamPeriodQuery,
    StreamPeriodsQuery,
    StreamQuery,
    StreamsQuery,
    TokenQuery,
    TokensQuery,
    TokenStatisticQuery,
    TokenStatisticsQuery,
} from './entityArgs';

export const createEntityEndpoints = (builder: SubgraphEndpointBuilder) => {
    // NOTE: Ignoring prettier because longer lines are more readable here.
    // prettier-ignore
    return {
        account: get<Account, AccountQuery>(builder, new AccountQueryHandler(), "Event"),
        accounts: list<Account, AccountsQuery>(builder, new AccountQueryHandler(), "Event"),
        accountTokenSnapshot: get<AccountTokenSnapshot, AccountTokenSnapshotQuery>(builder, new AccountTokenSnapshotQueryHandler(), "Token"),
        accountTokenSnapshots: list<AccountTokenSnapshot, AccountTokenSnapshotsQuery>(builder, new AccountTokenSnapshotQueryHandler(), "Token"),
        index: get<Index, IndexQuery>(builder, new IndexQueryHandler(), "Index"),
        indexes: list<Index, IndexesQuery>(builder, new IndexQueryHandler(), "Index"),
        indexSubscription: get<IndexSubscription, IndexSubscriptionQuery>(builder, new IndexSubscriptionQueryHandler(), "Index"),
        indexSubscriptions: list<IndexSubscription, IndexSubscriptionsQuery>(builder, new IndexSubscriptionQueryHandler(), "Index"),
        stream: get<Stream, StreamQuery>(builder, new StreamQueryHandler(), "Stream"),
        streams: list<Stream, StreamsQuery>(builder, new StreamQueryHandler(), "Stream"),
        streamPeriod: get<StreamPeriod, StreamPeriodQuery>(builder, new StreamPeriodQueryHandler(), "Stream"),
        streamPeriods: list<StreamPeriod, StreamPeriodsQuery>(builder, new StreamPeriodQueryHandler(), "Stream"),
        token: get<Token, TokenQuery>(builder, new TokenQueryHandler(), "Token", CacheTime.ThreeMinutes),
        tokens: list<Token, TokensQuery>(builder, new TokenQueryHandler(), "Token"),
        tokenStatistic: get<TokenStatistic, TokenStatisticQuery>(builder, new TokenStatisticQueryHandler(), "Token"),
        tokenStatistics: list<TokenStatistic, TokenStatisticsQuery>(builder, new TokenStatisticQueryHandler(), "Token")
    };
};

/**
 * Creates "get" endpoint.
 */
function get<TReturn extends ILightEntity, TQuery extends {chainId: number} & SubgraphGetQuery>(
    builder: SubgraphEndpointBuilder,
    queryHandler: SubgraphGetQueryHandler<TReturn> & RelevantAddressProviderFromResult<TReturn>,
    tag: CacheTagTypes,
    cacheTime?: CacheTime
) {
    return builder.query<TReturn | null, TQuery>({
        queryFn: async (arg) => {
            const subgraphClient = await getSubgraphClient(arg.chainId);
            return {
                data: await queryHandler.get(subgraphClient, arg),
            };
        },
        providesTags: (result, _error, arg) =>
            provideCacheTagsFromRelevantAddresses(
                arg.chainId,
                queryHandler.getRelevantAddressesFromResult(result),
                tag
            ),
        keepUnusedDataFor: cacheTime ?? CacheTime.OneMinute,
    });
}

/**
 * Creates "list" endpoint.
 */
function list<
    TReturn extends ILightEntity,
    TQuery extends {chainId: number} & SubgraphListQuery<TFilter, TOrderBy>,
    TFilter extends {[key: string]: unknown} = NonNullable<TQuery['filter']>,
    TOrderBy extends string = NonNullable<TQuery['order']>['orderBy']
>(
    builder: SubgraphEndpointBuilder,
    queryHandler: SubgraphListQueryHandler<TReturn, TQuery, TFilter> & RelevantAddressProviderFromFilter<TFilter>,
    tag: CacheTagTypes,
    cacheTime?: CacheTime
) {
    return builder.query<PagedResult<TReturn>, TQuery>({
        queryFn: async (arg) => {
            const subgraphClient = await getSubgraphClient(arg.chainId);
            return {
                data: await queryHandler.list(subgraphClient, arg),
            };
        },
        providesTags: (_result, _error, arg) =>
            provideCacheTagsFromRelevantAddresses(
                arg.chainId,
                queryHandler.getRelevantAddressesFromFilter(arg.filter),
                tag
            ),
        keepUnusedDataFor: cacheTime ?? CacheTime.OneMinute,
    });
}
