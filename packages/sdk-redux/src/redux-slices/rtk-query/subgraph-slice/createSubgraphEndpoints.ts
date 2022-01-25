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
    IndexUpdatedEvent,
    IndexUpdatedEventQueryHandler,
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
    SubscriptionUnitsUpdatedEvent,
    SubscriptionUnitsUpdatedEventQueryHandler,
    Token,
    TokenQueryHandler,
    TokenStatistic,
    TokenStatisticQueryHandler,
} from '@superfluid-finance/sdk-core';

import {CacheTagTypes} from '../cacheTags/CacheTagTypes';
import {CacheTime} from '../cacheTime';

import {provideTagsFromRelevantAddresses} from './provideTagsFromRelevantAddresses';
import {
    AccountQuery,
    AccountsQuery,
    AccountTokenSnapshotQuery,
    AccountTokenSnapshotsQuery,
    IndexesQuery,
    IndexQuery,
    IndexSubscriptionQuery,
    IndexSubscriptionsQuery,
    IndexUpdatedEventQuery,
    IndexUpdatedEventsQuery,
    StreamPeriodQuery,
    StreamPeriodsQuery,
    StreamQuery,
    StreamsQuery,
    SubscriptionUnitsUpdatedEventQuery,
    SubscriptionUnitsUpdatedEventsQuery,
    TokenQuery,
    TokensQuery,
    TokenStatisticQuery,
    TokenStatisticsQuery,
} from './queryArgs';
import {SubgraphSliceEndpointBuilder} from './subgraphSlice';

export const createSubgraphEndpoints = (builder: SubgraphSliceEndpointBuilder) => {
    // NOTE: Ignoring prettier because longer lines are more readable here.
    // prettier-ignore
    return {
        account: get<Account, AccountQuery>(new AccountQueryHandler(), "Event"),
        accounts: list<Account, AccountsQuery>(new AccountQueryHandler(), "Event"),
        accountTokenSnapshot: get<AccountTokenSnapshot, AccountTokenSnapshotQuery>(new AccountTokenSnapshotQueryHandler(), "Token"),
        accountTokenSnapshots: list<AccountTokenSnapshot, AccountTokenSnapshotsQuery>(new AccountTokenSnapshotQueryHandler(), "Token"),
        index: get<Index, IndexQuery>(new IndexQueryHandler(), "Index"),
        indexes: list<Index, IndexesQuery>(new IndexQueryHandler(), "Index"),
        indexSubscription: get<IndexSubscription, IndexSubscriptionQuery>(new IndexSubscriptionQueryHandler(), "Index"),
        indexSubscriptions: list<IndexSubscription, IndexSubscriptionsQuery>(new IndexSubscriptionQueryHandler(), "Index"),
        stream: get<Stream, StreamQuery>(new StreamQueryHandler(), "Stream"),
        streams: list<Stream, StreamsQuery>(new StreamQueryHandler(), "Stream"),
        streamPeriod: get<StreamPeriod, StreamPeriodQuery>(new StreamPeriodQueryHandler(), "Stream"),
        streamPeriods: list<StreamPeriod, StreamPeriodsQuery>(new StreamPeriodQueryHandler(), "Stream"),
        token: get<Token, TokenQuery>(new TokenQueryHandler(), "Token"),
        tokens: list<Token, TokensQuery>(new TokenQueryHandler(), "Token"),
        tokenStatistic: get<TokenStatistic, TokenStatisticQuery>(new TokenStatisticQueryHandler(), "Token"),
        tokenStatistics: list<TokenStatistic, TokenStatisticsQuery>(new TokenStatisticQueryHandler(), "Token"),
        indexUpdatedEvent: get<IndexUpdatedEvent, IndexUpdatedEventQuery>(new IndexUpdatedEventQueryHandler(), "Event"),
        indexUpdatedEvents: list<IndexUpdatedEvent, IndexUpdatedEventsQuery>(new IndexUpdatedEventQueryHandler(), "Event"),
        subscriptionUnitsUpdatedEvent: get<SubscriptionUnitsUpdatedEvent, SubscriptionUnitsUpdatedEventQuery>(new SubscriptionUnitsUpdatedEventQueryHandler(), "Event"),
        subscriptionUnitsUpdatedEvents: list<SubscriptionUnitsUpdatedEvent, SubscriptionUnitsUpdatedEventsQuery>(new SubscriptionUnitsUpdatedEventQueryHandler(), "Event")
    };

    /**
     * Creates "get" endpoint.
     */
    function get<TReturn extends ILightEntity, TQuery extends {chainId: number} & SubgraphGetQuery>(
        queryHandler: SubgraphGetQueryHandler<TReturn> & RelevantAddressProviderFromResult<TReturn>,
        tag: CacheTagTypes,
        cacheTime?: CacheTime
    ) {
        return builder.query<TReturn | null, TQuery>({
            query: (arg) => {
                const {chainId, ...coreQuery} = arg;
                return {
                    chainId,
                    handle: (framework) => queryHandler.get(framework.query.subgraphClient, coreQuery),
                };
            },
            providesTags: (result, _error, arg) =>
                provideTagsFromRelevantAddresses(arg.chainId, queryHandler.getRelevantAddressesFromResult(result), tag),
            ...(cacheTime ? {keepUnusedDataFor: cacheTime} : {}),
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
        queryHandler: SubgraphListQueryHandler<TReturn, TQuery, TFilter> & RelevantAddressProviderFromFilter<TFilter>,
        tag: CacheTagTypes
    ) {
        return builder.query<PagedResult<TReturn>, TQuery>({
            query: (arg) => {
                const {chainId, ...coreQuery} = arg;
                return {
                    chainId,
                    handle: (framework) => queryHandler.list(framework.query.subgraphClient, coreQuery),
                };
            },
            providesTags: (_result, _error, arg) =>
                provideTagsFromRelevantAddresses(
                    arg.chainId,
                    queryHandler.getRelevantAddressesFromFilter(arg.filter),
                    tag
                ),
        });
    }
};
