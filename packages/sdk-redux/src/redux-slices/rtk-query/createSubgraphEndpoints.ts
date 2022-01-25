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

import {BaseGetQuery, BaseQuery2} from '../argTypes';

import {CacheTagTypes} from './cacheTags/CacheTagTypes';
import {
    GetAccount,
    GetAccountTokenSnapshot,
    GetIndex2,
    GetIndexSubscription,
    GetIndexUpdatedEvent,
    GetStream2,
    GetStreamPeriod,
    GetSubscriptionUnitsUpdatedEvent,
    GetToken,
    GetTokenStatistic,
    ListAccounts,
    ListAccountTokenSnapshots,
    ListIndexes2,
    ListIndexSubscriptions2,
    ListIndexUpdatedEvents,
    ListStreamPeriods,
    ListStreams2,
    ListSubscriptionUnitsUpdatedEvents,
    ListTokens,
    ListTokenStatistics,
} from './entities/entityQueries';
import {provideTagsFromRelevantAddresses} from './entities/provideTagsFromRelevantAddresses';
import {SubgraphSliceEndpointBuilder} from './subgraphSlice';

export const createSubgraphEndpoints = (builder: SubgraphSliceEndpointBuilder) => {
    // NOTE: Ignoring prettier because longer lines are more readable here.
    // prettier-ignore
    return {
        account: get<Account, GetAccount>(new AccountQueryHandler(), 'Event'),
        accounts: list<Account, ListAccounts>(new AccountQueryHandler(), 'Event'),
        accountTokenSnapshot: get<AccountTokenSnapshot, GetAccountTokenSnapshot>(new AccountTokenSnapshotQueryHandler(), 'Event'),
        accountTokenSnapshots: list<AccountTokenSnapshot, ListAccountTokenSnapshots>(new AccountTokenSnapshotQueryHandler(), 'Token'),
        index: get<Index, GetIndex2>(new IndexQueryHandler(), 'Index'),
        indexes: list<Index, ListIndexes2>(new IndexQueryHandler(), 'Index'),
        indexSubscription: get<IndexSubscription, GetIndexSubscription>(new IndexSubscriptionQueryHandler(), 'Index'),
        indexSubscriptions: list<IndexSubscription, ListIndexSubscriptions2>(new IndexSubscriptionQueryHandler(), 'Index'),
        stream: get<Stream, GetStream2>(new StreamQueryHandler(), 'Stream'),
        streams: list<Stream, ListStreams2>(new StreamQueryHandler(), 'Stream'),
        streamPeriod: get<StreamPeriod, GetStreamPeriod>(new StreamPeriodQueryHandler(), 'Stream'),
        streamPeriods: list<StreamPeriod, ListStreamPeriods>(new StreamPeriodQueryHandler(), 'Stream'),
        token: get<Token, GetToken>(new TokenQueryHandler(), 'Token'),
        tokens: list<Token, ListTokens>(new TokenQueryHandler(), 'Token'),
        tokenStatistic: get<TokenStatistic, GetTokenStatistic>(new TokenStatisticQueryHandler(), 'Token'),
        tokenStatistics: list<TokenStatistic, ListTokenStatistics>(new TokenStatisticQueryHandler(), 'Token'),
        indexUpdatedEvent: get<IndexUpdatedEvent, GetIndexUpdatedEvent>(new IndexUpdatedEventQueryHandler(), 'Event'),
        indexUpdatedEvents: list<IndexUpdatedEvent, ListIndexUpdatedEvents>(new IndexUpdatedEventQueryHandler(), 'Event'),
        subscriptionUnitsUpdatedEvent: get<SubscriptionUnitsUpdatedEvent, GetSubscriptionUnitsUpdatedEvent>(new SubscriptionUnitsUpdatedEventQueryHandler(), 'Event'),
        subscriptionUnitsUpdatedEvents: list<SubscriptionUnitsUpdatedEvent, ListSubscriptionUnitsUpdatedEvents>(new SubscriptionUnitsUpdatedEventQueryHandler(), 'Event'),
    };

    /**
     * Creates "get" endpoint.
     */
    function get<TReturn extends ILightEntity, TQuery extends BaseGetQuery<TReturn>>(
        queryHandler: SubgraphGetQueryHandler<TReturn> & RelevantAddressProviderFromResult<TReturn>,
        tag: CacheTagTypes
    ) {
        return builder.query<TReturn | null, TQuery>({
            query: (arg) => ({
                chainId: arg.chainId,
                handle: (framework) => {
                    const {chainId, ...query} = arg;
                    return queryHandler.get(framework.query.subgraphClient, query);
                },
            }),
            providesTags: (result, _error, arg) =>
                provideTagsFromRelevantAddresses(arg.chainId, queryHandler.getRelevantAddressesFromResult(result), tag),
        });
    }

    /**
     * Creates "list" endpoint.
     */
    function list<
        TReturn extends ILightEntity,
        TQuery extends BaseQuery2 & SubgraphListQuery<TFilter, TOrderBy>,
        TFilter extends {[key: string]: unknown} = NonNullable<TQuery['filter']>,
        TOrderBy extends string = NonNullable<TQuery['order']>['orderBy']
    >(
        queryHandler: SubgraphListQueryHandler<TReturn, TQuery, TFilter> & RelevantAddressProviderFromFilter<TFilter>,
        tag: CacheTagTypes
    ) {
        return builder.query<PagedResult<TReturn>, TQuery>({
            query: (arg) => ({
                chainId: arg.chainId,
                handle: (framework) => {
                    const {chainId, ...query} = arg;
                    return queryHandler.list(framework.query.subgraphClient, query);
                },
            }),
            providesTags: (_result, _error, arg) =>
                provideTagsFromRelevantAddresses(
                    arg.chainId,
                    queryHandler.getRelevantAddressesFromFilter(arg.filter),
                    tag
                ),
        });
    }
};
