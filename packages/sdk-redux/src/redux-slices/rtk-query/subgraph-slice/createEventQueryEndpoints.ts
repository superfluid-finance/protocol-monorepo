import {QueryDefinition} from '@reduxjs/toolkit/dist/query';
import {
    AllEvents,
    EventQueryHandler,
    FlowUpdatedEvent,
    FlowUpdatedEventQueryHandler,
    ILightEntity,
    IndexUpdatedEvent,
    IndexUpdatedEventQueryHandler,
    PagedResult,
    RelevantAddressProviderFromFilter,
    RelevantAddressProviderFromResult,
    SubgraphGetQuery,
    SubgraphGetQueryHandler,
    SubgraphListQuery,
    SubgraphListQueryHandler,
    SubscriptionUnitsUpdatedEvent,
    SubscriptionUnitsUpdatedEventQueryHandler,
} from '@superfluid-finance/sdk-core';

import {CacheTagTypes} from '../cacheTags/CacheTagTypes';
import {CacheTime} from '../cacheTime';

import {
    EventQuery,
    EventsQuery,
    FlowUpdatedEventQuery,
    FlowUpdatedEventsQuery,
    IndexUpdatedEventQuery,
    IndexUpdatedEventsQuery,
    SubscriptionUnitsUpdatedEventQuery,
    SubscriptionUnitsUpdatedEventsQuery,
} from './eventQueryArgs';
import {provideCacheTagsFromRelevantAddresses} from './provideCacheTagsFromRelevantAddresses';
import {SubgraphSliceBaseQueryType, SubgraphSliceEndpointBuilder} from './subgraphSlice';

export const createEventQueryEndpoints = (builder: SubgraphSliceEndpointBuilder) => {
    // NOTE: Ignoring prettier because longer lines are more readable here.
    // prettier-ignore
    return {
        event: get<AllEvents, EventQuery>(builder, new EventQueryHandler()),
        events: list<AllEvents, EventsQuery>(builder, new EventQueryHandler()),
        flowUpdatedEvent: get<FlowUpdatedEvent, FlowUpdatedEventQuery>(builder, new FlowUpdatedEventQueryHandler()),
        flowUpdatedEvents: list<FlowUpdatedEvent, FlowUpdatedEventsQuery>(builder, new FlowUpdatedEventQueryHandler()),
        indexUpdatedEvent: get<IndexUpdatedEvent, IndexUpdatedEventQuery>(builder, new IndexUpdatedEventQueryHandler()),
        indexUpdatedEvents: list<IndexUpdatedEvent, IndexUpdatedEventsQuery>(builder, new IndexUpdatedEventQueryHandler()),
        subscriptionUnitsUpdatedEvent: get<SubscriptionUnitsUpdatedEvent, SubscriptionUnitsUpdatedEventQuery>(builder, new SubscriptionUnitsUpdatedEventQueryHandler()),
        subscriptionUnitsUpdatedEvents: list<SubscriptionUnitsUpdatedEvent, SubscriptionUnitsUpdatedEventsQuery>(builder, new SubscriptionUnitsUpdatedEventQueryHandler())
    };
};

/**
 * Creates "get" endpoint.
 */
function get<TReturn extends ILightEntity, TQuery extends {chainId: number} & SubgraphGetQuery>(
    builder: SubgraphSliceEndpointBuilder,
    queryHandler: SubgraphGetQueryHandler<TReturn> & RelevantAddressProviderFromResult<TReturn>
): QueryDefinition<TQuery, SubgraphSliceBaseQueryType, CacheTagTypes, TReturn | null> {
    return builder.query<TReturn | null, TQuery>({
        query: (arg) => {
            const {chainId, ...coreQuery} = arg;
            return {
                chainId,
                handle: (framework) => queryHandler.get(framework.query.subgraphClient, coreQuery),
            };
        },
        keepUnusedDataFor: CacheTime.Forever, // Events don't change (unless re-org but that's handled by invalidating whole cache anyway).
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
    builder: SubgraphSliceEndpointBuilder,
    queryHandler: SubgraphListQueryHandler<TReturn, TQuery, TFilter> & RelevantAddressProviderFromFilter<TFilter>
): QueryDefinition<TQuery, SubgraphSliceBaseQueryType, CacheTagTypes, PagedResult<TReturn>> {
    return builder.query<PagedResult<TReturn>, TQuery>({
        query: (arg) => {
            const {chainId, ...coreQuery} = arg;
            return {
                chainId,
                handle: (framework) => queryHandler.list(framework.query.subgraphClient, coreQuery),
            };
        },
        providesTags: (_result, _error, arg) =>
            provideCacheTagsFromRelevantAddresses(
                arg.chainId,
                queryHandler.getRelevantAddressesFromFilter(arg.filter),
                'Event'
            ),
    });
}
