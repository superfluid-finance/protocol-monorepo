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
import {provideTagsFromRelevantAddresses} from './provideTagsFromRelevantAddresses';
import {SubgraphSliceEndpointBuilder} from './subgraphSlice';

export const createEventEndpoints = (builder: SubgraphSliceEndpointBuilder) => {
    // NOTE: Ignoring prettier because longer lines are more readable here.
    // prettier-ignore
    return {
        event: get<AllEvents, EventQuery>(new EventQueryHandler()),
        events: list<AllEvents, EventsQuery>(new EventQueryHandler()),
        flowUpdatedEvent: get<FlowUpdatedEvent, FlowUpdatedEventQuery>(new FlowUpdatedEventQueryHandler()),
        flowUpdatedEvents: list<FlowUpdatedEvent, FlowUpdatedEventsQuery>(new FlowUpdatedEventQueryHandler()),
        indexUpdatedEvent: get<IndexUpdatedEvent, IndexUpdatedEventQuery>(new IndexUpdatedEventQueryHandler()),
        indexUpdatedEvents: list<IndexUpdatedEvent, IndexUpdatedEventsQuery>(new IndexUpdatedEventQueryHandler()),
        subscriptionUnitsUpdatedEvent: get<SubscriptionUnitsUpdatedEvent, SubscriptionUnitsUpdatedEventQuery>(new SubscriptionUnitsUpdatedEventQueryHandler()),
        subscriptionUnitsUpdatedEvents: list<SubscriptionUnitsUpdatedEvent, SubscriptionUnitsUpdatedEventsQuery>(new SubscriptionUnitsUpdatedEventQueryHandler())
    };

    /**
     * Creates "get" endpoint.
     */
    function get<TReturn extends ILightEntity, TQuery extends {chainId: number} & SubgraphGetQuery>(
        queryHandler: SubgraphGetQueryHandler<TReturn> & RelevantAddressProviderFromResult<TReturn>
    ) {
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
    >(queryHandler: SubgraphListQueryHandler<TReturn, TQuery, TFilter> & RelevantAddressProviderFromFilter<TFilter>) {
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
                    'Event'
                ),
        });
    }
};
