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

import {getFramework} from '../../../../sdkReduxConfig';
import {CacheTime} from '../../cacheTime';
import {provideCacheTagsFromRelevantAddresses} from '../provideCacheTagsFromRelevantAddresses';
import {SubgraphApiEndpointBuilder} from '../subgraphApiEndpointBuilder';

import {
    EventQuery,
    EventsQuery,
    FlowUpdatedEventQuery,
    FlowUpdatedEventsQuery,
    IndexUpdatedEventQuery,
    IndexUpdatedEventsQuery,
    SubscriptionUnitsUpdatedEventQuery,
    SubscriptionUnitsUpdatedEventsQuery,
} from './eventArgs';

export const createEventQueryEndpoints = (builder: SubgraphApiEndpointBuilder) => {
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
    builder: SubgraphApiEndpointBuilder,
    queryHandler: SubgraphGetQueryHandler<TReturn> & RelevantAddressProviderFromResult<TReturn>
) {
    return builder.query<TReturn | null, TQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            return {
                data: await queryHandler.get(framework.query.subgraphClient, arg),
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
    builder: SubgraphApiEndpointBuilder,
    queryHandler: SubgraphListQueryHandler<TReturn, TQuery, TFilter> & RelevantAddressProviderFromFilter<TFilter>
) {
    return builder.query<PagedResult<TReturn>, TQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            return {
                data: await queryHandler.list(framework.query.subgraphClient, arg),
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
