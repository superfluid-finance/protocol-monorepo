import {
    EventListQuery,
    FlowUpdatedEventListQuery,
    IndexUpdatedEventListQuery,
    SubgraphGetQuery,
    SubscriptionUnitsUpdatedEventListQuery,
} from '@superfluid-finance/sdk-core';

export interface EventQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface EventsQuery extends EventListQuery {
    chainId: number;
}

export interface FlowUpdatedEventQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface FlowUpdatedEventsQuery extends FlowUpdatedEventListQuery {
    chainId: number;
}

export interface IndexUpdatedEventQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface IndexUpdatedEventsQuery extends IndexUpdatedEventListQuery {
    chainId: number;
}

export interface SubscriptionUnitsUpdatedEventQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface SubscriptionUnitsUpdatedEventsQuery extends SubscriptionUnitsUpdatedEventListQuery {
    chainId: number;
}
