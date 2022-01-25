import { SubscriptionUnitsUpdatedEvent } from "../../../events";
import { mapGetAllEventsQueryEvents } from "../../../mapGetAllEventsQueryEvents";
import {
    SubscriptionUnitsUpdatedEvent_Filter,
    SubscriptionUnitsUpdatedEvent_OrderBy,
} from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    SubscriptionUnitsUpdatedEventsDocument,
    SubscriptionUnitsUpdatedEventsQuery,
    SubscriptionUnitsUpdatedEventsQueryVariables,
} from "./subscriptionUnitsUpdatedEvents.generated";

export type SubscriptionUnitsUpdatedEventListQuery = SubgraphListQuery<
    SubscriptionUnitsUpdatedEvent_Filter,
    SubscriptionUnitsUpdatedEvent_OrderBy
>;

export class SubscriptionUnitsUpdatedEventQueryHandler extends SubgraphQueryHandler<
    SubscriptionUnitsUpdatedEvent,
    SubscriptionUnitsUpdatedEventListQuery,
    SubscriptionUnitsUpdatedEventsQuery,
    SubscriptionUnitsUpdatedEventsQueryVariables
> {
    protected getRelevantAddressesFromFilterCore(
        filter: SubscriptionUnitsUpdatedEvent_Filter
    ): RelevantAddressesIntermediate {
        return {
            accounts: [
                filter.publisher,
                filter.publisher_in,
                filter.publisher_not,
                filter.publisher_not_in,
                filter.subscriber,
                filter.subscriber_in,
                filter.subscriber_not,
                filter.subscriber_not_in,
            ],
            tokens: [
                filter.token,
                filter.token_in,
                filter.token_not,
                filter.token_not_in,
            ],
        };
    }

    protected getRelevantAddressesFromResultCore(
        result: SubscriptionUnitsUpdatedEvent
    ): RelevantAddressesIntermediate {
        return {
            accounts: [result.publisher, result.subscriber],
            tokens: [result.token],
        };
    }

    mapFromSubgraphResponse(
        response: SubscriptionUnitsUpdatedEventsQuery
    ): SubscriptionUnitsUpdatedEvent[] {
        return mapGetAllEventsQueryEvents(
            response.subscriptionUnitsUpdatedEvents
        ) as SubscriptionUnitsUpdatedEvent[];
    }

    requestDocument = SubscriptionUnitsUpdatedEventsDocument;
}
