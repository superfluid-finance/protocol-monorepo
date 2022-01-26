import { SubscriptionUnitsUpdatedEvent } from "../../events";
import { mapGetAllEventsQueryEvents } from "../../mapGetAllEventsQueryEvents";
import {
    SubscriptionUnitsUpdatedEvent_Filter,
    SubscriptionUnitsUpdatedEvent_OrderBy,
} from "../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../subgraphQueryHandler";

import {
    SubscriptionUnitsUpdatedEventsDocument,
    SubscriptionUnitsUpdatedEventsQuery,
    SubscriptionUnitsUpdatedEventsQueryVariables,
} from "./events.generated";

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
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof SubscriptionUnitsUpdatedEvent_Filter)[];
        tokenKeys: (keyof SubscriptionUnitsUpdatedEvent_Filter)[];
    } => ({
        accountKeys: ["publisher", "subscriber"],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore(
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
