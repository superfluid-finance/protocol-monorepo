import { SubscriptionUnitsUpdatedEvent } from "../../../events";
import { mapGetAllEventsQueryEvents } from "../../../mapGetAllEventsQueryEvents";
import {
    RelevantAddressesIntermediate,
    SubgraphFilterOmitFieldList,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../../queryV2";
import {
    SubscriptionUnitsUpdatedEvent_Filter,
    SubscriptionUnitsUpdatedEvent_OrderBy,
} from "../../schema.generated";

import {
    SubscriptionUnitsUpdatedEventsDocument,
    SubscriptionUnitsUpdatedEventsQuery,
    SubscriptionUnitsUpdatedEventsQueryVariables,
} from "./subscriptionUnitsUpdatedEvents.generated";

export type SubscriptionUnitsUpdatedEventOrderBy =
    SubscriptionUnitsUpdatedEvent_OrderBy;

export type SubscriptionUnitsUpdatedEventListQueryFilter = Omit<
    SubscriptionUnitsUpdatedEvent_Filter,
    SubgraphFilterOmitFieldList
>;

export type SubscriptionUnitsUpdatedEventListQuery = SubgraphListQuery<
    SubscriptionUnitsUpdatedEventListQueryFilter,
    SubscriptionUnitsUpdatedEventOrderBy
>;

export class SubscriptionUnitsUpdatedEventQueryHandler extends SubgraphQueryHandler<
    SubscriptionUnitsUpdatedEvent,
    SubscriptionUnitsUpdatedEventListQuery,
    SubscriptionUnitsUpdatedEventsQuery,
    SubscriptionUnitsUpdatedEvent_Filter,
    SubscriptionUnitsUpdatedEventsQueryVariables
> {
    convertToSubgraphFilter(
        filter: SubscriptionUnitsUpdatedEventListQueryFilter
    ): SubscriptionUnitsUpdatedEvent_Filter {
        return filter;
    }

    protected getRelevantAddressesFromFilterCore(
        _filter: SubscriptionUnitsUpdatedEventListQueryFilter
    ): RelevantAddressesIntermediate {
        return {
            accounts: [],
            tokens: [],
        };
    }

    protected getRelevantAddressesFromResultCore(
        _result: SubscriptionUnitsUpdatedEvent
    ): RelevantAddressesIntermediate {
        return {
            accounts: [],
            tokens: [],
        };
    }

    mapFromSubgraphResponse(
        _response: SubscriptionUnitsUpdatedEventsQuery
    ): SubscriptionUnitsUpdatedEvent[] {
        return mapGetAllEventsQueryEvents(
            _response.subscriptionUnitsUpdatedEvents
        ) as SubscriptionUnitsUpdatedEvent[];
    }

    requestDocument = SubscriptionUnitsUpdatedEventsDocument;
}
