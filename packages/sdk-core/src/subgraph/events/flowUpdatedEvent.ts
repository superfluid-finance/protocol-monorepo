import { FlowUpdatedEvent } from "../../events";
import { mapGetAllEventsQueryEvents } from "../../mapGetAllEventsQueryEvents";
import {
    FlowUpdatedEvent_Filter,
    FlowUpdatedEvent_OrderBy,
} from "../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../subgraphQueryHandler";

import {
    FlowUpdatedEventsDocument,
    FlowUpdatedEventsQuery,
    FlowUpdatedEventsQueryVariables,
} from "./events.generated";

export type FlowUpdatedEventListQuery = SubgraphListQuery<
    FlowUpdatedEvent_Filter,
    FlowUpdatedEvent_OrderBy
>;

export class FlowUpdatedEventQueryHandler extends SubgraphQueryHandler<
    FlowUpdatedEvent,
    FlowUpdatedEventListQuery,
    FlowUpdatedEventsQuery,
    FlowUpdatedEventsQueryVariables
> {
    protected getRelevantAddressesFromFilterCore(
        filter: FlowUpdatedEvent_Filter
    ): RelevantAddressesIntermediate {
        return {
            accounts: [
                filter.sender,
                filter.sender_in,
                filter.sender_not,
                filter.sender_not_in,
                filter.receiver,
                filter.receiver_in,
                filter.receiver_not,
                filter.receiver_not_in,
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
        _result: FlowUpdatedEvent
    ): RelevantAddressesIntermediate {
        return {
            accounts: [],
            tokens: [],
        };
    }

    mapFromSubgraphResponse(
        response: FlowUpdatedEventsQuery
    ): FlowUpdatedEvent[] {
        return mapGetAllEventsQueryEvents(
            response.flowUpdatedEvents
        ) as FlowUpdatedEvent[];
    }

    requestDocument = FlowUpdatedEventsDocument;
}
