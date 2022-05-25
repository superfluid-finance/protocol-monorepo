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
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof FlowUpdatedEvent_Filter)[];
        tokenKeys: (keyof FlowUpdatedEvent_Filter)[];
    } => ({
        accountKeys: ["sender", "receiver", "flowOperator"],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore(
        result: FlowUpdatedEvent
    ): RelevantAddressesIntermediate {
        return {
            accounts: [result.sender, result.receiver],
            tokens: [result.token],
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
