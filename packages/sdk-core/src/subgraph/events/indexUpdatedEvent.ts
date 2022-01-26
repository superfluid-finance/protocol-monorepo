import { IndexUpdatedEvent } from "../../events";
import { mapGetAllEventsQueryEvents } from "../../mapGetAllEventsQueryEvents";
import {
    IndexUpdatedEvent_Filter,
    IndexUpdatedEvent_OrderBy,
} from "../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../subgraphQueryHandler";

import {
    IndexUpdatedEventsDocument,
    IndexUpdatedEventsQuery,
    IndexUpdatedEventsQueryVariables,
} from "./events.generated";

export type IndexUpdatedEventListQuery = SubgraphListQuery<
    IndexUpdatedEvent_Filter,
    IndexUpdatedEvent_OrderBy
>;

export class IndexUpdatedEventQueryHandler extends SubgraphQueryHandler<
    IndexUpdatedEvent,
    IndexUpdatedEventListQuery,
    IndexUpdatedEventsQuery,
    IndexUpdatedEventsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof IndexUpdatedEvent_Filter)[];
        tokenKeys: (keyof IndexUpdatedEvent_Filter)[];
    } => ({
        accountKeys: ["publisher"],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore(
        result: IndexUpdatedEvent
    ): RelevantAddressesIntermediate {
        return {
            accounts: [result.publisher],
            tokens: [result.token],
        };
    }

    mapFromSubgraphResponse(
        response: IndexUpdatedEventsQuery
    ): IndexUpdatedEvent[] {
        return mapGetAllEventsQueryEvents(
            response.indexUpdatedEvents
        ) as IndexUpdatedEvent[];
    }

    requestDocument = IndexUpdatedEventsDocument;
}
