import { AllEvents } from "../../events";
import { mapGetAllEventsQueryEvents } from "../../mapGetAllEventsQueryEvents";
import { Event_Filter, Event_OrderBy } from "../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../subgraphQueryHandler";

import {
    EventsDocument,
    EventsQuery,
    EventsQueryVariables,
} from "./events.generated";

export type EventListQuery = SubgraphListQuery<Event_Filter, Event_OrderBy>;

export class EventQueryHandler extends SubgraphQueryHandler<
    AllEvents,
    EventListQuery,
    EventsQuery,
    EventsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof Event_Filter)[];
        tokenKeys: (keyof Event_Filter)[];
    } => ({
        accountKeys: ["addresses"], // Note that "addresses" can contain both accounts and tokens. Not sure what the best thing to do here is.
        tokenKeys: [],
    });

    getRelevantAddressesFromResultCore(): RelevantAddressesIntermediate {
        return {
            accounts: [],
            tokens: [],
        };
    }

    mapFromSubgraphResponse(response: EventsQuery): AllEvents[] {
        return mapGetAllEventsQueryEvents(response.events);
    }

    requestDocument = EventsDocument;
}
