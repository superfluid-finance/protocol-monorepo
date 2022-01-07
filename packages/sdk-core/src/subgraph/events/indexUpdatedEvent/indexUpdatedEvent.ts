import { IndexUpdatedEvent } from "../../../events";
import { mapGetAllEventsQueryEvents } from "../../../mapGetAllEventsQueryEvents";
import {
    RelevantAddressesIntermediate,
    SubgraphFilterOmitFieldList,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../../queryV2";
import {
    IndexUpdatedEvent_Filter,
    IndexUpdatedEvent_OrderBy,
} from "../../schema.generated";

import {
    IndexUpdatedEventsDocument,
    IndexUpdatedEventsQuery,
    IndexUpdatedEventsQueryVariables,
} from "./indexUpdatedEvents.generated";

export type IndexUpdatedEventOrderBy = IndexUpdatedEvent_OrderBy;

export type IndexUpdatedEventListQueryFilter = Omit<
    IndexUpdatedEvent_Filter,
    SubgraphFilterOmitFieldList
>;

export type IndexUpdatedEventListQuery = SubgraphListQuery<
    IndexUpdatedEventListQueryFilter,
    IndexUpdatedEventOrderBy
>;

export class IndexUpdatedEventQueryHandler extends SubgraphQueryHandler<
    IndexUpdatedEvent,
    IndexUpdatedEventListQuery,
    IndexUpdatedEventsQuery,
    IndexUpdatedEvent_Filter,
    IndexUpdatedEventsQueryVariables
> {
    convertToSubgraphFilter(
        filter: IndexUpdatedEventListQueryFilter
    ): IndexUpdatedEvent_Filter {
        return filter;
    }

    protected getRelevantAddressesFromFilterCore(
        _filter: IndexUpdatedEventListQueryFilter
    ): RelevantAddressesIntermediate {
        return {
            accounts: [],
            tokens: [],
        };
    }

    protected getRelevantAddressesFromResultCore(
        _result: IndexUpdatedEvent
    ): RelevantAddressesIntermediate {
        return {
            accounts: [],
            tokens: [],
        };
    }

    mapFromSubgraphResponse(
        _response: IndexUpdatedEventsQuery
    ): IndexUpdatedEvent[] {
        return mapGetAllEventsQueryEvents(
            _response.indexUpdatedEvents
        ) as IndexUpdatedEvent[];
    }

    requestDocument = IndexUpdatedEventsDocument;
}
