import { TransferEvent } from "../../events";
import { mapGetAllEventsQueryEvents } from "../../mapGetAllEventsQueryEvents";
import {
    TransferEvent_Filter,
    TransferEvent_OrderBy,
} from "../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../subgraphQueryHandler";

import {
    TransferEventsDocument,
    TransferEventsQuery,
    TransferEventsQueryVariables,
} from "./events.generated";

export type TransferEventListQuery = SubgraphListQuery<
    TransferEvent_Filter,
    TransferEvent_OrderBy
>;

export class TransferEventQueryHandler extends SubgraphQueryHandler<
    TransferEvent,
    TransferEventListQuery,
    TransferEventsQuery,
    TransferEventsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof TransferEvent_Filter)[];
        tokenKeys: (keyof TransferEvent_Filter)[];
    } => ({
        accountKeys: ["from", "to"],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore(
        result: TransferEvent
    ): RelevantAddressesIntermediate {
        return {
            accounts: [result.from, result.to],
            tokens: [result.token],
        };
    }

    mapFromSubgraphResponse(response: TransferEventsQuery): TransferEvent[] {
        return mapGetAllEventsQueryEvents(
            response.transferEvents
        ) as TransferEvent[];
    }

    requestDocument = TransferEventsDocument;
}
