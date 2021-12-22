import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphGetQuery,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
    Timestamp,
} from "../../../queryV2";
import {
    InputMaybe,
    Scalars,
    StreamPeriod_Filter,
    StreamPeriod_OrderBy,
} from "../../schema.generated";

import {
    StreamPeriodsDocument,
    StreamPeriodsQuery,
    StreamPeriodsQueryVariables,
} from "./streamPeriods.generated";

export interface StreamPeriod {
    id: SubgraphId;
    flowRate: BigNumber;
    startedAtBlockNumber: BlockNumber;
    startedAtTimestamp: Timestamp;
    stoppedAtBlockNumber?: BlockNumber;
    stoppedAtTimestamp?: Timestamp;
    totalAmountStreamed?: BigNumber;
    token: Address;
    stream: SubgraphId;
    sender: Address;
    receiver: Address;
    stoppedAtEvent?: SubgraphId;
    startedAtEvent: SubgraphId;
}

export type StreamPeriodGetQuery = SubgraphGetQuery<StreamPeriod>;

export type StreamPeriodListQuery = SubgraphListQuery<
    StreamPeriod,
    StreamPeriodListQueryFilter,
    StreamPeriod_OrderBy
>;

export interface StreamPeriodListQueryFilter {
    flowRate?: InputMaybe<Scalars["BigInt"]>;
    flowRate_gt?: InputMaybe<Scalars["BigInt"]>;
    flowRate_gte?: InputMaybe<Scalars["BigInt"]>;
    flowRate_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    flowRate_lt?: InputMaybe<Scalars["BigInt"]>;
    flowRate_lte?: InputMaybe<Scalars["BigInt"]>;
    flowRate_not?: InputMaybe<Scalars["BigInt"]>;
    flowRate_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    receiver?: InputMaybe<Scalars["String"]>;
    receiver_in?: InputMaybe<Array<Scalars["String"]>>;
    receiver_not_in?: InputMaybe<Array<Scalars["String"]>>;
    sender?: InputMaybe<Scalars["String"]>;
    sender_in?: InputMaybe<Array<Scalars["String"]>>;
    sender_not_in?: InputMaybe<Array<Scalars["String"]>>;
    startedAtBlockNumber?: InputMaybe<Scalars["BigInt"]>;
    startedAtBlockNumber_gt?: InputMaybe<Scalars["BigInt"]>;
    startedAtBlockNumber_gte?: InputMaybe<Scalars["BigInt"]>;
    startedAtBlockNumber_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    startedAtBlockNumber_lt?: InputMaybe<Scalars["BigInt"]>;
    startedAtBlockNumber_lte?: InputMaybe<Scalars["BigInt"]>;
    startedAtBlockNumber_not?: InputMaybe<Scalars["BigInt"]>;
    startedAtBlockNumber_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    startedAtEvent?: InputMaybe<Scalars["String"]>;
    startedAtEvent_in?: InputMaybe<Array<Scalars["String"]>>;
    startedAtEvent_not_in?: InputMaybe<Array<Scalars["String"]>>;
    startedAtTimestamp?: InputMaybe<Scalars["BigInt"]>;
    startedAtTimestamp_gt?: InputMaybe<Scalars["BigInt"]>;
    startedAtTimestamp_gte?: InputMaybe<Scalars["BigInt"]>;
    startedAtTimestamp_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    startedAtTimestamp_lt?: InputMaybe<Scalars["BigInt"]>;
    startedAtTimestamp_lte?: InputMaybe<Scalars["BigInt"]>;
    startedAtTimestamp_not?: InputMaybe<Scalars["BigInt"]>;
    startedAtTimestamp_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    stoppedAtBlockNumber?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtBlockNumber_gt?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtBlockNumber_gte?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtBlockNumber_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    stoppedAtBlockNumber_lt?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtBlockNumber_lte?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtBlockNumber_not?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtBlockNumber_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    stoppedAtEvent?: InputMaybe<Scalars["String"]>;
    stoppedAtEvent_in?: InputMaybe<Array<Scalars["String"]>>;
    stoppedAtEvent_not_in?: InputMaybe<Array<Scalars["String"]>>;
    stoppedAtTimestamp?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtTimestamp_gt?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtTimestamp_gte?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtTimestamp_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    stoppedAtTimestamp_lt?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtTimestamp_lte?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtTimestamp_not?: InputMaybe<Scalars["BigInt"]>;
    stoppedAtTimestamp_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    stream?: InputMaybe<Scalars["String"]>;
    stream_in?: InputMaybe<Array<Scalars["String"]>>;
    stream_not_in?: InputMaybe<Array<Scalars["String"]>>;
    token?: InputMaybe<Scalars["String"]>;
    token_in?: InputMaybe<Array<Scalars["String"]>>;
    token_not_in?: InputMaybe<Array<Scalars["String"]>>;
    totalAmountStreamed?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamed_gt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamed_gte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamed_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalAmountStreamed_lt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamed_lte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamed_not?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamed_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
}

export class StreamPeriodQueryHandler extends SubgraphQueryHandler<
    StreamPeriod,
    StreamPeriodListQueryFilter,
    StreamPeriod_OrderBy,
    StreamPeriodsQuery,
    StreamPeriod_Filter,
    StreamPeriodsQueryVariables
> {
    convertToSubgraphFilter(
        filter: StreamPeriodListQueryFilter
    ): StreamPeriod_Filter {
        return filter;
    }

    mapFromSubgraphResponse(response: StreamPeriodsQuery): StreamPeriod[] {
        return response.streamPeriods.map((x) => ({
            ...x,
            stream: x.stream.id,
            token: x.token.id,
            sender: x.sender.id,
            receiver: x.receiver.id,
            startedAtEvent: x.startedAtEvent.id,
            stoppedAtEvent: x.stoppedAtEvent?.id,
            startedAtBlockNumber: Number(x.startedAtBlockNumber),
            stoppedAtBlockNumber: Number(x.stoppedAtBlockNumber),
            startedAtTimestamp: Number(x.startedAtTimestamp),
            stoppedAtTimestamp: Number(x.stoppedAtTimestamp),
        }));
    }

    requestDocument = StreamPeriodsDocument;
}
