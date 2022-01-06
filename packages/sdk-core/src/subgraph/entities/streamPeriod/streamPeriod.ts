import {
    Address,
    BigNumber,
    BlockNumber,
    RelevantAddressesIntermediate,
    SubgraphFilterOmitFieldList,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
    Timestamp,
} from "../../../queryV2";
import {
    StreamPeriod_Filter,
    StreamPeriod_OrderBy
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

export type StreamPeriodOrderBy = StreamPeriod_OrderBy;

export type StreamPeriodListQuery = SubgraphListQuery<
    StreamPeriodListQueryFilter,
    StreamPeriodOrderBy
>;

export type StreamPeriodListQueryFilter = Omit<
    StreamPeriod_Filter,
    SubgraphFilterOmitFieldList
>;

export class StreamPeriodQueryHandler extends SubgraphQueryHandler<
    StreamPeriod,
    StreamPeriodListQuery,
    StreamPeriodsQuery,
    StreamPeriod_Filter,
    StreamPeriodsQueryVariables
> {
    convertToSubgraphFilter = (
        filter: StreamPeriodListQueryFilter
    ): StreamPeriod_Filter => filter;

    protected getRelevantAddressesFromFilterCore = (
        filter: StreamPeriodListQueryFilter
    ): RelevantAddressesIntermediate => ({
        tokens: [filter.token, filter.token_in, filter.token_not_in],
        accounts: [
            filter.sender,
            filter.sender_in,
            filter.sender_not_in,
            filter.receiver,
            filter.receiver_in,
            filter.receiver_not_in,
        ],
    });

    protected getRelevantAddressesFromResultCore = (
        result: StreamPeriod
    ): RelevantAddressesIntermediate => ({
        tokens: [result.token],
        accounts: [result.sender, result.receiver],
    });

    mapFromSubgraphResponse = (response: StreamPeriodsQuery): StreamPeriod[] =>
        response.streamPeriods.map((x) => ({
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

    requestDocument = StreamPeriodsDocument;
}
