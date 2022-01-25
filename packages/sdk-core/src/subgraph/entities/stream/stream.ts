import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphId,
    Timestamp,
} from "../../mappedSubgraphTypes";
import { Stream_Filter, Stream_OrderBy } from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    StreamsDocument,
    StreamsQuery,
    StreamsQueryVariables,
} from "./streams.generated";

export interface Stream {
    id: SubgraphId;
    createdAtBlockNumber: BlockNumber;
    createdAtTimestamp: Timestamp;
    updatedAtTimestamp: Timestamp;
    updatedAtBlockNumber: BlockNumber;
    currentFlowRate: BigNumber;
    streamedUntilUpdatedAt: BigNumber;
    receiver: Address;
    sender: Address;
    token: Address;
}

export type StreamListQuery = SubgraphListQuery<Stream_Filter, Stream_OrderBy>;

export class StreamQueryHandler extends SubgraphQueryHandler<
    Stream,
    StreamListQuery,
    StreamsQuery,
    StreamsQueryVariables
> {
    getRelevantAddressesFromFilterCore = (
        filter: Stream_Filter
    ): RelevantAddressesIntermediate => ({
        tokens: [filter.token, filter.token_in, filter.token_not_in],
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
    });

    getRelevantAddressesFromResultCore = (
        result: Stream
    ): RelevantAddressesIntermediate => ({
        tokens: [result.token],
        accounts: [result.sender, result.receiver],
    });

    mapFromSubgraphResponse = (response: StreamsQuery): Stream[] =>
        response.streams.map((x) => ({
            ...x,
            createdAtTimestamp: Number(x.createdAtTimestamp),
            createdAtBlockNumber: Number(x.createdAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            receiver: x.receiver.id,
            token: x.token.id,
            sender: x.sender.id,
        }));

    requestDocument = StreamsDocument;
}
