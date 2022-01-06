import {
    Address,
    BigNumber,
    EntityBase,
    RelevantAddressesIntermediate,
    SubgraphFilterOmitFieldList,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../../queryV2";
import { Stream_Filter, Stream_OrderBy } from "../../schema.generated";

import {
    StreamsDocument,
    StreamsQuery,
    StreamsQueryVariables,
} from "./streams.generated";

export interface Stream extends EntityBase {
    currentFlowRate: BigNumber;
    streamedUntilUpdatedAt: BigNumber;
    receiver: Address;
    sender: Address;
    token: Address;
}

export type StreamOrderBy = Stream_OrderBy;

export type StreamListQuery = SubgraphListQuery<
    StreamListQueryFilter,
    StreamOrderBy
>;

export type StreamListQueryFilter = Omit<
    Stream_Filter,
    SubgraphFilterOmitFieldList
>;

export class StreamQueryHandler extends SubgraphQueryHandler<
    Stream,
    StreamListQuery,
    StreamsQuery,
    Stream_Filter,
    StreamsQueryVariables
> {
    convertToSubgraphFilter = (filter: StreamListQueryFilter): Stream_Filter =>
        filter;

    getRelevantAddressesFromFilterCore = (
        filter: StreamListQueryFilter
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
