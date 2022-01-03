import {
    Address,
    BigNumber,
    EntityBase,
    EntityFilterBase,
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../../queryV2";
import {
    InputMaybe,
    Scalars,
    Stream_Filter,
    Stream_OrderBy,
} from "../../schema.generated";

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

export type StreamListQuery = SubgraphListQuery<
    StreamListQueryFilter,
    Stream_OrderBy
>;

export interface StreamListQueryFilter extends EntityFilterBase {
    currentFlowRate?: InputMaybe<Scalars["BigInt"]>;
    currentFlowRate_gt?: InputMaybe<Scalars["BigInt"]>;
    currentFlowRate_gte?: InputMaybe<Scalars["BigInt"]>;
    currentFlowRate_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    currentFlowRate_lt?: InputMaybe<Scalars["BigInt"]>;
    currentFlowRate_lte?: InputMaybe<Scalars["BigInt"]>;
    currentFlowRate_not?: InputMaybe<Scalars["BigInt"]>;
    currentFlowRate_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    receiver?: InputMaybe<Scalars["String"]>;
    receiver_in?: InputMaybe<Array<Scalars["String"]>>;
    receiver_not_in?: InputMaybe<Array<Scalars["String"]>>;
    sender?: InputMaybe<Scalars["String"]>;
    sender_in?: InputMaybe<Array<Scalars["String"]>>;
    sender_not_in?: InputMaybe<Array<Scalars["String"]>>;
    streamedUntilUpdatedAt?: InputMaybe<Scalars["BigInt"]>;
    streamedUntilUpdatedAt_gt?: InputMaybe<Scalars["BigInt"]>;
    streamedUntilUpdatedAt_gte?: InputMaybe<Scalars["BigInt"]>;
    streamedUntilUpdatedAt_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    streamedUntilUpdatedAt_lt?: InputMaybe<Scalars["BigInt"]>;
    streamedUntilUpdatedAt_lte?: InputMaybe<Scalars["BigInt"]>;
    streamedUntilUpdatedAt_not?: InputMaybe<Scalars["BigInt"]>;
    streamedUntilUpdatedAt_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    token?: InputMaybe<Scalars["String"]>;
    token_in?: InputMaybe<Array<Scalars["String"]>>;
    token_not_in?: InputMaybe<Array<Scalars["String"]>>;
}

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
            filter.sender_not_in,
            filter.receiver,
            filter.receiver_in,
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
