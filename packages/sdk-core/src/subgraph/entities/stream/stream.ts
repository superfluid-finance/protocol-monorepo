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
    tokenSymbol: string;
    deposit: BigNumber;
    userData: string;
}

export type StreamListQuery = SubgraphListQuery<Stream_Filter, Stream_OrderBy>;

export class StreamQueryHandler extends SubgraphQueryHandler<
    Stream,
    StreamListQuery,
    StreamsQuery,
    StreamsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof Stream_Filter)[];
        tokenKeys: (keyof Stream_Filter)[];
    } => ({
        accountKeys: ["sender", "receiver"],
        tokenKeys: ["token"],
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
            tokenSymbol: x.token.symbol,
            sender: x.sender.id,
        }));

    requestDocument = StreamsDocument;
}
