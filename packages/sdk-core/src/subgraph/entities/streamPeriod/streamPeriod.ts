import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphId,
    Timestamp,
} from "../../mappedSubgraphTypes";
import {
    StreamPeriod_Filter,
    StreamPeriod_OrderBy,
} from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

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
    tokenSymbol: string;
    stream: SubgraphId;
    sender: Address;
    receiver: Address;
    stoppedAtEvent?: SubgraphId;
    startedAtEvent: SubgraphId;
}

export type StreamPeriodListQuery = SubgraphListQuery<
    StreamPeriod_Filter,
    StreamPeriod_OrderBy
>;

export class StreamPeriodQueryHandler extends SubgraphQueryHandler<
    StreamPeriod,
    StreamPeriodListQuery,
    StreamPeriodsQuery,
    StreamPeriodsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof StreamPeriod_Filter)[];
        tokenKeys: (keyof StreamPeriod_Filter)[];
    } => ({
        accountKeys: ["sender", "receiver"],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore = (
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
            tokenSymbol: x.token.symbol,
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
