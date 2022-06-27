import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphId,
    Timestamp,
} from "../../mappedSubgraphTypes";
import {
    TokenStatisticLog_Filter,
    TokenStatisticLog_OrderBy,
} from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    TokenStatisticLogsDocument,
    TokenStatisticLogsQuery,
    TokenStatisticLogsQueryVariables,
} from "./tokenStatisticLogs.generated";

export interface TokenStatisticLog {
    id: SubgraphId;
    timestamp: Timestamp;
    blockNumber: BlockNumber;
    logIndex: BigNumber;
    order: BigNumber;
    triggeredByEventName: string;
    totalNumberOfActiveStreams: number;
    totalNumberOfClosedStreams: number;
    totalNumberOfIndexes: number;
    totalNumberOfActiveIndexes: number;
    totalSubscriptionsWithUnits: number;
    totalApprovedSubscriptions: number;
    totalDeposit: BigNumber;
    totalOutflowRate: BigNumber;
    totalAmountStreamed: BigNumber;
    totalAmountTransferred: BigNumber;
    totalAmountDistributed: BigNumber;
    totalSupply: BigNumber;
    token: Address;
}

export type TokenStatisticLogListQuery = SubgraphListQuery<
    TokenStatisticLog_Filter,
    TokenStatisticLog_OrderBy
>;

export class TokenStatisticLogQueryHandler extends SubgraphQueryHandler<
    TokenStatisticLog,
    TokenStatisticLogListQuery,
    TokenStatisticLogsQuery,
    TokenStatisticLogsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof TokenStatisticLog_Filter)[];
        tokenKeys: (keyof TokenStatisticLog_Filter)[];
    } => ({
        accountKeys: [],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore = (
        result: TokenStatisticLog
    ): RelevantAddressesIntermediate => ({
        tokens: [result.token],
        accounts: [],
    });

    mapFromSubgraphResponse = (
        response: TokenStatisticLogsQuery
    ): TokenStatisticLog[] =>
        response.tokenStatisticLogs.map((x) => ({
            ...x,
            blockNumber: Number(x.blockNumber),
            timestamp: Number(x.timestamp),
            token: x.token.id,
        }));

    requestDocument = TokenStatisticLogsDocument;
}
