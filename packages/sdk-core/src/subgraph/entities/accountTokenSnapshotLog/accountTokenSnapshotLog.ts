import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphId,
    Timestamp,
} from "../../mappedSubgraphTypes";
import {
    AccountTokenSnapshotLog_Filter,
    AccountTokenSnapshotLog_OrderBy,
} from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    AccountTokenSnapshotLogsDocument,
    AccountTokenSnapshotLogsQuery,
    AccountTokenSnapshotLogsQueryVariables,
} from "./accountTokenSnapshotLogs.generated";

export interface AccountTokenSnapshotLog {
    id: SubgraphId;
    transactionHash: string;
    order: number;
    logIndex: number;
    timestamp: Timestamp;
    blockNumber: BlockNumber;
    balance: BigNumber;
    maybeCriticalAtTimestamp: Timestamp | null;
    totalAmountStreamed: BigNumber;
    totalAmountTransferred: BigNumber;
    totalApprovedSubscriptions: number;
    totalDeposit: BigNumber;
    totalInflowRate: BigNumber;
    totalNetFlowRate: BigNumber;
    totalNumberOfActiveStreams: number;
    activeOutgoingStreamCount: number;
    activeIncomingStreamCount: number;
    totalNumberOfClosedStreams: number;
    inactiveOutgoingStreamCount: number;
    inactiveIncomingStreamCount: number;
    totalSubscriptionsWithUnits: number;
    totalOutflowRate: BigNumber;
    triggeredByEventName: string;
    account: Address;
    token: Address;
    tokenSymbol: string;
}

export type AccountTokenSnapshotLogListQuery = SubgraphListQuery<
    AccountTokenSnapshotLog_Filter,
    AccountTokenSnapshotLog_OrderBy
>;

export class AccountTokenSnapshotLogQueryHandler extends SubgraphQueryHandler<
    AccountTokenSnapshotLog,
    AccountTokenSnapshotLogListQuery,
    AccountTokenSnapshotLogsQuery,
    AccountTokenSnapshotLogsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof AccountTokenSnapshotLog_Filter)[];
        tokenKeys: (keyof AccountTokenSnapshotLog_Filter)[];
    } => ({
        accountKeys: ["account"],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore = (
        result: AccountTokenSnapshotLog
    ): RelevantAddressesIntermediate => ({
        tokens: [result.token],
        accounts: [result.account],
    });

    mapFromSubgraphResponse = (
        response: AccountTokenSnapshotLogsQuery
    ): AccountTokenSnapshotLog[] =>
        response.accountTokenSnapshotLogs.map((x) => ({
            ...x,
            account: x.account.id,
            token: x.token.id,
            tokenSymbol: x.token.symbol,
            maybeCriticalAtTimestamp:
                x.maybeCriticalAtTimestamp != null
                    ? Number(x.maybeCriticalAtTimestamp)
                    : null,
            blockNumber: Number(x.blockNumber),
            timestamp: Number(x.timestamp),
            order: Number(x.order),
            logIndex: Number(x.logIndex),
        }));

    requestDocument = AccountTokenSnapshotLogsDocument;
}
