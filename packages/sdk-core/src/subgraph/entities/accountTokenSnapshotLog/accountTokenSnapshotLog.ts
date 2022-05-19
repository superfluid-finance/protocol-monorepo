import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphId,
    Timestamp,
} from "../../mappedSubgraphTypes";
import {
    AccountTokenSnapshot_Filter,
    AccountTokenSnapshot_OrderBy,
} from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    AccountTokenSnapshotsDocument,
    AccountTokenSnapshotsQuery,
    AccountTokenSnapshotsQueryVariables,
} from "./accountTokenSnapshots.generated";

export interface AccountTokenSnapshotLog {
    id: SubgraphId;
    transactionHash: string;
    order: number;
    logIndex: number;
    timestamp: Timestamp;
    blockNumber: BlockNumber;
    balance: BigNumber;
    maybeCriticalAtTimestamp: BigNumber;
    totalAmountStreamed: BigNumber;
    totalAmountTransferred: BigNumber;
    totalApprovedSubscriptions: number;
    totalDeposit: BigNumber;
    totalInflowRate: BigNumber;
    totalNetFlowRate: BigNumber;
    totalNumberOfActiveStreams: number;
    totalNumberOfClosedStreams: number;
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
        response.AccountTokenSnapshotLogs.map((x) => ({
            ...x,
            account: x.account.id,
            token: x.token.id,
            tokenSymbol: x.token.symbol,
            updatedAtBlockNumber: Number(x.blockNumber),
            updatedAtTimestamp: Number(x.timestamp),
        }));

    requestDocument = AccountTokenSnapshotLogsDocument;
}
