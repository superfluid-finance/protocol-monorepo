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

export interface AccountTokenSnapshot {
    balanceUntilUpdatedAt: BigNumber;
    id: SubgraphId;
    totalAmountStreamedUntilUpdatedAt: BigNumber;
    totalAmountTransferredUntilUpdatedAt: BigNumber;
    totalInflowRate: BigNumber;
    totalApprovedSubscriptions: number;
    totalNetFlowRate: BigNumber;
    totalNumberOfActiveStreams: number;
    totalOutflowRate: BigNumber;
    maybeCriticalAtTimestamp: BigNumber;
    isLiquidationEstimateOptimistic: boolean;
    totalNumberOfClosedStreams: number;
    totalSubscriptionsWithUnits: number;
    updatedAtBlockNumber: BlockNumber;
    updatedAtTimestamp: Timestamp;
    account: Address;
    token: Address;
    tokenSymbol: string;
}

export type AccountTokenSnapshotListQuery = SubgraphListQuery<
    AccountTokenSnapshot_Filter,
    AccountTokenSnapshot_OrderBy
>;

export class AccountTokenSnapshotQueryHandler extends SubgraphQueryHandler<
    AccountTokenSnapshot,
    AccountTokenSnapshotListQuery,
    AccountTokenSnapshotsQuery,
    AccountTokenSnapshotsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof AccountTokenSnapshot_Filter)[];
        tokenKeys: (keyof AccountTokenSnapshot_Filter)[];
    } => ({
        accountKeys: ["account"],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore = (
        result: AccountTokenSnapshot
    ): RelevantAddressesIntermediate => ({
        tokens: [result.token],
        accounts: [result.account],
    });

    mapFromSubgraphResponse = (
        response: AccountTokenSnapshotsQuery
    ): AccountTokenSnapshot[] =>
        response.accountTokenSnapshots.map((x) => ({
            ...x,
            account: x.account.id,
            token: x.token.id,
            tokenSymbol: x.token.symbol,
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
        }));

    requestDocument = AccountTokenSnapshotsDocument;
}
