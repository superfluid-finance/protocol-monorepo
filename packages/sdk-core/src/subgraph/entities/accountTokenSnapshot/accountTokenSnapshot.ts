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
    AccountTokenSnapshot_Filter,
    AccountTokenSnapshot_OrderBy,
} from "../../schema.generated";

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
    totalNumberOfClosedStreams: number;
    totalSubscriptionsWithUnits: number;
    updatedAtBlockNumber: BlockNumber;
    updatedAtTimestamp: Timestamp;
    account: Address;
    token: Address;
}

export type AccountTokenSnapshotListQuery = SubgraphListQuery<
    AccountTokenSnapshotListQueryFilter,
    AccountTokenSnapshot_OrderBy
>;

export type AccountTokenSnapshotListQueryFilter = Omit<
    AccountTokenSnapshot_Filter,
    SubgraphFilterOmitFieldList
>;

export class AccountTokenSnapshotQueryHandler extends SubgraphQueryHandler<
    AccountTokenSnapshot,
    AccountTokenSnapshotListQuery,
    AccountTokenSnapshotsQuery,
    AccountTokenSnapshot_Filter,
    AccountTokenSnapshotsQueryVariables
> {
    convertToSubgraphFilter = (
        filter: AccountTokenSnapshotListQueryFilter
    ): AccountTokenSnapshot_Filter => filter;

    protected getRelevantAddressesFromFilterCore = (
        filter: AccountTokenSnapshotListQueryFilter
    ): RelevantAddressesIntermediate => ({
        tokens: [filter.token, filter.token_in, filter.token_not_in],
        accounts: [filter.account, filter.account_in, filter.account_not_in],
    });

    protected getRelevantAddressesFromResultCore = (
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
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
        }));

    requestDocument = AccountTokenSnapshotsDocument;
}
