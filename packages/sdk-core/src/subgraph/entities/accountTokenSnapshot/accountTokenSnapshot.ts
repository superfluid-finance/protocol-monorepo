import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphGetQuery,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
    Timestamp,
} from "../../../queryV2";
import {
    AccountTokenSnapshot_Filter,
    AccountTokenSnapshot_OrderBy,
    InputMaybe,
    Scalars,
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

export type AccountTokenSnapshotGetQuery =
    SubgraphGetQuery<AccountTokenSnapshot>;

export type AccountTokenSnapshotListQuery = SubgraphListQuery<
    AccountTokenSnapshot,
    any,
    AccountTokenSnapshot_OrderBy
>;

export interface AccountTokenSnapshotListQueryFilter {
    account?: InputMaybe<Scalars["String"]>;
    account_in?: InputMaybe<Array<Scalars["String"]>>;
    account_not_in?: InputMaybe<Array<Scalars["String"]>>;
    balanceUntilUpdatedAt?: InputMaybe<Scalars["BigInt"]>;
    balanceUntilUpdatedAt_gt?: InputMaybe<Scalars["BigInt"]>;
    balanceUntilUpdatedAt_gte?: InputMaybe<Scalars["BigInt"]>;
    balanceUntilUpdatedAt_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    balanceUntilUpdatedAt_lt?: InputMaybe<Scalars["BigInt"]>;
    balanceUntilUpdatedAt_lte?: InputMaybe<Scalars["BigInt"]>;
    balanceUntilUpdatedAt_not?: InputMaybe<Scalars["BigInt"]>;
    balanceUntilUpdatedAt_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    token?: InputMaybe<Scalars["String"]>;
    token_in?: InputMaybe<Array<Scalars["String"]>>;
    token_not_in?: InputMaybe<Array<Scalars["String"]>>;
    totalAmountStreamedUntilUpdatedAt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamedUntilUpdatedAt_gt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamedUntilUpdatedAt_gte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamedUntilUpdatedAt_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalAmountStreamedUntilUpdatedAt_lt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamedUntilUpdatedAt_lte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamedUntilUpdatedAt_not?: InputMaybe<Scalars["BigInt"]>;
    totalAmountStreamedUntilUpdatedAt_not_in?: InputMaybe<
        Array<Scalars["BigInt"]>
    >;
    totalAmountTransferredUntilUpdatedAt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountTransferredUntilUpdatedAt_gt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountTransferredUntilUpdatedAt_gte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountTransferredUntilUpdatedAt_in?: InputMaybe<
        Array<Scalars["BigInt"]>
    >;
    totalAmountTransferredUntilUpdatedAt_lt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountTransferredUntilUpdatedAt_lte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountTransferredUntilUpdatedAt_not?: InputMaybe<Scalars["BigInt"]>;
    totalAmountTransferredUntilUpdatedAt_not_in?: InputMaybe<
        Array<Scalars["BigInt"]>
    >;
    totalApprovedSubscriptions?: InputMaybe<Scalars["Int"]>;
    totalApprovedSubscriptions_gt?: InputMaybe<Scalars["Int"]>;
    totalApprovedSubscriptions_gte?: InputMaybe<Scalars["Int"]>;
    totalApprovedSubscriptions_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalApprovedSubscriptions_lt?: InputMaybe<Scalars["Int"]>;
    totalApprovedSubscriptions_lte?: InputMaybe<Scalars["Int"]>;
    totalApprovedSubscriptions_not?: InputMaybe<Scalars["Int"]>;
    totalApprovedSubscriptions_not_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalInflowRate?: InputMaybe<Scalars["BigInt"]>;
    totalInflowRate_gt?: InputMaybe<Scalars["BigInt"]>;
    totalInflowRate_gte?: InputMaybe<Scalars["BigInt"]>;
    totalInflowRate_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalInflowRate_lt?: InputMaybe<Scalars["BigInt"]>;
    totalInflowRate_lte?: InputMaybe<Scalars["BigInt"]>;
    totalInflowRate_not?: InputMaybe<Scalars["BigInt"]>;
    totalInflowRate_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalNetFlowRate?: InputMaybe<Scalars["BigInt"]>;
    totalNetFlowRate_gt?: InputMaybe<Scalars["BigInt"]>;
    totalNetFlowRate_gte?: InputMaybe<Scalars["BigInt"]>;
    totalNetFlowRate_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalNetFlowRate_lt?: InputMaybe<Scalars["BigInt"]>;
    totalNetFlowRate_lte?: InputMaybe<Scalars["BigInt"]>;
    totalNetFlowRate_not?: InputMaybe<Scalars["BigInt"]>;
    totalNetFlowRate_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalNumberOfActiveStreams?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveStreams_gt?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveStreams_gte?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveStreams_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalNumberOfActiveStreams_lt?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveStreams_lte?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveStreams_not?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveStreams_not_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalNumberOfClosedStreams?: InputMaybe<Scalars["Int"]>;
    totalNumberOfClosedStreams_gt?: InputMaybe<Scalars["Int"]>;
    totalNumberOfClosedStreams_gte?: InputMaybe<Scalars["Int"]>;
    totalNumberOfClosedStreams_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalNumberOfClosedStreams_lt?: InputMaybe<Scalars["Int"]>;
    totalNumberOfClosedStreams_lte?: InputMaybe<Scalars["Int"]>;
    totalNumberOfClosedStreams_not?: InputMaybe<Scalars["Int"]>;
    totalNumberOfClosedStreams_not_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalOutflowRate?: InputMaybe<Scalars["BigInt"]>;
    totalOutflowRate_gt?: InputMaybe<Scalars["BigInt"]>;
    totalOutflowRate_gte?: InputMaybe<Scalars["BigInt"]>;
    totalOutflowRate_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalOutflowRate_lt?: InputMaybe<Scalars["BigInt"]>;
    totalOutflowRate_lte?: InputMaybe<Scalars["BigInt"]>;
    totalOutflowRate_not?: InputMaybe<Scalars["BigInt"]>;
    totalOutflowRate_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalSubscriptionsWithUnits?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_gt?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_gte?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalSubscriptionsWithUnits_lt?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_lte?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_not?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_not_in?: InputMaybe<Array<Scalars["Int"]>>;
    updatedAtBlockNumber?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_gt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_gte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtBlockNumber_lt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_lte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_not?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtTimestamp?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_gt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_gte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtTimestamp_lt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_lte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_not?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
}

export class AccountTokenSnapshotQueryHandler extends SubgraphQueryHandler<
    AccountTokenSnapshot,
    AccountTokenSnapshotListQueryFilter,
    AccountTokenSnapshot_OrderBy,
    AccountTokenSnapshotsQuery,
    AccountTokenSnapshot_Filter,
    AccountTokenSnapshotsQueryVariables
> {
    convertToSubgraphFilter(
        filter: AccountTokenSnapshotListQueryFilter
    ): AccountTokenSnapshot_Filter {
        return filter;
    }

    mapFromSubgraphResponse(
        response: AccountTokenSnapshotsQuery
    ): AccountTokenSnapshot[] {
        return response.accountTokenSnapshots.map((x) => ({
            ...x,
            account: x.account.id,
            token: x.token.id,
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
        }));
    }

    requestDocument = AccountTokenSnapshotsDocument;
}
