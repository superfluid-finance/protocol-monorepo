import {
    BigNumber,
    RelevantAddressesIntermediate,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
    UpdatedAt,
} from "../../../queryV2";
import {
    InputMaybe,
    Scalars,
    TokenStatistic_Filter,
    TokenStatistic_OrderBy,
} from "../../schema.generated";

import {
    TokenStatisticsDocument,
    TokenStatisticsQuery,
    TokenStatisticsQueryVariables,
} from "./tokenStatistics.generated";

export interface TokenStatistic extends UpdatedAt {
    id: SubgraphId;
    totalAmountDistributedUntilUpdatedAt: BigNumber;
    totalAmountStreamedUntilUpdatedAt: BigNumber;
    totalAmountTransferredUntilUpdatedAt: BigNumber;
    totalApprovedSubscriptions: number;
    totalNumberOfActiveIndexes: number;
    totalNumberOfActiveStreams: number;
    totalNumberOfIndexes: number;
    totalNumberOfClosedStreams: number;
    totalOutflowRate: BigNumber;
    totalSubscriptionsWithUnits: number;
    totalSupply: BigNumber;
}

export type TokenStatisticListQuery = SubgraphListQuery<
    TokenStatisticsListQueryFilter,
    TokenStatistic_OrderBy
>;

export interface TokenStatisticsListQueryFilter {
    token?: InputMaybe<Scalars["String"]>;
    token_in?: InputMaybe<Array<Scalars["String"]>>;
    token_not_in?: InputMaybe<Array<Scalars["String"]>>;
    totalAmountDistributedUntilUpdatedAt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_gt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_gte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_in?: InputMaybe<
        Array<Scalars["BigInt"]>
    >;
    totalAmountDistributedUntilUpdatedAt_lt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_lte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_not?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_not_in?: InputMaybe<
        Array<Scalars["BigInt"]>
    >;
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
    totalNumberOfActiveIndexes?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveIndexes_gt?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveIndexes_gte?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveIndexes_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalNumberOfActiveIndexes_lt?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveIndexes_lte?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveIndexes_not?: InputMaybe<Scalars["Int"]>;
    totalNumberOfActiveIndexes_not_in?: InputMaybe<Array<Scalars["Int"]>>;
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
    totalNumberOfIndexes?: InputMaybe<Scalars["Int"]>;
    totalNumberOfIndexes_gt?: InputMaybe<Scalars["Int"]>;
    totalNumberOfIndexes_gte?: InputMaybe<Scalars["Int"]>;
    totalNumberOfIndexes_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalNumberOfIndexes_lt?: InputMaybe<Scalars["Int"]>;
    totalNumberOfIndexes_lte?: InputMaybe<Scalars["Int"]>;
    totalNumberOfIndexes_not?: InputMaybe<Scalars["Int"]>;
    totalNumberOfIndexes_not_in?: InputMaybe<Array<Scalars["Int"]>>;
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
    totalSupply?: InputMaybe<Scalars["BigInt"]>;
    totalSupply_gt?: InputMaybe<Scalars["BigInt"]>;
    totalSupply_gte?: InputMaybe<Scalars["BigInt"]>;
    totalSupply_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalSupply_lt?: InputMaybe<Scalars["BigInt"]>;
    totalSupply_lte?: InputMaybe<Scalars["BigInt"]>;
    totalSupply_not?: InputMaybe<Scalars["BigInt"]>;
    totalSupply_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
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

export class TokenStatisticQueryHandler extends SubgraphQueryHandler<
    TokenStatistic,
    TokenStatisticListQuery,
    TokenStatisticsQuery,
    TokenStatistic_Filter,
    TokenStatisticsQueryVariables
> {
    convertToSubgraphFilter = (
        filter: TokenStatisticsListQueryFilter
    ): TokenStatistic_Filter => filter;

    protected getRelevantAddressesFromFilterCore = (
        filter: TokenStatisticListQuery["filter"]
    ): RelevantAddressesIntermediate => ({
        tokens: [filter.token, filter.token_in, filter.token_not_in],
        accounts: [],
    });

    protected getRelevantAddressesFromResultCore = (
        result: TokenStatistic
    ): RelevantAddressesIntermediate => ({
        tokens: [result.id],
        accounts: [],
    });

    mapFromSubgraphResponse = (
        response: TokenStatisticsQuery
    ): TokenStatistic[] =>
        response.tokenStatistics.map((x) => ({
            ...x,
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
        }));

    requestDocument = TokenStatisticsDocument;
}
