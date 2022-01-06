import {
    BigNumber,
    RelevantAddressesIntermediate,
    SubgraphFilterOmitFieldList,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
    UpdatedAt,
} from "../../../queryV2";
import {
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

export type TokenStatisticOrderBy = TokenStatistic_OrderBy;

export type TokenStatisticListQuery = SubgraphListQuery<
    TokenStatisticsListQueryFilter,
    TokenStatisticOrderBy
>;

export type TokenStatisticsListQueryFilter = Omit<
    TokenStatistic_Filter,
    SubgraphFilterOmitFieldList
>;

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
        filter: TokenStatisticsListQueryFilter
    ): RelevantAddressesIntermediate => ({
        tokens: [
            filter.token,
            filter.token_in,
            filter.token_not,
            filter.token_not_in,
        ],
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
