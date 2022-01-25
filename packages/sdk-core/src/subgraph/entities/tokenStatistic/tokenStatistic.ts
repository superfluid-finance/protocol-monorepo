import {
    BigNumber,
    BlockNumber,
    SubgraphId,
    Timestamp,
} from "../../mappedSubgraphTypes";
import {
    TokenStatistic_Filter,
    TokenStatistic_OrderBy,
} from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    TokenStatisticsDocument,
    TokenStatisticsQuery,
    TokenStatisticsQueryVariables,
} from "./tokenStatistics.generated";

export interface TokenStatistic {
    id: SubgraphId;
    updatedAtTimestamp: Timestamp;
    updatedAtBlockNumber: BlockNumber;
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
    TokenStatistic_Filter,
    TokenStatistic_OrderBy
>;

export class TokenStatisticQueryHandler extends SubgraphQueryHandler<
    TokenStatistic,
    TokenStatisticListQuery,
    TokenStatisticsQuery,
    TokenStatisticsQueryVariables
> {
    protected getRelevantAddressesFromFilterCore = (
        filter: TokenStatistic_Filter
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
