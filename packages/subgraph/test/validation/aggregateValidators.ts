import { expect } from "chai";
import { subgraphRequest } from "../helpers/helpers";
import { IAccountTokenSnapshot, ITokenStatistic } from "../interfaces";
import {
    getAccountTokenSnapshot,
    getTokenStatistic,
} from "../queries/aggregateQueries";

export const fetchATSAndValidate = async (
    atsId: string,
    expectedATSData: IAccountTokenSnapshot
) => {
    const atsVars = {
        id: atsId,
    };
    const { accountTokenSnapshot: graphATS } = await subgraphRequest<{
        accountTokenSnapshot: IAccountTokenSnapshot | undefined;
    }>(getAccountTokenSnapshot, atsVars);

    if (!graphATS) {
        throw new Error("ATS entity not found.");
    }
    validateATSEntity(graphATS, expectedATSData);
};

export const fetchTokenStatsAndValidate = async (
    tokenId: string,
    expectedTokenStatsData: ITokenStatistic
) => {
    const { tokenStatistic: graphTokenStats } = await subgraphRequest<{
        tokenStatistic: ITokenStatistic | undefined;
    }>(getTokenStatistic, { id: tokenId });

    if (!graphTokenStats) {
        throw new Error("TokenStats entity not found.");
    }

    validateTokenStatsEntity(graphTokenStats, expectedTokenStatsData);
};

/**
 * Validates the AccountTokenSnapshot entity.
 * @param graphATSData
 * @param expectedATSData
 */
export const validateATSEntity = (
    graphATSData: IAccountTokenSnapshot,
    expectedATSData: IAccountTokenSnapshot
) => {
    const {
        totalNumberOfActiveStreams: expectedTotalNumberOfActiveStreams,
        totalNumberOfClosedStreams: expectedTotalNumberOfClosedStreams,
        totalSubscriptions: expectedTotalSubscriptions,
        totalApprovedSubscriptions: expectedTotalApprovedSubscriptions,
        balanceUntilUpdatedAt: expectedBalanceUntilUpdatedAt,
        totalNetFlowRate: expectedTotalNetFlowRate,
        totalInflowRate: expectedTotalInflowRate,
        totalOutflowRate: expectedTotalOutflowRate,
        totalAmountStreamedUntilUpdatedAt:
            expectedTotalAmountStreamedUntilUpdatedAt,
        totalAmountTransferredUntilUpdatedAt:
            expectedTotalAmountTransferredUntilUpdatedAt,
    } = expectedATSData;

    expect(
        graphATSData.totalNumberOfActiveStreams,
        "ATS: totalNumberOfActiveStreams error"
    ).to.equal(expectedTotalNumberOfActiveStreams);
    expect(
        graphATSData.totalNumberOfClosedStreams,
        "ATS: totalNumberOfClosedStreams error"
    ).to.equal(expectedTotalNumberOfClosedStreams);
    expect(
        graphATSData.totalSubscriptions,
        "ATS: totalSubscriptions error"
    ).to.equal(expectedTotalSubscriptions);
    expect(
        graphATSData.totalApprovedSubscriptions,
        "ATS: totalApprovedSubscriptions error"
    ).to.equal(expectedTotalApprovedSubscriptions);
    expect(
        graphATSData.balanceUntilUpdatedAt,
        "ATS: balanceUntilUpdatedAt error"
    ).to.equal(expectedBalanceUntilUpdatedAt);
    expect(
        graphATSData.totalNetFlowRate,
        "ATS: totalNetFlowRate error"
    ).to.equal(expectedTotalNetFlowRate);
    expect(graphATSData.totalInflowRate, "ATS: totalInflowRate error").to.equal(
        expectedTotalInflowRate
    );
    expect(
        graphATSData.totalOutflowRate,
        "ATS: totalOutflowRate error"
    ).to.equal(expectedTotalOutflowRate);
    expect(
        graphATSData.totalAmountStreamedUntilUpdatedAt,
        "ATS: totalAmountStreamedUntilUpdatedAt error"
    ).to.equal(expectedTotalAmountStreamedUntilUpdatedAt);
    expect(
        graphATSData.totalAmountTransferredUntilUpdatedAt,
        "ATS: totalAmountTransferredUntilUpdatedAt error"
    ).to.equal(expectedTotalAmountTransferredUntilUpdatedAt);
};

/**
 * Validates the TokenStatistic entity.
 * @param graphTokenStats
 * @param expectedTokenStats
 */
export const validateTokenStatsEntity = (
    graphTokenStats: ITokenStatistic,
    expectedTokenStats: ITokenStatistic
) => {
    const {
        totalNumberOfActiveStreams: expectedTotalNumberOfActiveStreams,
        totalNumberOfClosedStreams: expectedTotalNumberOfClosedStreams,
        totalNumberOfIndexes: expectedTotalNumberOfIndexes,
        totalNumberOfActiveIndexes: expectedTotalNumberOfActiveIndexes,
        totalSubscriptions: expectedTotalSubscriptions,
        totalApprovedSubscriptions: expectedTotalApprovedSubscriptions,
        totalOutflowRate: expectedTotalOutflowRate,
        totalAmountStreamedUntilUpdatedAt:
            expectedTotalAmountStreamedUntilUpdatedAt,
        totalAmountTransferredUntilUpdatedAt:
            expectedTotalAmountTransferredUntilUpdatedAt,
        totalAmountDistributedUntilUpdatedAt:
            expectedTotalAmountDistributedUntilUpdatedAt,
        totalSupply: expectedTotalSupply,
    } = expectedTokenStats;

    expect(
        graphTokenStats.totalNumberOfActiveStreams,
        "TokenStats: totalNumberOfActiveStreams error"
    ).to.equal(expectedTotalNumberOfActiveStreams);
    expect(
        graphTokenStats.totalNumberOfClosedStreams,
        "TokenStats: totalNumberOfClosedStreams error"
    ).to.equal(expectedTotalNumberOfClosedStreams);
    expect(graphTokenStats.totalNumberOfIndexes).to.equal(
        expectedTotalNumberOfIndexes
    );
    expect(
        graphTokenStats.totalNumberOfActiveIndexes,
        "totalNumberOfActiveIndexes error"
    ).to.equal(expectedTotalNumberOfActiveIndexes);
    expect(
        graphTokenStats.totalSubscriptions,
        "totalSubscriptions error"
    ).to.equal(expectedTotalSubscriptions);
    expect(
        graphTokenStats.totalApprovedSubscriptions,
        "totalApprovedSubscriptions error"
    ).to.equal(expectedTotalApprovedSubscriptions);
    expect(
        graphTokenStats.totalOutflowRate,
        "TokenStats: totalOutflowRate error"
    ).to.equal(expectedTotalOutflowRate);
    expect(
        graphTokenStats.totalAmountStreamedUntilUpdatedAt,
        "TokenStats: totalAmountStreamedUntilUpdatedAt error"
    ).to.equal(expectedTotalAmountStreamedUntilUpdatedAt);
    // expect(
    //     graphTokenStats.totalAmountTransferredUntilUpdatedAt,
    //     "totalAmountTransferredUntilUpdatedAt error"
    // ).to.equal(expectedTotalAmountTransferredUntilUpdatedAt);
    expect(
        graphTokenStats.totalAmountDistributedUntilUpdatedAt,
        "totalAmountDistributedUntilUpdatedAt error"
    ).to.equal(expectedTotalAmountDistributedUntilUpdatedAt);
    // TODO: handle the uprade/downgrade here
    // expect(graphTokenStats.totalSupply, "totalSupply error").to.equal(
    //     expectedTotalSupply
    // );
};
