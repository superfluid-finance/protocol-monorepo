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
    validateATSEntityForFlowUpdated(graphATS, expectedATSData);
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

    validateTokenStatsEntityForFlowUpdated(
        graphTokenStats,
        expectedTokenStatsData
    );
};

/**
 * Validates the ATS entity when a flow is updated.
 * @param graphATSData
 * @param expectedATSData
 */
export const validateATSEntityForFlowUpdated = (
    graphATSData: IAccountTokenSnapshot,
    expectedATSData: IAccountTokenSnapshot
) => {
    const {
        balanceUntilUpdatedAt: expectedBalanceUntilUpdatedAt,
        totalAmountStreamedUntilUpdatedAt: expectedATSStreamedUntilAt,
        totalNumberOfActiveStreams: expectedTotalNumberOfActiveStreams,
        totalNumberOfClosedStreams: expectedTotalNumberOfClosedStreams,
        totalInflowRate: expectedTotalInflowRate,
        totalOutflowRate: expectedTotalOutflowRate,
        totalNetFlowRate: expectedTotalNetFlowRate,
    } = expectedATSData;

    expect(
        graphATSData.balanceUntilUpdatedAt,
        "ATS: graphATSData.balanceUntilUpdatedAt"
    ).to.equal(expectedBalanceUntilUpdatedAt);
    expect(
        graphATSData.totalNumberOfActiveStreams,
        "ATS: totalNumberOfActiveStreams error"
    ).to.equal(expectedTotalNumberOfActiveStreams);
    expect(
        graphATSData.totalNumberOfClosedStreams,
        "ATS: totalNumberOfClosedStreams error"
    ).to.equal(expectedTotalNumberOfClosedStreams);
    expect(graphATSData.totalInflowRate, "ATS: totalInflowRate error").to.equal(
        expectedTotalInflowRate
    );
    expect(
        graphATSData.totalOutflowRate,
        "ATS: totalOutflowRate error"
    ).to.equal(expectedTotalOutflowRate);
    expect(
        graphATSData.totalNetFlowRate,
        "ATS: totalNetFlowRate error"
    ).to.equal(expectedTotalNetFlowRate);
    expect(
        graphATSData.totalAmountStreamedUntilUpdatedAt,
        "ATS: totalAmountStreamedUntilUpdatedAt error"
    ).to.equal(expectedATSStreamedUntilAt);
};

// CFA TokenStats Validators
export const validateTokenStatsEntityForFlowUpdated = (
    graphTokenStats: ITokenStatistic,
    expectedTokenStats: ITokenStatistic
) => {
    const {
        totalAmountStreamedUntilUpdatedAt:
            expectedTotalAmountStreamedUntilUpdatedAt,
        totalNumberOfActiveStreams: expectedTotalNumberOfActiveStreams,
        totalNumberOfClosedStreams: expectedTotalNumberOfClosedStreams,
        totalOutflowRate: expectedTotalOutflowRate,
    } = expectedTokenStats;

    expect(
        graphTokenStats.totalNumberOfActiveStreams,
        "TokenStats: totalNumberOfActiveStreams error"
    ).to.equal(expectedTotalNumberOfActiveStreams);
    expect(
        graphTokenStats.totalNumberOfClosedStreams,
        "TokenStats: totalNumberOfClosedStreams error"
    ).to.equal(expectedTotalNumberOfClosedStreams);
    expect(
        graphTokenStats.totalOutflowRate,
        "TokenStats: totalOutflowRate error"
    ).to.equal(expectedTotalOutflowRate);
    expect(
        graphTokenStats.totalAmountStreamedUntilUpdatedAt,
        "TokenStats: totalAmountStreamedUntilUpdatedAt error"
    ).to.equal(expectedTotalAmountStreamedUntilUpdatedAt);
};

// IDA AccountTokenSnapshot Validators

// IDA TokenStats Validators
