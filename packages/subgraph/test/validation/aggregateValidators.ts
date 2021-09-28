import { expect } from "chai";
import { ethers } from "ethers";
import { SuperToken } from "../../typechain/SuperToken";
import { FlowActionType } from "../helpers/constants";
import { getTotalAmountStreamed, subgraphRequest } from "../helpers/helpers";
import {
    IAccountTokenSnapshot,
    IExpectedATSData,
    IExpectedTokenStats,
    ITokenStatistic,
} from "../interfaces";
import {
    getAccountTokenSnapshot,
    getTokenStatistic,
} from "../queries/aggregateQueries";

export const getATSDataForFlowUpdated = async (
    atsId: string,
    superToken: SuperToken,
    currentATSData: IAccountTokenSnapshot,
    actionType: FlowActionType,
    flowRateDelta: number,
    isSender: boolean
) => {
    const atsVars = {
        id: atsId,
    };
    const [accountId, tokenId] = atsId.split("-");
    const { accountTokenSnapshot: graphATS } = await subgraphRequest<{
        accountTokenSnapshot: IAccountTokenSnapshot | undefined;
    }>(getAccountTokenSnapshot, atsVars);

    if (!graphATS) {
        throw new Error("ATS entity not found.");
    }

    const activeStreamsDelta = getActiveStreamsDelta(actionType);
    const closedStreamsDelta = getClosedStreamsDelta(actionType);
    const balance = await superToken.balanceOf(
        ethers.utils.getAddress(accountId)
    );
    const expectedTotalAmountStreamedUntilUpdatedAt =
        Number(currentATSData.totalAmountStreamedUntilUpdatedAt) +
        (Number(graphATS.updatedAtTimestamp) -
            Number(currentATSData.updatedAtTimestamp)) *
            Number(currentATSData.totalOutflowRate);

    const expectedATS = {
        id: atsVars.id,
        balanceUntilUpdatedAt: balance.toString(),
        updatedAtBlock: graphATS.updatedAtBlock,
        updatedAtTimestamp: graphATS.updatedAtTimestamp,
        totalNumberOfActiveStreams:
            currentATSData.totalNumberOfActiveStreams + activeStreamsDelta,
        totalNumberOfClosedStreams:
            currentATSData.totalNumberOfClosedStreams + closedStreamsDelta,
        totalNetFlowRate: isSender
            ? (
                  Number(currentATSData.totalNetFlowRate) - flowRateDelta
              ).toString()
            : (
                  Number(currentATSData.totalNetFlowRate) + flowRateDelta
              ).toString(),
        totalInflowRate: isSender
            ? currentATSData.totalInflowRate
            : (
                  Number(currentATSData.totalInflowRate) + flowRateDelta
              ).toString(),
        totalOutflowRate: isSender
            ? (
                  Number(currentATSData.totalOutflowRate) + flowRateDelta
              ).toString()
            : currentATSData.totalOutflowRate,
        totalAmountStreamedUntilUpdatedAt: isSender
            ? expectedTotalAmountStreamedUntilUpdatedAt.toString()
            : currentATSData.totalAmountStreamedUntilUpdatedAt,
        account: { id: accountId },
        token: { id: tokenId },
    };

    return {
        graphATS,
        expectedATS,
        updatedATS: { ...currentATSData, ...expectedATS },
    };
};

export const getTokenStatsData = async (
    tokenId: string,
    currentTokenStats: ITokenStatistic,
    actionType: FlowActionType,
    flowRateDelta: number,
    atsData: { [id: string]: IAccountTokenSnapshot }
) => {
    const { tokenStatistic: graphTokenStats } = await subgraphRequest<{
        tokenStatistic: ITokenStatistic | undefined;
    }>(getTokenStatistic, { id: tokenId });

    if (!graphTokenStats) {
        throw new Error("TokenStats entity not found.");
    }

    const activeStreamsDelta = getActiveStreamsDelta(actionType);
    const closedStreamsDelta = getClosedStreamsDelta(actionType);

    const expectedAmountStreamedSinceLastUpdate = Object.values(atsData)
        .map((x) =>
            getTotalAmountStreamed(
                x.totalAmountStreamedUntilUpdatedAt,
                graphTokenStats.updatedAtTimestamp,
                x.updatedAtTimestamp,
                x.totalOutflowRate
            )
        )
        .reduce((x, y) => x + y, 0);

    const expectedTokenStats = {
        id: tokenId,
        updatedAtBlock: graphTokenStats.updatedAtBlock,
        updatedAtTimestamp: graphTokenStats.updatedAtTimestamp,
        totalNumberOfActiveStreams:
            currentTokenStats.totalNumberOfActiveStreams + activeStreamsDelta,
        totalNumberOfClosedStreams:
            currentTokenStats.totalNumberOfClosedStreams + closedStreamsDelta,
        totalOutflowRate: (
            Number(currentTokenStats.totalOutflowRate) + flowRateDelta
        ).toString(),
        totalAmountStreamedUntilUpdatedAt:
            expectedAmountStreamedSinceLastUpdate.toString(),
        token: { id: tokenId },
    };

    return {
        graphTokenStats,
        expectedTokenStats,
        updatedTokenStats: { ...currentTokenStats, ...expectedTokenStats },
    };
};

const getActiveStreamsDelta = (actionType: FlowActionType) =>
    actionType === FlowActionType.Create
        ? 1
        : actionType === FlowActionType.Update
        ? 0
        : -1;

const getClosedStreamsDelta = (actionType: FlowActionType) =>
    actionType === FlowActionType.Delete ? 1 : 0;

/**
 * Validates the ATS entity when a flow is updated.
 * @param graphATSData
 * @param expectedATSData
 */
export const validateATSEntityForFlowUpdated = (
    graphATSData: IAccountTokenSnapshot,
    expectedATSData: IExpectedATSData
) => {
    const {
        balanceUntilUpdatedAt,
        totalAmountStreamedUntilUpdatedAt: expectedATSStreamedUntilAt,
        totalNumberOfActiveStreams,
        totalNumberOfClosedStreams,
        totalInflowRate,
        totalOutflowRate,
        totalNetFlowRate,
    } = expectedATSData;

    expect(
        graphATSData.balanceUntilUpdatedAt,
        "ATS: graphATSData.balanceUntilUpdatedAt"
    ).to.equal(balanceUntilUpdatedAt);
    expect(
        graphATSData.totalNumberOfActiveStreams,
        "ATS: totalNumberOfActiveStreams error"
    ).to.equal(totalNumberOfActiveStreams);
    expect(
        graphATSData.totalNumberOfClosedStreams,
        "ATS: totalNumberOfClosedStreams error"
    ).to.equal(totalNumberOfClosedStreams);
    expect(graphATSData.totalInflowRate, "ATS: totalInflowRate error").to.equal(
        totalInflowRate
    );
    expect(
        graphATSData.totalOutflowRate,
        "ATS: totalOutflowRate error"
    ).to.equal(totalOutflowRate);
    expect(
        graphATSData.totalNetFlowRate,
        "ATS: totalNetFlowRate error"
    ).to.equal(totalNetFlowRate);
    expect(
        graphATSData.totalAmountStreamedUntilUpdatedAt,
        "ATS: totalAmountStreamedUntilUpdatedAt error"
    ).to.equal(expectedATSStreamedUntilAt);
};

// CFA TokenStats Validators
export const validateTokenStatsEntityForFlowUpdated = (
    graphTokenStats: ITokenStatistic,
    expectedTokenStats: IExpectedTokenStats
) => {
    const {
        totalAmountStreamedUntilUpdatedAt:
            expectedTotalAmountStreamedUntilUpdatedAt,
        totalNumberOfActiveStreams,
        totalNumberOfClosedStreams,
        totalOutflowRate,
    } = expectedTokenStats;

    const expectedTokenStatsStreamedUntilAt = Number(
        expectedTotalAmountStreamedUntilUpdatedAt
    ).toString();

    expect(
        graphTokenStats.totalNumberOfActiveStreams,
        "TokenStats: totalNumberOfActiveStreams error"
    ).to.equal(totalNumberOfActiveStreams);
    expect(
        graphTokenStats.totalNumberOfClosedStreams,
        "TokenStats: totalNumberOfClosedStreams error"
    ).to.equal(totalNumberOfClosedStreams);
    expect(
        graphTokenStats.totalOutflowRate,
        "TokenStats: totalOutflowRate error"
    ).to.equal(totalOutflowRate);
    expect(
        graphTokenStats.totalAmountStreamedUntilUpdatedAt,
        "TokenStats: totalAmountStreamedUntilUpdatedAt error"
    ).to.equal(expectedTokenStatsStreamedUntilAt);
};

// IDA AccountTokenSnapshot Validators

// IDA TokenStats Validators
