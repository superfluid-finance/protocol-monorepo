import { expect } from "chai";
import { FlowActionType } from "../helpers/constants";
import { subgraphRequest } from "../helpers/helpers";
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
    currentATSData: IAccountTokenSnapshot,
    actionType: FlowActionType,
    flowRateDelta: number,
    amountStreamedSinceLastUpdate: number,
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

    const expectedATS = {
        id: atsVars.id,
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
            ? (
                  Number(currentATSData.totalAmountStreamedUntilUpdatedAt) +
                  amountStreamedSinceLastUpdate
              ).toString()
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
    amountStreamedSinceLastUpdate: number
) => {
    const { tokenStatistic: graphTokenStats } = await subgraphRequest<{
        tokenStatistic: ITokenStatistic | undefined;
    }>(getTokenStatistic, { id: tokenId });

    if (!graphTokenStats) {
        throw new Error("TokenStats entity not found.");
    }

    const activeStreamsDelta = getActiveStreamsDelta(actionType);
    const closedStreamsDelta = getClosedStreamsDelta(actionType);

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
        totalAmountStreamedUntilUpdatedAt: (
            Number(currentTokenStats.totalAmountStreamedUntilUpdatedAt) +
            amountStreamedSinceLastUpdate
        ).toString(),
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
 * @param flowedAmountSinceUpdatedAt
 * @param expectedATSData
 */
export const validateATSEntityForFlowUpdated = (
    graphATSData: IAccountTokenSnapshot,
    flowedAmountSinceUpdatedAt: number,
    expectedATSData: IExpectedATSData
) => {
    const {
        totalAmountStreamedUntilUpdatedAt,
        totalNumberOfActiveStreams,
        totalNumberOfClosedStreams,
        totalInflowRate,
        totalOutflowRate,
        totalNetFlowRate,
    } = expectedATSData;

    const expectedATSStreamedUntilAt = (
        Number(totalAmountStreamedUntilUpdatedAt) + flowedAmountSinceUpdatedAt
    ).toString();
    expect(graphATSData.totalNumberOfActiveStreams).to.equal(
        totalNumberOfActiveStreams
    );
    expect(graphATSData.totalNumberOfClosedStreams).to.equal(
        totalNumberOfClosedStreams
    );
    expect(graphATSData.totalInflowRate).to.equal(totalInflowRate);
    expect(graphATSData.totalOutflowRate).to.equal(totalOutflowRate);
    expect(graphATSData.totalNetFlowRate).to.equal(totalNetFlowRate);
    expect(graphATSData.totalAmountStreamedUntilUpdatedAt).to.equal(
        expectedATSStreamedUntilAt
    );
};

// CFA TokenStats Validators
export const validateTokenStatsEntityForFlowUpdated = (
    graphTokenStats: ITokenStatistic,
    flowedAmountSinceUpdatedAt: number,
    expectedTokenStats: IExpectedTokenStats
) => {
    const {
        totalAmountStreamedUntilUpdatedAt,
        totalNumberOfActiveStreams,
        totalNumberOfClosedStreams,
        totalOutflowRate,
    } = expectedTokenStats;

    const expectedTokenStatsStreamedUntilAt = (
        Number(totalAmountStreamedUntilUpdatedAt) + flowedAmountSinceUpdatedAt
    ).toString();

    expect(graphTokenStats.totalNumberOfActiveStreams).to.equal(
        totalNumberOfActiveStreams
    );
    expect(graphTokenStats.totalNumberOfClosedStreams).to.equal(
        totalNumberOfClosedStreams
    );
    expect(graphTokenStats.totalOutflowRate).to.equal(totalOutflowRate);
    expect(graphTokenStats.totalAmountStreamedUntilUpdatedAt).to.equal(
        expectedTokenStatsStreamedUntilAt
    );
};

// IDA AccountTokenSnapshot Validators

// IDA TokenStats Validators
