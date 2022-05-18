import {expect} from "chai";
import {fetchEntityAndEnsureExistence} from "../helpers/helpers";
import {IAccountTokenSnapshot, IAccountTokenSnapshotLog, ITokenStatistic} from "../interfaces";
import {getAccountTokenSnapshot, getTokenStatistic,} from "../queries/aggregateQueries";

export const fetchATSAndValidate = async (
    expectedATSData: IAccountTokenSnapshot,
    skipLogEntryValidation: Boolean
) => {
    const graphATS = await fetchEntityAndEnsureExistence<IAccountTokenSnapshot>(
        getAccountTokenSnapshot,
        expectedATSData.id,
        "AccountTokenSnapshot"
    );
    validateATSEntity(graphATS, expectedATSData);
   if(!skipLogEntryValidation) validateASTEntityAndItsLogEntry(graphATS);
};

export const fetchTokenStatsAndValidate = async (
    expectedTokenStatsData: ITokenStatistic
) => {
    const graphTokenStats =
        await fetchEntityAndEnsureExistence<ITokenStatistic>(
            getTokenStatistic,
            expectedTokenStatsData.id,
            "TokenStats"
        );
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
        totalSubscriptionsWithUnits: expectedTotalSubscriptions,
        totalApprovedSubscriptions: expectedTotalApprovedSubscriptions,
        balanceUntilUpdatedAt: expectedBalanceUntilUpdatedAt,
        totalNetFlowRate: expectedTotalNetFlowRate,
        totalInflowRate: expectedTotalInflowRate,
        totalOutflowRate: expectedTotalOutflowRate,
        totalAmountStreamedUntilUpdatedAt:
            expectedTotalAmountStreamedUntilUpdatedAt,
        totalAmountTransferredUntilUpdatedAt:
            expectedTotalAmountTransferredUntilUpdatedAt,
        totalDeposit: expectedTotalDeposit,
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
        graphATSData.totalSubscriptionsWithUnits,
        "ATS: totalSubscriptionWithUnits error"
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
    expect(graphATSData.totalDeposit, "ATS: totalDeposit error").to.equal(
        expectedTotalDeposit
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

export const validateASTEntityAndItsLogEntry = (
    graphATSData: IAccountTokenSnapshot
) => {
    const accountTokenSnapshotLogs: IAccountTokenSnapshotLog[] = graphATSData.accountTokenSnapshotLogs;
    const accountTokenSnapshotLog: IAccountTokenSnapshotLog = accountTokenSnapshotLogs[0];
    expect(
        graphATSData.totalNumberOfActiveStreams,
        "ATS: totalNumberOfActiveStreams error"
    ).to.equal(accountTokenSnapshotLog.totalNumberOfActiveStreamsSoFar);
    expect(
        graphATSData.totalNumberOfClosedStreams,
        "ATS: totalNumberOfClosedStreams error"
    ).to.equal(accountTokenSnapshotLog.totalNumberOfClosedStreamsSoFar);
    expect(
        graphATSData.totalSubscriptionsWithUnits,
        "ATS: totalSubscriptionWithUnits error"
    ).to.equal(accountTokenSnapshotLog.totalSubscriptionsWithUnitsSoFar);
    expect(
        graphATSData.totalApprovedSubscriptions,
        "ATS: totalApprovedSubscriptions error"
    ).to.equal(accountTokenSnapshotLog.totalApprovedSubscriptionsSoFar);
    expect(
        graphATSData.balanceUntilUpdatedAt,
        "ATS: balanceUntilUpdatedAt error"
    ).to.equal(accountTokenSnapshotLog.balanceSoFar);
    expect(
        graphATSData.totalNetFlowRate,
        "ATS: totalNetFlowRate error"
    ).to.equal(accountTokenSnapshotLog.totalNetFlowRateSoFar);

    expect(graphATSData.totalInflowRate, "ATS: totalInflowRate error").to.equal(
        accountTokenSnapshotLog.totalInflowRateSoFar
    );
    expect(graphATSData.totalDeposit, "ATS: totalDeposit error").to.equal(
        accountTokenSnapshotLog.totalDepositSoFar
    );
    expect(
        graphATSData.totalOutflowRate,
        "ATS: totalOutflowRate error"
    ).to.equal(accountTokenSnapshotLog.totalOutflowRateSoFar);
    expect(
        graphATSData.totalAmountStreamedUntilUpdatedAt,
        "ATS: totalAmountStreamedUntilUpdatedAt error"
    ).to.equal(accountTokenSnapshotLog.totalAmountStreamedSoFar);
    expect(
        graphATSData.totalAmountTransferredUntilUpdatedAt,
        "ATS: totalAmountTransferredUntilUpdatedAt error"
    ).to.equal(accountTokenSnapshotLog.totalAmountTransferredSoFar);
    expect(
        graphATSData.updatedAtTimestamp,
        "ATS: updatedAtTimestamp error"
    ).to.equal(accountTokenSnapshotLog.timestamp);

    expect(
        graphATSData.updatedAtBlockNumber,
        "ATS: updatedAtBlockNumber error"
    ).to.equal(accountTokenSnapshotLog.blockNumber);
}

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
        totalSubscriptionsWithUnits: expectedTotalSubscriptions,
        totalApprovedSubscriptions: expectedTotalApprovedSubscriptions,
        totalOutflowRate: expectedTotalOutflowRate,
        totalAmountStreamedUntilUpdatedAt:
            expectedTotalAmountStreamedUntilUpdatedAt,
        totalAmountTransferredUntilUpdatedAt:
            expectedTotalAmountTransferredUntilUpdatedAt,
        totalAmountDistributedUntilUpdatedAt:
            expectedTotalAmountDistributedUntilUpdatedAt,
        totalSupply: expectedTotalSupply,
        totalDeposit: expectedTotalDeposit,
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
        graphTokenStats.totalSubscriptionsWithUnits,
        "totalSubscriptionWithUnits error"
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
    expect(
        graphTokenStats.totalAmountDistributedUntilUpdatedAt,
        "totalAmountDistributedUntilUpdatedAt error"
    ).to.equal(expectedTotalAmountDistributedUntilUpdatedAt);
    expect(graphTokenStats.totalDeposit, "totalDeposit error").to.equal(
        expectedTotalDeposit
    );
};
