/**************************************************************************
 * Entity Updaters (For expected values)
 *
 * This file contains all the updater functions for the objects initalized
 * in the intializers.ts file. This file contains the logic for changing
 * the data to what we expect it to be. The output of these functions
 * are what is used to compare against the data obtained from the graph.
 *************************************************************************/
import { BigNumber } from "@ethersproject/bignumber";
import { SuperToken } from "@superfluid-finance/sdk-core";
import {
    IAccountTokenSnapshot,
    IFlowUpdatedUpdateTestData,
    IGetExpectedIDADataParams,
    IIndex,
    IStreamData,
    IIndexSubscription,
    ITokenStatistic,
} from "../interfaces";
import {
    actionTypeToActiveStreamsDeltaMap,
    actionTypeToClosedStreamsDeltaMap,
    actionTypeToPeriodRevisionIndexDeltaMap,
    FlowActionType,
} from "./constants";
import { getCurrentTotalAmountStreamed, toBN } from "./helpers";
import { BaseProvider } from "@ethersproject/providers";

export const getExpectedStreamData = (
    currentStreamData: IStreamData,
    actionType: FlowActionType,
    oldFlowRate: string,
    lastUpdatedAtTimestamp: string,
    streamedAmountSinceUpdatedAt: BigNumber
) => {
    const {
        revisionIndex: currentRevisionIndex,
        periodRevisionIndex: currentPeriodRevisionIndex,
    } = currentStreamData;
    const revisionIndex = getUpdatedIndex(
        currentRevisionIndex,
        actionTypeToClosedStreamsDeltaMap,
        actionType
    );
    const periodRevisionIndex = getUpdatedIndex(
        currentPeriodRevisionIndex,
        actionTypeToPeriodRevisionIndexDeltaMap,
        actionType
    );
    const updatedStreamedUntilUpdatedAt = toBN(
        currentStreamData.streamedUntilUpdatedAt
    ).add(streamedAmountSinceUpdatedAt);
    return {
        ...currentStreamData,
        revisionIndex,
        periodRevisionIndex,
        oldFlowRate,
        streamedUntilUpdatedAt: updatedStreamedUntilUpdatedAt.toString(),
        updatedAtTimestamp: lastUpdatedAtTimestamp,
    } as IStreamData;
};

const getUpdatedIndex = (
    currentIndex: string,
    actionToIndexDeltaMap: Map<FlowActionType, number>,
    actionType: FlowActionType
) => {
    const indexDelta = actionToIndexDeltaMap.get(actionType)!;
    return (Number(currentIndex) + indexDelta).toString();
};

/**
 * Updates AccountTokenSnapshot balance and stream data. Used on
 * any updates to the AccountTokenSnapshot entity.
 */
export const getExpectedATSForCFAEvent = async (
    superToken: SuperToken,
    currentATS: IAccountTokenSnapshot,
    updatedAtBlockNumber: string,
    updatedAtTimestamp: string,
    actionType: FlowActionType,
    isSender: boolean,
    flowRate: BigNumber,
    flowRateDelta: BigNumber,
    provider: BaseProvider
): Promise<IAccountTokenSnapshot> => {
    const balanceUntilUpdatedAt = (
        await superToken.balanceOf({
            account: currentATS.account.id,
            providerOrSigner: provider,
        })
    ).toString();

    // Force casting because they will never be undefined
    const activeStreamsDelta =
        actionTypeToActiveStreamsDeltaMap.get(actionType)!;
    const closedStreamsDelta =
        actionTypeToClosedStreamsDeltaMap.get(actionType)!;
    const totalNetFlowRate = isSender
        ? toBN(currentATS.totalNetFlowRate).sub(flowRateDelta).toString()
        : toBN(currentATS.totalNetFlowRate).add(flowRateDelta).toString();
    const inflowRate = toBN(currentATS.totalInflowRate)
        .add(flowRateDelta)
        .lt(toBN(0))
        ? flowRate
        : toBN(currentATS.totalInflowRate).add(flowRateDelta);
    const outflowRate = toBN(currentATS.totalOutflowRate)
        .add(flowRateDelta)
        .lt(toBN(0))
        ? flowRate
        : toBN(currentATS.totalOutflowRate).add(flowRateDelta);
    const totalInflowRate =
        isSender === true ? currentATS.totalInflowRate : inflowRate.toString();
    const totalOutflowRate =
        isSender === true
            ? outflowRate.toString()
            : currentATS.totalOutflowRate;
    const totalAmountStreamedUntilUpdatedAt = toBN(
        currentATS.totalAmountStreamedUntilUpdatedAt
    )
        .add(
            toBN(currentATS.totalOutflowRate).mul(
                toBN(updatedAtTimestamp).sub(
                    toBN(currentATS.updatedAtTimestamp)
                )
            )
        )
        .toString();
    return {
        ...currentATS,
        updatedAtBlockNumber,
        updatedAtTimestamp,
        totalNumberOfActiveStreams:
            currentATS.totalNumberOfActiveStreams + activeStreamsDelta,
        totalNumberOfClosedStreams:
            currentATS.totalNumberOfClosedStreams + closedStreamsDelta,
        balanceUntilUpdatedAt,
        totalNetFlowRate,
        totalInflowRate,
        totalOutflowRate,
        totalAmountStreamedUntilUpdatedAt,
    };
};

/**
 * Updates TokenStatistic stream data. Used on any updates
 * to the TokenStatistic entity.
 */
export const getExpectedTokenStatsForCFAEvent = (
    currentTokenStats: ITokenStatistic,
    accountTokenSnapshots: IAccountTokenSnapshot[],
    updatedAtBlockNumber: string,
    lastUpdatedAtTimestamp: string,
    actionType: FlowActionType,
    flowRate: BigNumber,
    flowRateDelta: BigNumber
) => {
    const activeStreamsDelta =
        actionTypeToActiveStreamsDeltaMap.get(actionType)!;
    const closedStreamsDelta =
        actionTypeToClosedStreamsDeltaMap.get(actionType)!;
    const outflowRate = toBN(currentTokenStats.totalOutflowRate)
        .add(flowRateDelta)
        .lt(toBN(0))
        ? flowRate
        : toBN(currentTokenStats.totalOutflowRate).add(flowRateDelta);
    const totalOutflowRate = outflowRate.toString();

    const totalAmountStreamedUntilUpdatedAt = toBN(
        currentTokenStats.totalAmountStreamedUntilUpdatedAt
    )
        .add(
            toBN(currentTokenStats.totalOutflowRate).mul(
                toBN(lastUpdatedAtTimestamp).sub(
                    toBN(currentTokenStats.updatedAtTimestamp)
                )
            )
        )
        .toString();

    const atsSum = accountTokenSnapshots
        .map((x) =>
            getCurrentTotalAmountStreamed(
                x.totalAmountStreamedUntilUpdatedAt,
                lastUpdatedAtTimestamp,
                x.updatedAtTimestamp,
                x.totalOutflowRate
            )
        )
        .reduce((a, b) => a.add(b), toBN(0));
    return {
        ...currentTokenStats,
        updatedAtBlockNumber,
        updatedAtTimestamp: lastUpdatedAtTimestamp,
        totalNumberOfActiveStreams:
            currentTokenStats.totalNumberOfActiveStreams + activeStreamsDelta,
        totalNumberOfClosedStreams:
            currentTokenStats.totalNumberOfClosedStreams + closedStreamsDelta,
        totalOutflowRate,
        totalAmountStreamedUntilUpdatedAt,
    } as ITokenStatistic;
};

/**
 * Gets all the expected data necessary for validating the subgraph data after the
 * FlowUpdated event.
 */
export const getExpectedDataForFlowUpdated = async (
    testData: IFlowUpdatedUpdateTestData
) => {
    const {
        actionType,
        accountTokenSnapshots,
        flowRate,
        lastUpdatedAtTimestamp,
        lastUpdatedBlockNumber,
        superToken,
        pastStreamData,
        currentSenderATS,
        currentReceiverATS,
        currentTokenStats,
        provider
    } = testData;
    // newFlowRate - previousFlowRate
    const flowRateDelta = flowRate.sub(toBN(pastStreamData.oldFlowRate));

    // Update the data - we use this for comparison
    const updatedSenderATS = await getExpectedATSForCFAEvent(
        superToken,
        currentSenderATS,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        actionType,
        true,
        flowRate,
        flowRateDelta,
        provider
    );
    const updatedReceiverATS = await getExpectedATSForCFAEvent(
        superToken,
        currentReceiverATS,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        actionType,
        false,
        flowRate,
        flowRateDelta,
        provider
    );
    const updatedTokenStats = getExpectedTokenStatsForCFAEvent(
        currentTokenStats,
        accountTokenSnapshots,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        actionType,
        flowRate,
        flowRateDelta
    );

    return { updatedSenderATS, updatedReceiverATS, updatedTokenStats };
};

export const getExpectedDataForIndexCreated = async (
    data: IGetExpectedIDADataParams
) => {
    const {
        atsArray,
        currentIndex,
        currentSubscription,
        currentSubscriberATS: currentSubscriberATS,
        currentPublisherATS,
        currentTokenStats,
        updatedAtBlockNumber,
        timestamp,
        token,
        provider
    } = data;

    const updatedPublisherATS: IAccountTokenSnapshot = {
        ...(await getExpectedATSForCFAEvent(
            token,
            currentPublisherATS,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0),
            provider
        )),
    };

    const updatedTokenStats: ITokenStatistic = {
        ...getExpectedTokenStatsForCFAEvent(
            currentTokenStats,
            atsArray,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            toBN(0),
            toBN(0)
        ),
        totalNumberOfIndexes: currentTokenStats.totalNumberOfIndexes + 1,
    };

    return {
        updatedIndex: currentIndex,
        updatedPublisherATS,
        updatedTokenStats,
        updatedSubscription: currentSubscription,
        updatedSubscriberATS: currentSubscriberATS,
    };
};

export const getExpectedDataForIndexUpdated = async (
    data: IGetExpectedIDADataParams,
    totalUnits: BigNumber,
    newIndexValue: BigNumber,
    indexTotalUnitsApproved: BigNumber,
    indexTotalUnitsPending: BigNumber
) => {
    const {
        token,
        atsArray,
        currentIndex,
        currentSubscription,
        currentSubscriberATS,
        currentPublisherATS,
        currentTokenStats,
        updatedAtBlockNumber,
        timestamp,
        provider
    } = data;

    const amountDistributedDelta = totalUnits.mul(
        newIndexValue.sub(toBN(currentIndex.indexValue))
    );

    const updatedIndex: IIndex = {
        ...currentIndex,
        indexValue: newIndexValue.toString(),
        totalUnitsApproved: indexTotalUnitsApproved.toString(),
        totalUnitsPending: indexTotalUnitsPending.toString(),
        totalUnits: totalUnits.toString(),
        totalAmountDistributedUntilUpdatedAt: toBN(
            currentIndex.totalAmountDistributedUntilUpdatedAt
        )
            .add(amountDistributedDelta)
            .toString(),
    };
    const updatedTokenStats: ITokenStatistic = {
        ...getExpectedTokenStatsForCFAEvent(
            currentTokenStats,
            atsArray,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            toBN(0),
            toBN(0)
        ),
        totalAmountDistributedUntilUpdatedAt: toBN(
            currentTokenStats.totalAmountDistributedUntilUpdatedAt
        )
            .add(amountDistributedDelta)
            .toString(),
        totalNumberOfActiveIndexes:
            currentIndex.totalAmountDistributedUntilUpdatedAt === "0"
                ? currentTokenStats.totalNumberOfActiveIndexes + 1
                : currentTokenStats.totalNumberOfActiveIndexes,
    };
    const updatedPublisherATS = await getExpectedATSForCFAEvent(
        token,
        currentPublisherATS,
        updatedAtBlockNumber,
        timestamp,
        FlowActionType.Update,
        true,
        toBN(0),
        toBN(0),
        provider
    );

    return {
        updatedIndex,
        updatedPublisherATS,
        updatedTokenStats,
        updatedSubscription: currentSubscription,
        updatedSubscriberATS: currentSubscriberATS,
    };
};

/**
 * Same expected data for IndexSubscribed
 * @param data
 * @returns
 */
export const getExpectedDataForSubscriptionApproved = async (
    data: IGetExpectedIDADataParams
) => {
    const {
        token,
        currentIndex,
        currentSubscription,
        atsArray,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlockNumber,
        timestamp,
        provider
    } = data;

    const subscriptionWithUnitsExists = currentSubscription.units !== "0";

    const balanceDelta = toBN(currentSubscription.units).mul(
        toBN(currentIndex.indexValue).sub(
            toBN(currentSubscription.indexValueUntilUpdatedAt)
        )
    );

    let updatedIndex = { ...currentIndex };
    let updatedSubscription: IIndexSubscription = {
        ...currentSubscription,
        approved: true,
        indexValueUntilUpdatedAt: currentIndex.indexValue,
    };
    let updatedPublisherATS: IAccountTokenSnapshot = {
        ...currentPublisherATS,
    };
    let updatedTokenStats: ITokenStatistic = {
        ...getExpectedTokenStatsForCFAEvent(
            currentTokenStats,
            atsArray,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            toBN(0),
            toBN(0)
        ),
        totalApprovedSubscriptions:
            currentTokenStats.totalApprovedSubscriptions + 1,
    };

    // this occurs whether subscription exists or not
    let updatedSubscriberATS: IAccountTokenSnapshot = {
        ...(await getExpectedATSForCFAEvent(
            token,
            currentSubscriberATS,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0),
            provider
        )),
        totalApprovedSubscriptions:
            currentSubscriberATS.totalApprovedSubscriptions + 1,
    };

    if (subscriptionWithUnitsExists === true) {
        // Update Index
        updatedIndex = {
            ...currentIndex,
            totalUnitsApproved: toBN(currentIndex.totalUnitsApproved)
                .add(toBN(currentSubscription.units))
                .toString(),
            totalUnitsPending: toBN(currentIndex.totalUnitsPending)
                .sub(toBN(currentSubscription.units))
                .toString(),
        };

        // Update Subscription
        updatedSubscription = {
            ...updatedSubscription,
            totalAmountReceivedUntilUpdatedAt: toBN(
                updatedSubscription.totalAmountReceivedUntilUpdatedAt
            )
                .add(balanceDelta)
                .toString(),
        };

        // Update Publisher ATS entity (stream data + balance)
        updatedPublisherATS = await getExpectedATSForCFAEvent(
            token,
            currentPublisherATS,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0),
            provider
        );
    }

    return {
        updatedIndex,
        updatedSubscription,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
};

/**
 * Same expected data for IndexDistributionClaimed
 * @param data 
 * @returns 
 */
export const getExpectedDataForSubscriptionDistributionClaimed = async (
    data: IGetExpectedIDADataParams
) => {
    const {
        token,
        currentIndex,
        currentSubscription,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlockNumber,
        timestamp,
        provider
    } = data;
    const distributionDelta = toBN(currentSubscription.units).mul(
        toBN(currentIndex.indexValue).sub(
            toBN(currentSubscription.indexValueUntilUpdatedAt)
        )
    );

    let updatedTokenStats: ITokenStatistic = {
        ...currentTokenStats,
    };

    let updatedIndex: IIndex = {
        ...currentIndex,
    };

    let updatedSubscription: IIndexSubscription = {
        ...currentSubscription,
        totalAmountReceivedUntilUpdatedAt: toBN(
            currentSubscription.totalAmountReceivedUntilUpdatedAt
        )
            .add(distributionDelta)
            .toString(),
        indexValueUntilUpdatedAt: updatedIndex.indexValue,
    };

    let updatedSubscriberATS: IAccountTokenSnapshot = {
        ...(await getExpectedATSForCFAEvent(
            token,
            currentSubscriberATS,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0),
            provider
        )),
    };

    let updatedPublisherATS: IAccountTokenSnapshot = {
        ...(await getExpectedATSForCFAEvent(
            token,
            currentPublisherATS,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0),
            provider
        )),
    };

    return {
        updatedIndex,
        updatedSubscription,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
};

/**
 * @dev Same expected data for IndexUnsubscribed
 * @param data
 * @param isRevoke
 * @returns
 */
export const getExpectedDataForRevokeOrDeleteSubscription = async (
    data: IGetExpectedIDADataParams,
    isRevoke: boolean
) => {
    const {
        token,
        currentIndex,
        currentSubscription,
        atsArray,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlockNumber,
        timestamp,
        provider
    } = data;
    const balanceDelta = toBN(currentSubscription.units).mul(
        toBN(currentIndex.indexValue).sub(
            toBN(currentSubscription.indexValueUntilUpdatedAt)
        )
    );

    let updatedIndex: IIndex = {
        ...currentIndex,
    };
    let updatedSubscription: IIndexSubscription = {
        ...currentSubscription,
        approved: false,
        totalAmountReceivedUntilUpdatedAt: toBN(
            currentSubscription.totalAmountReceivedUntilUpdatedAt
        )
            .add(balanceDelta)
            .toString(),
        indexValueUntilUpdatedAt: updatedIndex.indexValue,
    };
    let updatedPublisherATS: IAccountTokenSnapshot = {
        ...currentPublisherATS,
    };

    let updatedTokenStats: ITokenStatistic = {
        ...getExpectedTokenStatsForCFAEvent(
            currentTokenStats,
            atsArray,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            toBN(0),
            toBN(0)
        ),
    };

    // this occurs whether subscription exists or not
    let updatedSubscriberATS: IAccountTokenSnapshot = {
        ...(await getExpectedATSForCFAEvent(
            token,
            currentSubscriberATS,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0),
            provider
        )),
    };

    const subscriptionWithUnitsExists = currentSubscription.units !== "0";
    const subscriptionsWithUnitsDelta = subscriptionWithUnitsExists ? -1 : 0;
    const subscriptionsApprovedDelta = currentSubscription.approved ? -1 : 0;

    // handleRevokeOrDelete
    if (isRevoke) {
        updatedIndex = {
            ...updatedIndex,
            totalUnitsApproved: currentSubscription.approved
                ? toBN(updatedIndex.totalUnitsApproved)
                      .sub(currentSubscription.units)
                      .toString()
                : currentIndex.totalUnitsApproved,
            totalUnitsPending: currentSubscription.approved
                ? toBN(updatedIndex.totalUnitsPending)
                      .add(currentSubscription.units)
                      .toString()
                : currentIndex.totalUnitsPending,
        };
        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalApprovedSubscriptions:
                updatedSubscriberATS.totalApprovedSubscriptions - 1,
        };
        updatedTokenStats = {
            ...updatedTokenStats,
            totalApprovedSubscriptions:
                updatedTokenStats.totalApprovedSubscriptions - 1,
        };
    } else {
        // isDelete
        updatedIndex = {
            ...updatedIndex,
            totalUnits: toBN(updatedIndex.totalUnits)
                .sub(currentSubscription.units)
                .toString(),
            totalSubscriptionsWithUnits:
                currentIndex.totalSubscriptionsWithUnits +
                subscriptionsWithUnitsDelta,
        };
        if (currentSubscription.approved) {
            updatedIndex = {
                ...updatedIndex,
                totalUnitsApproved: toBN(updatedIndex.totalUnitsApproved)
                    .sub(currentSubscription.units)
                    .toString(),
            };
        } else {
            updatedIndex = {
                ...updatedIndex,
                totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
                    .sub(currentSubscription.units)
                    .toString(),
            };
        }
        updatedSubscription = { ...updatedSubscription, units: "0" };
        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalSubscriptionsWithUnits:
                updatedSubscriberATS.totalSubscriptionsWithUnits +
                subscriptionsWithUnitsDelta,
            totalApprovedSubscriptions:
                updatedSubscriberATS.totalApprovedSubscriptions +
                subscriptionsApprovedDelta,
        };
        updatedTokenStats = {
            ...updatedTokenStats,
            totalApprovedSubscriptions:
                updatedTokenStats.totalApprovedSubscriptions +
                subscriptionsApprovedDelta,
            totalSubscriptionsWithUnits:
                updatedTokenStats.totalSubscriptionsWithUnits +
                subscriptionsWithUnitsDelta,
        };
    }

    updatedPublisherATS = {
        ...(await getExpectedATSForCFAEvent(
            token,
            updatedPublisherATS,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0),
            provider
        )),
    };

    return {
        updatedIndex,
        updatedSubscription,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
};

export const getExpectedDataForSubscriptionUnitsUpdated = async (
    data: IGetExpectedIDADataParams,
    units: BigNumber
) => {
    const {
        token,
        currentIndex,
        currentSubscription,
        atsArray,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlockNumber,
        timestamp,
        provider
    } = data;
    let updatedIndex = { ...currentIndex };
    let updatedSubscription = { ...currentSubscription };
    let updatedPublisherATS = { ...currentPublisherATS };
    let updatedSubscriberATS = {
        ...currentSubscriberATS,
        ...(await getExpectedATSForCFAEvent(
            token,
            currentSubscriberATS,
            updatedAtBlockNumber,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0),
            provider
        )),
    };
    const subscriptionWithUnitsExists = currentSubscription.units !== "0";
    let updatedTokenStats = { ...currentTokenStats };

    const unitsDelta = units.sub(toBN(currentSubscription.units));
    if (currentSubscription.approved) {
        updatedIndex = {
            ...updatedIndex,
            totalUnitsApproved: toBN(updatedIndex.totalUnitsApproved)
                .add(unitsDelta)
                .toString(),
            totalUnits: toBN(updatedIndex.totalUnits)
                .add(unitsDelta)
                .toString(),
        };
    } else {
        updatedIndex = {
            ...updatedIndex,
            totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
                .add(unitsDelta)
                .toString(),
            totalUnits: toBN(updatedIndex.totalUnits)
                .add(unitsDelta)
                .toString(),
        };
    }

    const totalSubscriptionsWithUnitsDelta = subscriptionWithUnitsExists
        ? -1
        : 0;

    if (units.eq(toBN(0))) {
        updatedTokenStats = {
            ...getExpectedTokenStatsForCFAEvent(
                updatedTokenStats,
                atsArray,
                updatedAtBlockNumber,
                timestamp,
                FlowActionType.Update,
                toBN(0),
                toBN(0)
            ),
            totalSubscriptionsWithUnits:
                updatedTokenStats.totalSubscriptionsWithUnits +
                totalSubscriptionsWithUnitsDelta,
        };

        updatedIndex = {
            ...updatedIndex,
            totalSubscriptionsWithUnits:
                updatedIndex.totalSubscriptionsWithUnits +
                totalSubscriptionsWithUnitsDelta,
        };

        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalSubscriptionsWithUnits:
                updatedSubscriberATS.totalSubscriptionsWithUnits +
                totalSubscriptionsWithUnitsDelta,
        };
    }

    if (
        units.gt(toBN(0)) &&
        toBN(currentSubscription.units).eq(toBN(0))
    ) {
        updatedIndex = {
            ...updatedIndex,
            totalSubscriptionsWithUnits:
                updatedIndex.totalSubscriptionsWithUnits + 1,
        };

        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalSubscriptionsWithUnits:
                updatedSubscriberATS.totalSubscriptionsWithUnits + 1,
        };

        updatedTokenStats = {
            ...getExpectedTokenStatsForCFAEvent(
                updatedTokenStats,
                atsArray,
                updatedAtBlockNumber,
                timestamp,
                FlowActionType.Update,
                toBN(0),
                toBN(0)
            ),
            totalSubscriptionsWithUnits:
                updatedTokenStats.totalSubscriptionsWithUnits + 1,
        };
    }

    const balanceDelta = toBN(updatedSubscription.units).mul(
        toBN(updatedIndex.indexValue).sub(
            toBN(updatedSubscription.indexValueUntilUpdatedAt)
        )
    );

    updatedSubscription = {
        ...updatedSubscription,
        totalAmountReceivedUntilUpdatedAt: toBN(
            updatedSubscription.totalAmountReceivedUntilUpdatedAt
        )
            .add(balanceDelta)
            .toString(),
        indexValueUntilUpdatedAt: updatedIndex.indexValue,
        units: units.toString(),
    };

    updatedPublisherATS = await getExpectedATSForCFAEvent(
        token,
        currentPublisherATS,
        updatedAtBlockNumber,
        timestamp,
        FlowActionType.Update,
        true,
        toBN(0),
        toBN(0),
        provider
    );

    // handle the case where the subscriber is the publisher
    if (updatedSubscriberATS.id === updatedPublisherATS.id) {
        updatedPublisherATS = { ...updatedSubscriberATS };
    }

    return {
        updatedIndex,
        updatedSubscription,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
};
