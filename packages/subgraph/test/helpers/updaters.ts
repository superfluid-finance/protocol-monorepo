/**************************************************************************
 * Entity Updaters (For expected values)
 *
 * This file contains all the updater functions for the objects initalized
 * in the intializers.ts file. This file contains the logic for changing
 * the data to what we expect it to be. The output of these functions
 * are what is used to compare against the data obtained from the graph.
 *************************************************************************/

import { BigNumber } from "@ethersproject/bignumber";
import { SuperToken } from "../../typechain/SuperToken";
import {
    IAccountTokenSnapshot,
    IFlowUpdatedUpdateTestData,
    IGetExpectedIDADataBase,
    IGetExpectedSubscriberIDAData,
    IIndex,
    IStreamData,
    ISubscriber,
    ITokenStatistic,
    IUpdateIndexData,
} from "../interfaces";
import {
    actionTypeToActiveStreamsDeltaMap,
    actionTypeToClosedStreamsDeltaMap,
    FlowActionType,
} from "./constants";
import { getCurrentTotalAmountStreamed, toBN } from "./helpers";

export const getExpectedStreamData = (
    currentStreamData: IStreamData,
    actionType: FlowActionType,
    oldFlowRate: string,
    lastUpdatedAtTimestamp: string,
    streamedAmountSinceUpdatedAt: BigNumber
) => {
    const revisionIndexDelta =
        actionTypeToClosedStreamsDeltaMap.get(actionType)!;
    const revisionIndex = (
        Number(currentStreamData.revisionIndex) + revisionIndexDelta
    ).toString();
    const updatedStreamedUntilUpdatedAt = toBN(
        currentStreamData.streamedUntilUpdatedAt
    ).add(streamedAmountSinceUpdatedAt);
    return {
        ...currentStreamData,
        revisionIndex,
        oldFlowRate,
        streamedUntilUpdatedAt: updatedStreamedUntilUpdatedAt.toString(),
        lastUpdatedAtTimestamp,
    } as IStreamData;
};

export const getExpectedIndex = (
    currentIndex: IIndex,
    updatedIndexData: IUpdateIndexData
) => {
    const {
        oldIndexValue,
        newIndexValue,
        totalSubscribersDelta,
        totalUnitsPending,
        totalUnitsApproved,
    } = updatedIndexData;
    const updatedData = {
        ...currentIndex,
        oldIndexValue:
            oldIndexValue == null ? currentIndex.oldIndexValue : oldIndexValue,
        newIndexValue:
            newIndexValue == null ? currentIndex.newIndexValue : newIndexValue,
        totalSubscribers:
            currentIndex.totalSubscribers +
            (totalSubscribersDelta == null ? 0 : totalSubscribersDelta),
        totalUnitsPending:
            totalUnitsPending == null
                ? currentIndex.totalUnitsPending
                : totalUnitsPending.toString(),
        totalUnitsApproved:
            totalUnitsApproved == null
                ? currentIndex.totalUnitsApproved
                : totalUnitsApproved.toString(),
    };

    return {
        ...currentIndex,
        ...updatedData,
    };
};

/**
 * Updates AccountTokenSnapshot balance and stream data. Used on
 * any updates to the AccountTokenSnapshot entity.
 */
export const getExpectedATSForCFAEvent = async (
    superToken: SuperToken,
    currentATS: IAccountTokenSnapshot,
    updatedAtBlock: string,
    lastUpdatedAtTimestamp: string,
    actionType: FlowActionType,
    isSender: boolean,
    flowRate: BigNumber,
    flowRateDelta: BigNumber
): Promise<IAccountTokenSnapshot> => {
    const balanceUntilUpdatedAt = (
        await superToken.balanceOf(currentATS.account.id)
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
    const totalAmountStreamedUntilUpdatedAt =
        isSender === true
            ? toBN(currentATS.totalAmountStreamedUntilUpdatedAt)
                  .add(
                      toBN(currentATS.totalOutflowRate).mul(
                          toBN(lastUpdatedAtTimestamp).sub(
                              toBN(currentATS.updatedAtTimestamp)
                          )
                      )
                  )
                  .toString()
            : currentATS.totalAmountStreamedUntilUpdatedAt;
    return {
        ...currentATS,
        updatedAtBlock,
        updatedAtTimestamp: lastUpdatedAtTimestamp,
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
    updatedAtBlock: string,
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

    // TODO: consider summing all ATS and comparing it with that
    // consider that the ATS updatedAt times are all different.
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
        updatedAtBlock,
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
        flowRateDelta
    );
    const updatedReceiverATS = await getExpectedATSForCFAEvent(
        superToken,
        currentReceiverATS,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        actionType,
        false,
        flowRate,
        flowRateDelta
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

/**
 * Gets all the expected data necessary for validating the subgraph data after the
 * SubscriptionApproved event.
 */
export const getExpectedDataForSubscriptionApproved = async (
    data: IGetExpectedSubscriberIDAData,
    subscriptionExists: boolean
) => {
    const {
        token,
        currentIndex,
        currentSubscriber,
        atsArray,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlock,
        timestamp,
    } = data;

    const balanceDelta = toBN(currentIndex.newIndexValue)
        .sub(toBN(currentSubscriber.lastIndexValue))
        .mul(toBN(currentSubscriber.units));

    let updatedIndex = { ...currentIndex };
    let updatedSubscriber = {
        ...currentSubscriber,
        approved: true,
        lastIndexValue: currentIndex.newIndexValue,
    };
    let updatedPublisherATS: IAccountTokenSnapshot = {
        ...currentPublisherATS,
    };
    let updatedTokenStats: ITokenStatistic = {
        ...getExpectedTokenStatsForCFAEvent(
            currentTokenStats,
            atsArray,
            updatedAtBlock,
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
            updatedAtBlock,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0)
        )),
        totalApprovedSubscriptions:
            currentSubscriberATS.totalApprovedSubscriptions + 1,
    };

    if (subscriptionExists === true) {
        // Update Index
        updatedIndex = getExpectedIndex(currentIndex, {
            totalUnitsApproved: toBN(currentIndex.totalUnitsApproved).add(
                toBN(currentSubscriber.units)
            ),
            totalUnitsPending: toBN(currentIndex.totalUnitsPending).sub(
                toBN(currentSubscriber.units)
            ),
        });

        // Update Subscriber
        updatedSubscriber = {
            ...updatedSubscriber,
            totalAmountReceivedUntilUpdatedAt: toBN(
                updatedSubscriber.totalAmountReceivedUntilUpdatedAt
            )
                .add(balanceDelta)
                .toString(),
        };

        // Update Publisher ATS entity (stream data + balance)
        updatedPublisherATS = await getExpectedATSForCFAEvent(
            token,
            currentPublisherATS,
            updatedAtBlock,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0)
        );
    } else {
        // Update Subscriber entity
        updatedIndex = {
            ...updatedIndex,
            totalSubscribers: updatedIndex.totalSubscribers + 1,
        };
        // Update Subscriber ATS entity
        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalSubscriptions: currentSubscriberATS.totalSubscriptions + 1,
        };

        // Update Token Stats
        updatedTokenStats = {
            ...updatedTokenStats,
            totalSubscriptions: updatedTokenStats.totalSubscriptions + 1,
        };
    }

    return {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
};

/**
 * Gets all the expected data necessary for validating data after the
 * SubscriptionRevoked event.
 */
export const getExpectedDataForRevokeOrDeleteSubscription = async (
    data: IGetExpectedSubscriberIDAData,
    isRevoke: boolean
) => {
    const {
        token,
        currentIndex,
        currentSubscriber,
        atsArray,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlock,
        timestamp,
    } = data;
    const balanceDelta = toBN(currentIndex.newIndexValue)
        .sub(toBN(currentSubscriber.lastIndexValue))
        .mul(toBN(currentSubscriber.units));

    let updatedIndex: IIndex = {
        ...currentIndex,
    };
    let updatedSubscriber: ISubscriber = {
        ...currentSubscriber,
        approved: false,
        totalAmountReceivedUntilUpdatedAt: toBN(
            currentSubscriber.totalAmountReceivedUntilUpdatedAt
        )
            .add(balanceDelta)
            .toString(),
        lastIndexValue: updatedIndex.newIndexValue,
    };
    let updatedPublisherATS: IAccountTokenSnapshot = {
        ...currentPublisherATS,
    };

    let updatedTokenStats: ITokenStatistic = {
        ...getExpectedTokenStatsForCFAEvent(
            currentTokenStats,
            atsArray,
            updatedAtBlock,
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
            updatedAtBlock,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0)
        )),
    };

    // handleRevokeOrDelete
    if (isRevoke) {
        updatedIndex = {
            ...updatedIndex,
            totalUnitsApproved: toBN(updatedIndex.totalUnitsApproved)
                .sub(currentSubscriber.units)
                .toString(),
            totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
                .add(currentSubscriber.units)
                .toString(),
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
                .sub(currentSubscriber.units)
                .toString(),
            totalSubscribers: currentIndex.totalSubscribers - 1,
        };
        if (currentSubscriber.approved) {
            updatedIndex = {
                ...updatedIndex,
                totalUnitsApproved: toBN(updatedIndex.totalUnitsApproved)
                    .sub(currentSubscriber.units)
                    .toString(),
            };
        } else {
            updatedIndex = {
                ...updatedIndex,
                totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
                    .sub(currentSubscriber.units)
                    .toString(),
            };
        }
        updatedSubscriber = { ...updatedSubscriber, units: "0" };
        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalSubscriptions: updatedSubscriberATS.totalSubscriptions - 1,
            totalApprovedSubscriptions:
                updatedSubscriberATS.totalApprovedSubscriptions - 1,
        };
        updatedTokenStats = {
            ...updatedTokenStats,
            totalApprovedSubscriptions:
                updatedTokenStats.totalApprovedSubscriptions - 1,
            totalSubscriptions: updatedTokenStats.totalSubscriptions - 1,
        };
    }

    if (currentSubscriber.approved === false) {
        updatedPublisherATS = {
            ...(await getExpectedATSForCFAEvent(
                token,
                updatedPublisherATS,
                updatedAtBlock,
                timestamp,
                FlowActionType.Update,
                true,
                toBN(0),
                toBN(0)
            )),
        };
    }

    return {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
};

/**
 * Gets all the expected data necessary for validating data after the
 * SubscriptionUnitsUpdated event.
 */
export const getExpectedDataForSubscriptionUnitsUpdated = async (
    data: IGetExpectedSubscriberIDAData,
    units: string,
    subscriptionExists: boolean
) => {
    const {
        token,
        currentIndex,
        currentSubscriber,
        atsArray,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlock,
        timestamp,
    } = data;
    let updatedIndex = { ...currentIndex };
    let updatedSubscriber = { ...currentSubscriber };
    let updatedPublisherATS = { ...currentPublisherATS };
    let updatedSubscriberATS = {
        ...currentSubscriberATS,
        ...(await getExpectedATSForCFAEvent(
            token,
            currentSubscriberATS,
            updatedAtBlock,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0)
        )),
    };
    let updatedTokenStats = { ...currentTokenStats };

    const unitsDelta = toBN(units.toString()).sub(
        toBN(currentSubscriber.units)
    );
    if (subscriptionExists && currentSubscriber.approved) {
        updatedIndex = {
            ...updatedIndex,
            totalUnitsApproved: toBN(updatedIndex.totalUnitsApproved)
                .add(unitsDelta)
                .toString(),
            totalUnits: toBN(updatedIndex.totalUnits)
                .add(unitsDelta)
                .toString(),
        };
    } else if (subscriptionExists) {
        updatedIndex = {
            ...updatedIndex,
            totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
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
                .add(toBN(units))
                .toString(),
            totalUnits: toBN(updatedIndex.totalUnits)
                .add(toBN(units))
                .toString(),
            totalSubscribers: updatedIndex.totalSubscribers + 1,
        };

        updatedSubscriber = {
            ...updatedSubscriber,
            lastIndexValue: updatedIndex.newIndexValue,
            units: units.toString(),
        };

        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalSubscriptions: updatedSubscriberATS.totalSubscriptions + 1,
        };
        updatedTokenStats = {
            ...getExpectedTokenStatsForCFAEvent(
                updatedTokenStats,
                atsArray,
                updatedAtBlock,
                timestamp,
                FlowActionType.Update,
                toBN(0),
                toBN(0)
            ),
            totalSubscriptions: updatedTokenStats.totalSubscriptions + 1,
        };
    }

    const balanceDelta = toBN(updatedSubscriber.units).mul(
        toBN(updatedIndex.newIndexValue).sub(
            toBN(updatedSubscriber.lastIndexValue)
        )
    );

    updatedSubscriber = {
        ...updatedSubscriber,
        totalAmountReceivedUntilUpdatedAt: toBN(
            updatedSubscriber.totalAmountReceivedUntilUpdatedAt
        )
            .add(balanceDelta)
            .toString(),
    };

    if (currentSubscriber.approved === false) {
        updatedPublisherATS = await getExpectedATSForCFAEvent(
            token,
            currentPublisherATS,
            updatedAtBlock,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0)
        );
    }

    if (subscriptionExists) {
        updatedSubscriber = {
            ...updatedSubscriber,
            lastIndexValue: updatedIndex.newIndexValue,
            units: units.toString(),
        };
    }

    return {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
};

/**
 * Gets all the expected data necessary for validating data after the
 * IndexUpdated event.
 */
export const getExpectedDataForIndexUpdated = async (
    data: IGetExpectedIDADataBase,
    totalUnits: BigNumber,
    newIndexValue: BigNumber,
    indexTotalUnitsApproved: BigNumber,
    indexTotalUnitsPending: BigNumber
) => {
    const {
        token,
        atsArray,
        currentIndex,
        currentPublisherATS,
        currentTokenStats,
        updatedAtBlock,
        timestamp,
    } = data;

    const amountDistributedDelta = totalUnits.mul(
        newIndexValue.sub(toBN(currentIndex.newIndexValue))
    );

    const updatedIndex: IIndex = {
        ...currentIndex,
        oldIndexValue: currentIndex.newIndexValue,
        newIndexValue: newIndexValue.toString(),
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
            updatedAtBlock,
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
        updatedAtBlock,
        timestamp,
        FlowActionType.Update,
        true,
        toBN(0),
        toBN(0)
    );

    return { updatedIndex, updatedPublisherATS, updatedTokenStats };
};
