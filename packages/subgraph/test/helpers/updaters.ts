/**************************************************************************
 * Entity Updaters (For expected values)
 *
 * This file contains all the updater functions for the objects initalized
 * in the intializers.ts file. This file contains the logic for changing
 * the data to what we expect it to be. The output of these functions
 * are what is used to compare against the data obtained from the graph.
 *************************************************************************/

import BN from "bn.js";
import { BigNumber } from "@ethersproject/bignumber";
import { SuperToken } from "../../typechain/SuperToken";
import {
    IAccountTokenSnapshot,
    IFlowUpdatedUpdateTestData,
    IGetExpectedIDADataParams,
    IIndex,
    IStreamData,
    ISubscriber,
    ITokenStatistic,
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

export const getExpectedDataForIndexCreated = async (
    data: IGetExpectedIDADataParams
) => {
    const {
        atsArray,
        currentIndex,
        currentSubscriber,
        currentSubscriberATS,
        currentPublisherATS,
        currentTokenStats,
        updatedAtBlock,
        timestamp,
    } = data;

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
        totalNumberOfIndexes: currentTokenStats.totalNumberOfIndexes + 1,
    };

    return {
        updatedIndex: currentIndex,
        updatedPublisherATS: currentPublisherATS,
        updatedTokenStats,
        updatedSubscriber: currentSubscriber,
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
        currentSubscriber,
        currentSubscriberATS,
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

    return {
        updatedIndex,
        updatedPublisherATS,
        updatedTokenStats,
        updatedSubscriber: currentSubscriber,
        updatedSubscriberATS: currentSubscriberATS,
    };
};

export const getExpectedDataForSubscriptionApproved = async (
    data: IGetExpectedIDADataParams,
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
        updatedIndex = {
            ...currentIndex,
            totalUnitsApproved: toBN(currentIndex.totalUnitsApproved)
                .add(toBN(currentSubscriber.units))
                .toString(),
            totalUnitsPending: toBN(currentIndex.totalUnitsPending)
                .sub(toBN(currentSubscriber.units))
                .toString(),
        };

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

export const getExpectedDataForRevokeOrDeleteSubscription = async (
    data: IGetExpectedIDADataParams,
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
            totalUnitsApproved: currentSubscriber.approved
                ? toBN(updatedIndex.totalUnitsApproved)
                      .sub(currentSubscriber.units)
                      .toString()
                : currentIndex.totalUnitsApproved,
            totalUnitsPending: currentSubscriber.approved
                ? toBN(updatedIndex.totalUnitsPending)
                      .add(currentSubscriber.units)
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
                currentSubscriber.approved === true
                    ? updatedSubscriberATS.totalApprovedSubscriptions - 1
                    : updatedSubscriberATS.totalApprovedSubscriptions,
        };
        updatedTokenStats = {
            ...updatedTokenStats,
            totalApprovedSubscriptions:
                currentSubscriber.approved === true
                    ? updatedTokenStats.totalApprovedSubscriptions - 1
                    : updatedTokenStats.totalApprovedSubscriptions,
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

export const getExpectedDataForSubscriptionUnitsUpdated = async (
    data: IGetExpectedIDADataParams,
    units: BN,
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
    const stringUnits = units.toString();

    const unitsDelta = toBN(stringUnits).sub(toBN(currentSubscriber.units));
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
                .add(toBN(stringUnits))
                .toString(),
            totalUnits: toBN(updatedIndex.totalUnits)
                .add(toBN(stringUnits))
                .toString(),
            totalSubscribers: updatedIndex.totalSubscribers + 1,
        };

        updatedSubscriber = {
            ...updatedSubscriber,
            lastIndexValue: updatedIndex.newIndexValue,
            units: stringUnits,
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
