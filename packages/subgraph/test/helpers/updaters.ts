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
        userData,
        oldIndexValue,
        newIndexValue,
        totalSubscribersDelta,
        totalUnitsPending,
        totalUnitsApproved,
    } = updatedIndexData;
    const updatedData = {
        ...currentIndex,
        userData: userData == null ? currentIndex.userData : userData,
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
 * Gets all the expected data necessary for validating data after the
 * FlowUpdatedEvent.
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
 * Gets all the expected data necessary for validating data after the
 * SubscriptionRevokedEvent.
 */
export const getExpectedDataForRevokeOrDeleteSubscription = async (
    token: SuperToken,
    currentIndex: IIndex,
    currentSubscriber: ISubscriber,
    accountTokenSnapshots: { [id: string]: IAccountTokenSnapshot | undefined },
    currentPublisherATS: IAccountTokenSnapshot,
    currentSubscriberATS: IAccountTokenSnapshot,
    currentTokenStats: ITokenStatistic,
    isRevoke: boolean,
    userData: string,
    updatedAtBlock: string,
    timestamp: string
) => {
    const balanceDelta = toBN(currentIndex.newIndexValue)
        .sub(toBN(currentSubscriber.lastIndexValue))
        .mul(toBN(currentSubscriber.units));

    let updatedIndex: IIndex = {
        ...currentIndex,
    };
    let updatedSubscriber: ISubscriber = {
        ...currentSubscriber,
        userData,
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
    const accountTokenSnapshotsArray = Object.values(
        accountTokenSnapshots
    ).filter((x) => x != undefined) as IAccountTokenSnapshot[];
    let updatedTokenStats: ITokenStatistic = {
        ...getExpectedTokenStatsForCFAEvent(
            currentTokenStats,
            accountTokenSnapshotsArray,
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
