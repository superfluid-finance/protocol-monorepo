/**************************************************************************
 * Entity Initializers
 *
 * This file contains all the getter functions for the different HOL and
 * Aggregate entities which we need to maintain a record of expected
 * values for testing. We initialize the entity when it doesn't exist and
 * retrieve it from the global object if it does.
 *************************************************************************/

import {
    IAccountTokenSnapshot,
    IFlowUpdatedInitTestData,
    IIndex,
    IInstantDistributionTestData,
    IStreamData,
    IIndexSubscription,
    ITokenStatistic,
} from "../interfaces";
import {
    getIndexId,
    getRevisionIndexId,
    getStreamId,
    getSubscriptionId,
} from "./helpers";

export const getOrInitRevisionIndex = (
    revisionIndexes: { [id: string]: number | undefined },
    revisionIndexId: string
): number => {
    return revisionIndexes[revisionIndexId] || 0;
};
export const getOrInitPeriodRevisionIndex = (
    periodRevisionIndexes: { [id: string]: number | undefined },
    revisionIndexId: string
): number => {
    return periodRevisionIndexes[revisionIndexId] || 0;
};

export const getOrInitStreamData = (
    streamData: { [id: string]: IStreamData | undefined },
    revisionIndex: string,
    periodRevisionIndex: string,
    streamId: string,
    updatedAtTimestamp: string
): IStreamData => {
    const existingStreamData = streamData[streamId];
    if (existingStreamData == null) {
        return {
            id: streamId,
            revisionIndex,
            periodRevisionIndex,
            oldFlowRate: "0",
            streamedUntilUpdatedAt: "0",
            updatedAtTimestamp,
        };
    }
    return existingStreamData;
};

export const getOrInitIndex = (
    indexes: { [id: string]: IIndex | undefined },
    indexEntityId: string,
    updatedAtBlockNumber: string,
    updatedAtTimestamp: string
): IIndex => {
    const existingIndex = indexes[indexEntityId];
    if (existingIndex == null) {
        const [publisher, token, indexId] = indexEntityId.split("-");
        return {
            id: indexEntityId,
            createdAtTimestamp: updatedAtTimestamp,
            createdAtBlockNumber: updatedAtBlockNumber,
            updatedAtBlockNumber,
            updatedAtTimestamp,
            indexId,
            publisher: { id: publisher },
            token: { id: token },
            indexValue: "0",
            totalSubscriptionsWithUnits: 0,
            totalUnits: "0",
            totalUnitsApproved: "0",
            totalAmountDistributedUntilUpdatedAt: "0",
            totalUnitsPending: "0",
        };
    }
    return existingIndex;
};

export const getOrInitSubscription = (
    subscription: { [id: string]: IIndexSubscription | undefined },
    subscriptionId: string,
    updatedAtBlockNumber: string,
    updatedAtTimestamp: string
): IIndexSubscription => {
    const existingSubscription = subscription[subscriptionId];
    if (existingSubscription == null) {
        const [subscriber, publisher, token, indexId] =
            subscriptionId.split("-");
        const indexEntityId = getIndexId(publisher, token, indexId);
        return {
            id: subscriptionId,
            createdAtTimestamp: updatedAtTimestamp,
            createdAtBlockNumber: updatedAtBlockNumber,
            updatedAtBlockNumber,
            updatedAtTimestamp: updatedAtTimestamp,
            subscriber: { id: subscriber },
            approved: false,
            units: "0",
            totalAmountReceivedUntilUpdatedAt: "0",
            indexValueUntilUpdatedAt: "0",
            index: {
                id: indexEntityId,
                publisher: { id: publisher },
                indexId,
                token: { id: token },
            },
        };
    }
    return existingSubscription;
};

export const getOrInitAccountTokenSnapshot = (
    accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    },
    accountId: string,
    tokenId: string,
    updatedAtBlockNumber: string,
    updatedAtTimestamp: string
): IAccountTokenSnapshot => {
    const atsId = accountId + "-" + tokenId;
    const existingATS = accountTokenSnapshots[atsId];
    if (existingATS == null) {
        return {
            id: accountId + "-" + tokenId,
            updatedAtBlockNumber,
            updatedAtTimestamp,
            totalNumberOfActiveStreams: 0,
            totalNumberOfClosedStreams: 0,
            totalSubscriptionsWithUnits: 0,
            totalApprovedSubscriptions: 0,
            balanceUntilUpdatedAt: "0",
            totalNetFlowRate: "0",
            totalInflowRate: "0",
            totalOutflowRate: "0",
            totalAmountStreamedUntilUpdatedAt: "0",
            totalAmountTransferredUntilUpdatedAt: "0",
            account: { id: accountId },
            token: { id: tokenId },
        };
    }
    return existingATS;
};

export const getOrInitTokenStatistics = (
    tokenStatistics: { [id: string]: ITokenStatistic | undefined },
    tokenId: string,
    updatedAtBlockNumber: string,
    updatedAtTimestamp: string,
    totalSupply?: string
): ITokenStatistic => {
    const existingTokenStats = tokenStatistics[tokenId];
    if (existingTokenStats == null) {
        return {
            id: tokenId,
            updatedAtBlockNumber,
            updatedAtTimestamp,
            totalNumberOfActiveStreams: 0,
            totalNumberOfClosedStreams: 0,
            totalNumberOfIndexes: 0,
            totalNumberOfActiveIndexes: 0,
            totalSubscriptionsWithUnits: 0,
            totalApprovedSubscriptions: 0,
            totalOutflowRate: "0",
            totalAmountStreamedUntilUpdatedAt: "0",
            totalAmountTransferredUntilUpdatedAt: "0",
            totalAmountDistributedUntilUpdatedAt: "0",
            totalSupply: totalSupply || "0",
            token: { id: tokenId },
        };
    }
    return existingTokenStats;
};

/**
 * Gets/Initializes all data for the FlowUpdated event
 * @param testData
 * @returns
 */
export function getOrInitializeDataForFlowUpdated(
    testData: IFlowUpdatedInitTestData
) {
    const {
        accountTokenSnapshots,
        updatedAtBlockNumber,
        updatedAtTimestamp,
        receiver,
        revisionIndexes,
        periodRevisionIndexes,
        sender,
        streamData,
        token,
        tokenStatistics,
        totalSupply,
    } = testData;

    const revisionIndexId = getRevisionIndexId(sender, receiver, token);
    const tokenId = token.toLowerCase();
    const currentRevisionIndex = getOrInitRevisionIndex(
        revisionIndexes,
        revisionIndexId
    );
    const currentPeriodRevisionIndex = getOrInitPeriodRevisionIndex(
        periodRevisionIndexes,
        revisionIndexId
    );
    const streamId = getStreamId(
        sender,
        receiver,
        token,
        currentRevisionIndex.toString()
    );
    const pastStreamData = getOrInitStreamData(
        streamData,
        currentRevisionIndex.toString(),
        currentPeriodRevisionIndex.toString(),
        streamId,
        updatedAtTimestamp
    );
    const currentSenderATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        sender.toLowerCase(),
        tokenId,
        updatedAtBlockNumber,
        updatedAtTimestamp
    );
    const currentReceiverATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        receiver.toLowerCase(),
        tokenId,
        updatedAtBlockNumber,
        updatedAtTimestamp
    );
    const currentTokenStats = getOrInitTokenStatistics(
        tokenStatistics,
        tokenId,
        updatedAtBlockNumber,
        updatedAtTimestamp,
        totalSupply
    );
    
    return {
        currentSenderATS,
        currentReceiverATS,
        currentTokenStats,
        pastStreamData,
        revisionIndexId,
    };
}

/**
 * Gets/Initializes all data for the different IDA events
 * @param testData
 * @returns
 */
export function getOrInitializeDataForIDA(
    testData: IInstantDistributionTestData
) {
    const {
        accountTokenSnapshots,
        indexes,
        indexId,
        updatedAtTimestamp,
        updatedAtBlockNumber,
        publisher,
        subscriber,
        subscriptions,
        token,
        tokenStatistics,
    } = testData;
    let subscriberAddress = subscriber || "";
    const subscriptionId = getSubscriptionId(
        subscriberAddress,
        publisher,
        token,
        indexId
    );
    const indexEntityId = getIndexId(publisher, token, indexId);
    const currentIndex = getOrInitIndex(
        indexes,
        indexEntityId,
        updatedAtBlockNumber,
        updatedAtTimestamp
    );

    const currentSubscription = getOrInitSubscription(
        subscriptions,
        subscriptionId,
        updatedAtBlockNumber,
        updatedAtTimestamp
    );

    const currentPublisherATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        publisher.toLowerCase(),
        token.toLowerCase(),
        updatedAtBlockNumber,
        updatedAtTimestamp
    );
    const currentSubscriberATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        subscriberAddress.toLowerCase(),
        token.toLowerCase(),
        updatedAtBlockNumber,
        updatedAtTimestamp
    );

    const currentTokenStats = getOrInitTokenStatistics(
        tokenStatistics,
        token.toLowerCase(),
        updatedAtBlockNumber,
        updatedAtTimestamp
    );
    return {
        subscriptionId,
        indexEntityId,
        currentIndex,
        currentSubscription,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
    };
}
