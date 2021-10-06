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
    ISubscriber,
    ITokenStatistic,
} from "../interfaces";
import {
    getIndexId,
    getRevisionIndexId,
    getStreamId,
    getSubscriberId,
} from "./helpers";

export const getOrInitRevisionIndex = (
    revisionIndexes: { [id: string]: number | undefined },
    revisionIndexId: string
): number => {
    return revisionIndexes[revisionIndexId] || 0;
};

export const getOrInitStreamData = (
    streamData: { [id: string]: IStreamData | undefined },
    revisionIndex: string,
    streamId: string,
    lastUpdatedAtTimestamp: string
): IStreamData => {
    const existingStreamData = streamData[streamId];
    if (existingStreamData == null) {
        return {
            id: streamId,
            revisionIndex,
            oldFlowRate: "0",
            streamedUntilUpdatedAt: "0",
            lastUpdatedAtTimestamp,
        };
    }
    return existingStreamData;
};

export const getOrInitIndex = (
    indexes: { [id: string]: IIndex | undefined },
    indexEntityId: string,
    updatedAtBlock: string,
    updatedAtTimestamp: string
): IIndex => {
    const existingIndex = indexes[indexEntityId];
    if (existingIndex == null) {
        const [publisher, token, indexId] = indexEntityId.split("-");
        return {
            id: indexEntityId,
            createdAt: updatedAtTimestamp,
            updatedAtBlock: updatedAtBlock,
            updatedAtTimestamp,
            indexId,
            publisher: { id: publisher },
            token: { id: token },
            userData: "0x",
            oldIndexValue: "0",
            newIndexValue: "0",
            totalSubscribers: 0,
            totalUnits: "0",
            totalUnitsApproved: "0",
            totalAmountDistributedUntilUpdatedAt: "0",
            totalUnitsPending: "0",
        };
    }
    return existingIndex;
};

export const getOrInitSubscriber = (
    subscribers: { [id: string]: ISubscriber | undefined },
    subscriberId: string,
    updatedAtBlock: string,
    updatedAtTimestamp: string
): ISubscriber => {
    const existingSubscriber = subscribers[subscriberId];
    if (existingSubscriber == null) {
        const [subscriber, publisher, token, indexId] = subscriberId.split("-");
        const indexEntityId = getIndexId(publisher, token, indexId);
        return {
            id: subscriberId,
            createdAt: updatedAtTimestamp,
            updatedAtBlock: updatedAtBlock,
            updatedAtTimestamp: updatedAtTimestamp,
            token: { id: token },
            subscriber: { id: subscriber },
            publisher: { id: publisher },
            indexId,
            userData: "0x",
            approved: false,
            units: "0",
            totalAmountReceivedUntilUpdatedAt: "0",
            lastIndexValue: "0",
            index: { id: indexEntityId },
        };
    }
    return existingSubscriber;
};

export const getOrInitAccountTokenSnapshot = (
    accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    },
    accountId: string,
    tokenId: string,
    updatedAtBlock: string,
    updatedAtTimestamp: string
): IAccountTokenSnapshot => {
    const atsId = accountId + "-" + tokenId;
    const existingATS = accountTokenSnapshots[atsId];
    if (existingATS == null) {
        return {
            id: accountId + "-" + tokenId,
            updatedAtBlock,
            updatedAtTimestamp,
            totalNumberOfActiveStreams: 0,
            totalNumberOfClosedStreams: 0,
            totalSubscriptions: 0,
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
    updatedAtBlock: string,
    updatedAtTimestamp: string
) => {
    const existingTokenStats = tokenStatistics[tokenId];
    if (existingTokenStats == null) {
        return {
            id: tokenId,
            updatedAtBlock,
            updatedAtTimestamp,
            totalNumberOfActiveStreams: 0,
            totalNumberOfClosedStreams: 0,
            totalNumberOfIndexes: 0,
            totalNumberOfActiveIndexes: 0,
            totalSubscriptions: 0,
            totalApprovedSubscriptions: 0,
            totalOutflowRate: "0",
            totalAmountStreamedUntilUpdatedAt: "0",
            totalAmountTransferredUntilUpdatedAt: "0",
            totalAmountDistributedUntilUpdatedAt: "0",
            totalSupply: "0",
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
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        receiver,
        revisionIndexes,
        sender,
        streamData,
        token,
        tokenStatistics,
    } = testData;

    const revisionIndexId = getRevisionIndexId(sender, receiver, token);
    const tokenId = token.toLowerCase();
    const currentRevisionIndex = getOrInitRevisionIndex(
        revisionIndexes,
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
        streamId,
        lastUpdatedAtTimestamp
    );
    const currentSenderATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        sender.toLowerCase(),
        tokenId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    const currentReceiverATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        receiver.toLowerCase(),
        tokenId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    const currentTokenStats = getOrInitTokenStatistics(
        tokenStatistics,
        tokenId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
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
        lastUpdatedAtTimestamp,
        lastUpdatedBlockNumber,
        publisher,
        subscriber,
        subscribers,
        token,
        tokenStatistics,
    } = testData;
    let subscriberAddress = subscriber || "";
    const subscriberEntityId = getSubscriberId(
        subscriberAddress,
        publisher,
        token,
        indexId
    );
    const indexEntityId = getIndexId(publisher, token, indexId);
    const currentIndex = getOrInitIndex(
        indexes,
        indexEntityId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );

    const currentSubscriber = getOrInitSubscriber(
        subscribers,
        subscriberEntityId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );

    const currentPublisherATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        publisher.toLowerCase(),
        token.toLowerCase(),
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    const currentSubscriberATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        subscriberAddress.toLowerCase(),
        token.toLowerCase(),
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );

    const currentTokenStats = getOrInitTokenStatistics(
        tokenStatistics,
        token.toLowerCase(),
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    return {
        subscriberEntityId,
        indexEntityId,
        currentIndex,
        currentSubscriber,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
    };
}
