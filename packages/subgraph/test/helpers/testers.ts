/**************************************************************************
 * Testers
 *
 * This file contains all the tester functions for the different
 * events that are emitted as a result of some user action. The functions
 * are responsible for:
 * - modifying the state of the blockchain
 * - getting the initial data
 * - modifying data to get expected data for comparison
 * - query the graph to get the data to be validated
 * - validate the event based on the expected data
 * - valiate HOL/aggregate entities based on the expected data
 *************************************************************************/
import { ContractReceipt } from "@ethersproject/contracts";
import BN from "bn.js";
import { InstantDistributionAgreementV1Helper } from "@superfluid-finance/js-sdk/src/InstantDistributionAgreementV1Helper";
import {
    IBaseDistributionTesterParams,
    IBaseIDAEvent,
    IContracts,
    IDistributionLocalData,
    IExpectedFlowUpdateEvent,
    IExpectedIndexUpdated,
    IExpectedSubscriberEvent,
    IExpectedSubscriptionUnitsUpdated,
    IFlowUpdated,
    IIndexCreated,
    IIndexUpdated,
    ISubscriberDistributionTesterParams,
    ISubscriptionApproved,
    ISubscriptionRevoked,
    ISubscriptionUnitsUpdated,
    ITestModifyFlowData,
} from "../interfaces";
import {
    getFlowUpdatedEvents,
    getIndexCreatedEvents,
    getIndexUpdatedEvents,
    getSubscriptionApprovedEvents,
    getSubscriptionRevokedEvents,
    getSubscriptionUnitsUpdatedEvents,
} from "../queries/eventQueries";
import { fetchEventAndValidate } from "../validation/eventValidators";
import {
    validateFlowUpdated,
    validateModifyIDA,
} from "../validation/validators";
import {
    hasSubscription,
    modifyFlowAndReturnCreatedFlowData,
    monthlyToSecondRate,
    toBN,
    waitUntilBlockIndexed,
} from "./helpers";
import {
    getOrInitializeDataForFlowUpdated,
    getOrInitializeDataForIDA,
} from "./initializers";
import {
    getExpectedDataForFlowUpdated,
    getExpectedDataForIndexUpdated,
    getExpectedDataForRevokeOrDeleteSubscription,
    getExpectedDataForSubscriptionApproved,
    getExpectedDataForSubscriptionUnitsUpdated,
    getExpectedStreamData,
    getExpectedTokenStatsForCFAEvent,
} from "./updaters";
import { FlowActionType } from "./constants";
import { fetchIndexAndValidate } from "../validation/holValidators";
import { fetchTokenStatsAndValidate } from "../validation/aggregateValidators";

/**
 * A "God" function used to test modify flow events.
 * It modifies a flow (create, update, delete) and returns
 * data obtained from a web3 call. Gets the expected data
 * and validates the FlowUpdatedEvent and all the entities
 * that are updated and need to be validated as well.
 * It then returns the updated (expected) data to be
 * used in future tests.
 * @param data
 * @returns
 */
export async function testFlowUpdated(data: ITestModifyFlowData) {
    const {
        contracts,
        localData,
        provider,
        actionType,
        atsArray,
        newFlowRate,
        sender,
        receiver,
        tokenAddress,
    } = data;

    // Spread operator the variables
    const { sf, cfaV1, superToken } = contracts;
    const {
        accountTokenSnapshots,
        revisionIndexes,
        streamData,
        tokenStatistics,
    } = localData;

    // create/update/delete a flow
    const { receipt, timestamp, flowRate } =
        await modifyFlowAndReturnCreatedFlowData(
            provider,
            sf,
            cfaV1,
            actionType,
            superToken.address,
            sender,
            receiver,
            monthlyToSecondRate(newFlowRate)
        );
    const lastUpdatedAtTimestamp = timestamp.toString();
    const lastUpdatedBlockNumber = receipt.blockNumber.toString();
    const tokenId = tokenAddress.toLowerCase();

    // get or initialize the data
    const {
        currentReceiverATS,
        currentSenderATS,
        currentTokenStats,
        pastStreamData,
        revisionIndexId,
    } = getOrInitializeDataForFlowUpdated({
        lastUpdatedAtTimestamp,
        lastUpdatedBlockNumber,
        sender,
        receiver,
        token: superToken.address,
        accountTokenSnapshots,
        revisionIndexes,
        streamData,
        tokenStatistics,
    });

    // update and return updated (expected) data
    const { updatedSenderATS, updatedReceiverATS, updatedTokenStats } =
        await getExpectedDataForFlowUpdated({
            actionType,
            lastUpdatedBlockNumber,
            lastUpdatedAtTimestamp,
            accountTokenSnapshots: atsArray,
            flowRate,
            superToken,
            pastStreamData,
            currentSenderATS,
            currentReceiverATS,
            currentTokenStats,
        });

    const streamedAmountSinceUpdatedAt = toBN(pastStreamData.oldFlowRate).mul(
        toBN(lastUpdatedAtTimestamp).sub(
            toBN(pastStreamData.lastUpdatedAtTimestamp)
        )
    );

    const senderNetFlow = await cfaV1.getNetFlow(tokenAddress, sender);
    const receiverNetFlow = await cfaV1.getNetFlow(tokenAddress, receiver);
    // validate FlowUpdatedEvent
    const streamedAmountUntilTimestamp = toBN(
        pastStreamData.streamedUntilUpdatedAt
    ).add(streamedAmountSinceUpdatedAt);
    await fetchEventAndValidate<IFlowUpdated, IExpectedFlowUpdateEvent>(
        receipt,
        {
            flowRate: flowRate.toString(),
            oldFlowRate: pastStreamData.oldFlowRate,
            sender: sender.toLowerCase(),
            receiver: receiver.toLowerCase(),
            token: tokenAddress.toLowerCase(),
            totalAmountStreamedUntilTimestamp:
                streamedAmountUntilTimestamp.toString(),
            totalReceiverFlowRate: receiverNetFlow.toString(),
            totalSenderFlowRate: senderNetFlow.toString(),
            type: actionType,
        },
        getFlowUpdatedEvents,
        "flowUpdateds",
        "FlowUpdated"
    );

    await validateFlowUpdated(
        pastStreamData,
        streamedAmountUntilTimestamp,
        flowRate,
        tokenId,
        updatedSenderATS,
        updatedReceiverATS,
        updatedTokenStats
    );

    let updatedStreamData = getExpectedStreamData(
        pastStreamData,
        actionType,
        flowRate.toString(),
        lastUpdatedAtTimestamp,
        streamedAmountSinceUpdatedAt
    );

    return {
        revisionIndexId,
        updatedStreamData,
        updatedReceiverATS,
        updatedSenderATS,
        updatedTokenStats,
    };
}

export async function testIndexCreated(
    contracts: IContracts,
    localData: IDistributionLocalData,
    baseParams: IBaseDistributionTesterParams
) {
    const { sf, idaV1 } = contracts;
    const { accountTokenSnapshots, indexes, subscribers, tokenStatistics } =
        localData;
    const { provider, token, publisher, indexId, atsArray, userData } =
        baseParams;
    console.log("indexCreated baseParams", baseParams);
    const txn: any = await (
        sf.ida as InstantDistributionAgreementV1Helper
    ).createIndex({
        superToken: token,
        publisher,
        indexId,
        userData,
        onTransaction: () => {},
    });

    const receipt: ContractReceipt = txn.receipt;
    const block = await provider.getBlock(receipt.blockNumber);
    const timestamp = block.timestamp.toString();
    await waitUntilBlockIndexed(receipt.blockNumber);

    const updatedAtBlock = receipt.blockNumber.toString();

    await fetchEventAndValidate<IIndexCreated, IBaseIDAEvent>(
        receipt,
        {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            userData,
        },
        getIndexCreatedEvents,
        "indexCreateds",
        "IndexCreated"
    );

    let { currentIndex, currentTokenStats } = getOrInitializeDataForIDA({
        accountTokenSnapshots,
        indexes,
        indexId: indexId.toString(),
        lastUpdatedAtTimestamp: timestamp,
        lastUpdatedBlockNumber: updatedAtBlock,
        publisher,
        subscribers,
        token,
        tokenStatistics,
    });

    let updatedTokenStats = getExpectedTokenStatsForCFAEvent(
        currentTokenStats,
        atsArray,
        updatedAtBlock,
        timestamp.toString(),
        FlowActionType.Update,
        toBN(0),
        toBN(0)
    );
    updatedTokenStats = {
        ...updatedTokenStats,
        totalNumberOfIndexes: updatedTokenStats.totalNumberOfIndexes + 1,
    };

    await fetchIndexAndValidate(idaV1, currentIndex);
    await fetchTokenStatsAndValidate(token.toLowerCase(), updatedTokenStats);

    return { updatedTokenStats, currentIndex };
}

export async function testSubscriptionApproved(
    contracts: IContracts,
    localData: IDistributionLocalData,
    baseParams: ISubscriberDistributionTesterParams
) {
    const { sf, idaV1, superToken } = contracts;
    const { accountTokenSnapshots, indexes, subscribers, tokenStatistics } =
        localData;
    const {
        provider,
        token,
        publisher,
        indexId,
        atsArray,
        userData,
        subscriber,
    } = baseParams;
    const txn: any = await (
        sf.ida as InstantDistributionAgreementV1Helper
    ).approveSubscription({
        superToken: token,
        publisher,
        indexId,
        subscriber,
        userData,
        onTransaction: () => {},
    });
    const receipt: ContractReceipt = txn.receipt;
    const block = await provider.getBlock(receipt.blockNumber);
    const timestamp = block.timestamp.toString();
    await waitUntilBlockIndexed(receipt.blockNumber);

    const updatedAtBlock = receipt.blockNumber.toString();

    let {
        subscriberEntityId,
        currentIndex,
        currentSubscriber,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
    } = getOrInitializeDataForIDA({
        accountTokenSnapshots,
        indexes,
        indexId: indexId.toString(),
        lastUpdatedAtTimestamp: timestamp,
        lastUpdatedBlockNumber: updatedAtBlock,
        publisher,
        subscribers,
        subscriber,
        token,
        tokenStatistics,
    });

    await fetchEventAndValidate<
        ISubscriptionApproved,
        IExpectedSubscriberEvent
    >(
        receipt,
        {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            subscriber: { id: subscriberEntityId },
            userData,
        },
        getSubscriptionApprovedEvents,
        "subscriptionApproveds",
        "SubscriptionApproved"
    );

    const subscriptionExists = hasSubscription(subscribers, subscriberEntityId);

    const {
        updatedTokenStats,
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
    } = await getExpectedDataForSubscriptionApproved(
        {
            token: superToken,
            currentIndex,
            currentSubscriber,
            atsArray,
            currentPublisherATS,
            currentSubscriberATS,
            currentTokenStats,
            updatedAtBlock,
            timestamp,
        },
        subscriptionExists
    );

    await validateModifyIDA(
        idaV1,
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
        token,
        publisher,
        subscriber
    );

    return {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
}

export async function testSubscriptionUnitsUpdated(
    contracts: IContracts,
    localData: IDistributionLocalData,
    baseParams: ISubscriberDistributionTesterParams,
    units: BN
) {
    const { sf, idaV1, superToken } = contracts;
    const { accountTokenSnapshots, indexes, subscribers, tokenStatistics } =
        localData;
    const {
        provider,
        token,
        publisher,
        indexId,
        atsArray,
        userData,
        subscriber,
    } = baseParams;

    const txn: any = await (
        sf.ida as InstantDistributionAgreementV1Helper
    ).updateSubscription({
        superToken: token,
        publisher,
        indexId: indexId,
        subscriber,
        units,
        userData,
        onTransaction: () => {},
    });
    const receipt: ContractReceipt = txn.receipt;
    const block = await provider.getBlock(receipt.blockNumber);
    const timestamp = block.timestamp.toString();
    await waitUntilBlockIndexed(receipt.blockNumber);

    const updatedAtBlock = receipt.blockNumber.toString();

    let {
        subscriberEntityId,
        currentIndex,
        currentSubscriber,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
    } = getOrInitializeDataForIDA({
        accountTokenSnapshots,
        indexes,
        indexId: indexId.toString(),
        lastUpdatedAtTimestamp: timestamp,
        lastUpdatedBlockNumber: updatedAtBlock,
        publisher,
        subscribers,
        subscriber,
        token,
        tokenStatistics,
    });

    await fetchEventAndValidate<
        ISubscriptionUnitsUpdated,
        IExpectedSubscriptionUnitsUpdated
    >(
        receipt,
        {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            subscriber: { id: subscriberEntityId },
            units: units.toString(),
            userData,
        },
        getSubscriptionUnitsUpdatedEvents,
        "subscriptionUnitsUpdateds",
        "SubscriptionUnitsUpdated"
    );

    const subscriptionExists = hasSubscription(subscribers, subscriberEntityId);

    const {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    } = await getExpectedDataForSubscriptionUnitsUpdated(
        {
            token: superToken,
            currentIndex,
            currentSubscriber,
            atsArray,
            currentPublisherATS,
            currentSubscriberATS,
            currentTokenStats,
            updatedAtBlock,
            timestamp,
        },
        units.toString(),
        subscriptionExists
    );

    await validateModifyIDA(
        idaV1,
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
        token,
        publisher,
        subscriber
    );

    return {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
}

export async function testSubscriptionRevoked(
    contracts: IContracts,
    localData: IDistributionLocalData,
    baseParams: ISubscriberDistributionTesterParams,
    isRevoke: boolean,
    sender: string
) {
    const { sf, idaV1, superToken } = contracts;
    const { accountTokenSnapshots, indexes, subscribers, tokenStatistics } =
        localData;
    const {
        provider,
        token,
        publisher,
        indexId,
        atsArray,
        userData,
        subscriber,
    } = baseParams;
    let txn: any;
    if (isRevoke) {
        txn = await (
            sf.ida as InstantDistributionAgreementV1Helper
        ).revokeSubscription({
            superToken: token,
            publisher,
            indexId: indexId,
            subscriber,
            userData,
            onTransaction: () => {},
        });
    } else {
        txn = await (
            sf.ida as InstantDistributionAgreementV1Helper
        ).deleteSubscription({
            superToken: token,
            publisher,
            indexId: indexId,
            sender,
            subscriber,
            userData,
            onTransaction: () => {},
        });
    }
    const receipt: ContractReceipt = txn.receipt;
    const block = await provider.getBlock(receipt.blockNumber);
    const timestamp = block.timestamp.toString();
    await waitUntilBlockIndexed(receipt.blockNumber);

    const updatedAtBlock = receipt.blockNumber.toString();

    let {
        subscriberEntityId,
        currentIndex,
        currentSubscriber,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
    } = getOrInitializeDataForIDA({
        accountTokenSnapshots,
        indexes,
        indexId: indexId.toString(),
        lastUpdatedAtTimestamp: timestamp,
        lastUpdatedBlockNumber: updatedAtBlock,
        publisher,
        subscribers,
        subscriber,
        token,
        tokenStatistics,
    });

    await fetchEventAndValidate<ISubscriptionRevoked, IExpectedSubscriberEvent>(
        receipt,
        {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            subscriber: { id: subscriberEntityId },
            userData,
        },
        getSubscriptionRevokedEvents,
        "subscriptionRevokeds",
        "SubscriptionRevoked"
    );

    const {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    } = await getExpectedDataForRevokeOrDeleteSubscription(
        {
            token: superToken,
            currentIndex,
            currentSubscriber,
            atsArray,
            currentPublisherATS,
            currentSubscriberATS,
            currentTokenStats,
            updatedAtBlock,
            timestamp,
        },
        isRevoke
    );

    await validateModifyIDA(
        idaV1,
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
        token,
        publisher,
        subscriber
    );

    return {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
}

export async function testIndexUpdated(
    contracts: IContracts,
    localData: IDistributionLocalData,
    baseParams: ISubscriberDistributionTesterParams,
    amountOrIndexValue: BN,
    isDistribute: boolean
) {
    const { sf, idaV1, superToken } = contracts;
    const { accountTokenSnapshots, indexes, subscribers, tokenStatistics } =
        localData;
    const {
        provider,
        token,
        publisher,
        indexId,
        atsArray,
        userData,
        subscriber,
    } = baseParams;

    const txn: any = await (
        sf.ida as InstantDistributionAgreementV1Helper
    ).distribute({
        superToken: token,
        publisher,
        indexId: indexId,
        amount: amountOrIndexValue,
        userData,
        onTransaction: () => {},
    });
    const receipt: ContractReceipt = txn.receipt;
    const block = await provider.getBlock(receipt.blockNumber);
    const timestamp = block.timestamp.toString();
    await waitUntilBlockIndexed(receipt.blockNumber);

    const updatedAtBlock = receipt.blockNumber.toString();

    let {
        currentSubscriber,
        currentIndex,
        currentSubscriberATS,
        currentPublisherATS,
        currentTokenStats,
    } = getOrInitializeDataForIDA({
        accountTokenSnapshots,
        indexes,
        indexId: indexId.toString(),
        lastUpdatedAtTimestamp: timestamp,
        lastUpdatedBlockNumber: updatedAtBlock,
        publisher,
        subscribers,
        subscriber,
        token,
        tokenStatistics,
    });

    const [, , indexTotalUnitsApproved, indexTotalUnitsPending] =
        await idaV1.getIndex(token, publisher, indexId);
    const totalUnits = toBN(currentIndex.totalUnitsApproved).add(
        toBN(currentIndex.totalUnitsPending)
    );

    const indexDelta = totalUnits.gt(toBN(0))
        ? toBN(amountOrIndexValue.toString()).div(totalUnits)
        : toBN(0);
    const newIndexValue =
        isDistribute === true
            ? toBN(currentIndex.newIndexValue).add(indexDelta)
            : toBN(amountOrIndexValue.toString());

    await fetchEventAndValidate<IIndexUpdated, IExpectedIndexUpdated>(
        receipt,
        {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            oldIndexValue: currentIndex.newIndexValue,
            newIndexValue: newIndexValue.toString(),
            totalUnitsApproved: indexTotalUnitsApproved.toString(),
            totalUnitsPending: indexTotalUnitsPending.toString(),
            userData,
        },
        getIndexUpdatedEvents,
        "indexUpdateds",
        "IndexUpdated"
    );

    const {
        updatedIndex,
        updatedPublisherATS,
        updatedSubscriber,
        updatedSubscriberATS,
        updatedTokenStats,
    } = await getExpectedDataForIndexUpdated(
        {
            token: superToken,
            currentIndex,
            atsArray,
            currentSubscriber,
            currentSubscriberATS,
            currentPublisherATS,
            currentTokenStats,
            updatedAtBlock,
            timestamp,
        },
        totalUnits,
        newIndexValue,
        indexTotalUnitsApproved,
        indexTotalUnitsPending
    );

    await validateModifyIDA(
        idaV1,
        updatedIndex,
        currentSubscriber,
        updatedPublisherATS,
        currentSubscriberATS,
        updatedTokenStats,
        token,
        publisher,
        subscriber
    );

    return {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
}
