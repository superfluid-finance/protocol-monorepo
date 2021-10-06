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
    IExtraEventData,
    IExtraExpectedData,
    IFlowUpdated,
    IGetExpectedIDADataParams as IGetExpectedIDADataParams,
    IIndexCreated,
    ISubscriberDistributionTesterParams,
    ITestModifyFlowData,
    ITestModifyIDAData,
} from "../interfaces";
import {
    getFlowUpdatedEvents,
    getIndexCreatedEvents,
} from "../queries/eventQueries";
import { fetchEventAndValidate } from "../validation/eventValidators";
import {
    validateFlowUpdated,
    validateModifyIDA,
} from "../validation/validators";
import {
    getSubscriberId,
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
import {
    FlowActionType,
    IDAEventType,
    idaEventTypeToEventQueryDataMap,
} from "./constants";
import { fetchIndexAndValidate } from "../validation/holValidators";
import { fetchTokenStatsAndValidate } from "../validation/aggregateValidators";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import { BigNumber } from "@ethersproject/bignumber";

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

/**
  * A "God" function used to test IDA events.
 * It handles all the IDA actions and validates that the
 * data on the subgraph is as expected and compares it
 * with web3 data where possible.
 * It also returns the updated (expected) data to be
 * used in future tests.
 * @param data 
 * @returns 
 */
export async function testModifyIDA(data: ITestModifyIDAData) {
    const {
        baseParams,
        contracts,
        eventType,
        localData,
        units,
        isRevoke,
        sender,
        isDistribute,
        amountOrIndexValue,
    } = data;
    let indexTotalUnitsApproved: BigNumber = toBN(0);
    let indexTotalUnitsPending: BigNumber = toBN(0);
    let newIndexValue: BigNumber = toBN(0);
    let totalUnits: BigNumber = toBN(0);

    const { sf, idaV1, superToken } = contracts;
    const { accountTokenSnapshots, indexes, subscribers, tokenStatistics } =
        localData;
    const { token, publisher, indexId, atsArray, subscriber } = baseParams;

    const { receipt, timestamp, updatedAtBlock } =
        await executeIDATransactionByTypeAndWaitForIndexer(
            sf,
            eventType,
            baseParams,
            units,
            isRevoke,
            sender,
            isDistribute,
            amountOrIndexValue
        );

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

    const subscriptionExists = hasSubscription(subscribers, subscriberEntityId);

    if (eventType === IDAEventType.IndexUpdated) {
        if (amountOrIndexValue == null) {
            throw new Error(
                "amountOrIndexValue must be passed for IndexUpdated."
            );
        }
        [, , indexTotalUnitsApproved, indexTotalUnitsPending] =
            await idaV1.getIndex(token, publisher, indexId);
        totalUnits = toBN(currentIndex.totalUnitsApproved).add(
            toBN(currentIndex.totalUnitsPending)
        );

        const indexDelta = totalUnits.gt(toBN(0))
            ? toBN(amountOrIndexValue.toString()).div(totalUnits)
            : toBN(0);
        newIndexValue =
            isDistribute === true
                ? toBN(currentIndex.newIndexValue).add(indexDelta)
                : toBN(amountOrIndexValue.toString());
    }

    const extraEventData: IExtraEventData = {
        units,
        oldIndexValue: currentIndex.newIndexValue,
        newIndexValue,
        totalUnitsApproved: indexTotalUnitsApproved,
        totalUnitsPending: indexTotalUnitsPending,
    };

    await fetchIDAEventAndValidate(
        eventType,
        receipt,
        baseParams,
        extraEventData
    );

    const expectedDataParams = {
        token: superToken,
        currentIndex,
        currentSubscriber,
        atsArray,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlock,
        timestamp,
    };

    const extraData: IExtraExpectedData = {
        ...extraEventData,
        isRevoke,
        subscriptionExists,
        totalUnits,
    };
    const {
        updatedIndex,
        updatedPublisherATS,
        updatedSubscriber,
        updatedSubscriberATS,
        updatedTokenStats,
    } = await getExpectedDataForIDA(eventType, expectedDataParams, extraData);

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

async function executeIDATransactionByTypeAndWaitForIndexer(
    sf: Framework,
    type: IDAEventType,
    baseParams: ISubscriberDistributionTesterParams,
    units?: BN,
    isRevoke?: boolean,
    sender?: string,
    isDistribute?: boolean,
    amountOrIndexValue?: BN
) {
    let timestamp: string = "";
    let updatedAtBlock: string = "";
    let receipt;
    const { provider, token, publisher, indexId, userData, subscriber } =
        baseParams;
    const baseData = {
        superToken: token,
        publisher,
        indexId,
        userData,
        onTransaction: () => {},
    };
    if (type === IDAEventType.SubscriptionApproved) {
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

		// TODO: just do this once at the end to save a few more lines
        receipt = txn.receipt;
    } else if (type === IDAEventType.SubscriptionUnitsUpdated) {
        if (!units) {
            throw new Error(
                "You must pass units when updating subscription units."
            );
        }

        const txn: any = await (
            sf.ida as InstantDistributionAgreementV1Helper
        ).updateSubscription({
            ...baseData,
            subscriber,
            units,
        });

        receipt = txn.receipt;
    } else if (type === IDAEventType.SubscriptionRevoked) {
        if (isRevoke == null || sender == null) {
            throw new Error(
                "You must pass isRevoke and sender for subscription revoked."
            );
        }

        let txn: any;
        if (isRevoke) {
            txn = await (
                sf.ida as InstantDistributionAgreementV1Helper
            ).revokeSubscription({
                ...baseData,
                subscriber,
            });
        } else {
            txn = await (
                sf.ida as InstantDistributionAgreementV1Helper
            ).deleteSubscription({
                ...baseData,
                sender,
                subscriber,
            });
        }

        receipt = txn.receipt;
    } else {
        // type === IDAEventType.IndexUpdated
        if (amountOrIndexValue == null || isDistribute == null) {
            throw new Error(
                "You must pass isDistribute and amountOrIndexValue for index updated."
            );
        }

        let txn: any;
        if (isDistribute) {
            txn = await (
                sf.ida as InstantDistributionAgreementV1Helper
            ).distribute({
                ...baseData,
                amount: amountOrIndexValue,
            });
        } else {
            txn = await (
                sf.ida as InstantDistributionAgreementV1Helper
            ).updateIndex({
                ...baseData,
                indexValue: amountOrIndexValue,
            });
        }

        receipt = txn.receipt;
    }
    const block = await provider.getBlock(receipt.blockNumber);
    await waitUntilBlockIndexed(receipt.blockNumber);

    timestamp = block.timestamp.toString();
    updatedAtBlock = receipt.blockNumber.toString();

    return { receipt, timestamp, updatedAtBlock };
}

function getIDAEventDataForValidation(
    type: IDAEventType,
    baseParams: ISubscriberDistributionTesterParams,
    extraEventData: IExtraEventData
) {
    const { token, publisher, indexId, userData, subscriber } = baseParams;
    const {
        totalUnitsApproved,
        totalUnitsPending,
        newIndexValue,
        oldIndexValue,
        units,
    } = extraEventData;

    const subscriberEntityId = getSubscriberId(
        subscriber,
        publisher,
        token,
        indexId.toString()
    );

    if (type === IDAEventType.SubscriptionApproved) {
        return {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            subscriber: { id: subscriberEntityId },
            userData,
        };
    } else if (type === IDAEventType.SubscriptionRevoked) {
        return {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            subscriber: { id: subscriberEntityId },
            userData,
        };
    } else if (type === IDAEventType.SubscriptionUnitsUpdated) {
        if (units == null) {
            throw new Error("You must pass units for SubscriptionUnitsUpdated");
        }
        return {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            subscriber: { id: subscriberEntityId },
            units: units.toString(),
            userData,
        };
    } else {
        if (newIndexValue == null) {
            throw new Error("You must pass newIndexValue for IndexUpdated");
        }
        if (totalUnitsApproved == null) {
            throw new Error(
                "You must pass totalUnitsApproved for IndexUpdated"
            );
        }
        if (totalUnitsPending == null) {
            throw new Error("You must pass totalUnitsPending for IndexUpdated");
        }
        return {
            token: token.toLowerCase(),
            publisher: publisher.toLowerCase(),
            indexId: indexId.toString(),
            oldIndexValue,
            newIndexValue: newIndexValue.toString(),
            totalUnitsApproved: totalUnitsApproved.toString(),
            totalUnitsPending: totalUnitsPending.toString(),
            userData,
        };
    }
}

async function fetchIDAEventAndValidate(
    type: IDAEventType,
    receipt: ContractReceipt,
    baseParams: ISubscriberDistributionTesterParams,
    extraEventData: IExtraEventData
) {
    const eventQueryData = idaEventTypeToEventQueryDataMap.get(type);
    const eventDataToValidate = getIDAEventDataForValidation(
        type,
        baseParams,
        extraEventData
    );
    if (eventQueryData == null) {
        throw new Error("You have entered the wrong type.");
    }

    await fetchEventAndValidate(
        receipt,
        eventDataToValidate,
        eventQueryData.query,
        eventQueryData.queryResultName,
        eventQueryData.queryName
    );
}

async function getExpectedDataForIDA(
    type: IDAEventType,
    expectedDataParams: IGetExpectedIDADataParams,
    extraData: IExtraExpectedData
) {
    const {
        isRevoke,
        newIndexValue,
        subscriptionExists,
        totalUnits,
        totalUnitsApproved,
        totalUnitsPending,
        units,
    } = extraData;
    if (type === IDAEventType.IndexUpdated) {
        if (
            totalUnits == null ||
            newIndexValue == null ||
            totalUnitsApproved == null ||
            totalUnitsPending == null
        ) {
            throw new Error(
                "totalUnits, newIndexValue, totalUnitsApproved, totalUnitsPending are required for IndexUpdated"
            );
        }
        return await getExpectedDataForIndexUpdated(
            expectedDataParams,
            totalUnits,
            newIndexValue,
            totalUnitsApproved,
            totalUnitsPending
        );
    } else if (type === IDAEventType.SubscriptionApproved) {
        if (subscriptionExists == null) {
            throw new Error(
                "subscriptionExists is required for SubscriptionApproved"
            );
        }
        return await getExpectedDataForSubscriptionApproved(
            expectedDataParams,
            subscriptionExists
        );
    } else if (type === IDAEventType.SubscriptionRevoked) {
        if (isRevoke == null) {
            throw new Error("isRevoke is required for SubscriptionRevoked");
        }
        return await getExpectedDataForRevokeOrDeleteSubscription(
            expectedDataParams,
            isRevoke
        );
    }

    if (units == null || subscriptionExists == null) {
        throw new Error(
            "units, subscriptionExists is required for SubscriptionUnitsUpdated"
        );
    }
    // type === IDAEventType.SubscriptionUnitsUpdated
    return await getExpectedDataForSubscriptionUnitsUpdated(
        expectedDataParams,
        units,
        subscriptionExists
    );
}
