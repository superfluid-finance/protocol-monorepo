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
    IExpectedFlowUpdateEvent,
    IExtraEventData,
    IExtraExpectedData,
    IFlowUpdatedEvent,
    IGetExpectedIDADataParams as IGetExpectedIDADataParams,
    IIDAEvents,
    IIndexSubscription,
    ISubscriberDistributionTesterParams,
    ITestModifyFlowData,
    ITestModifyIDAData,
    IUpdateIDAGlobalObjects,
} from "../interfaces";
import { getFlowUpdatedEvents } from "../queries/eventQueries";
import { fetchEventAndValidate } from "../validation/eventValidators";
import {
    validateFlowUpdated,
    validateModifyIDA,
} from "../validation/validators";
import {
    fetchEntityAndEnsureExistence,
    getIndexId,
    getSubscriptionId,
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
    getExpectedDataForIndexCreated,
    getExpectedDataForIndexUpdated,
    getExpectedDataForRevokeOrDeleteSubscription,
    getExpectedDataForSubscriptionApproved,
    getExpectedDataForSubscriptionDistributionClaimed,
    getExpectedDataForSubscriptionUnitsUpdated,
    getExpectedStreamData,
} from "./updaters";
import {
    FlowActionType,
    IDAEventType,
    idaEventTypeToEventQueryDataMap,
    subscriptionEventTypeToIndexEventType,
} from "./constants";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import { BigNumber } from "@ethersproject/bignumber";
import { BaseProvider } from "@ethersproject/providers";
import { getSubscription } from "../queries/holQueries";
import { ethers } from "hardhat";
import { expect } from "chai";

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
        periodRevisionIndexes,
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
        updatedAtTimestamp: lastUpdatedAtTimestamp,
        updatedAtBlockNumber: lastUpdatedBlockNumber,
        sender,
        receiver,
        token: superToken.address,
        accountTokenSnapshots,
        revisionIndexes,
        periodRevisionIndexes,
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
            toBN(pastStreamData.updatedAtTimestamp)
        )
    );

    const senderNetFlow = await cfaV1.getNetFlow(tokenAddress, sender);
    const receiverNetFlow = await cfaV1.getNetFlow(tokenAddress, receiver);
    // validate FlowUpdatedEvent
    const streamedAmountUntilTimestamp = toBN(
        pastStreamData.streamedUntilUpdatedAt
    ).add(streamedAmountSinceUpdatedAt);
    const event = await fetchEventAndValidate<
        IFlowUpdatedEvent,
        IExpectedFlowUpdateEvent
    >(
        receipt,
        {
            flowRate: flowRate.toString(),
            oldFlowRate: pastStreamData.oldFlowRate,
            addresses: [
                tokenAddress.toLowerCase(),
                sender.toLowerCase(),
                receiver.toLowerCase(),
            ],
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
        "FlowUpdatedEvents"
    );

    await validateFlowUpdated(
        pastStreamData,
        streamedAmountUntilTimestamp,
        flowRate,
        tokenId,
        updatedSenderATS,
        updatedReceiverATS,
        updatedTokenStats,
        event,
        actionType
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
        provider,
        atsArray,
    } = data;
    let indexTotalUnitsApproved: BigNumber = toBN(0);
    let indexTotalUnitsPending: BigNumber = toBN(0);
    let newIndexValue: BigNumber = toBN(0);
    let totalUnits: BigNumber = toBN(0);

    const { sf, idaV1, superToken } = contracts;
    const { accountTokenSnapshots, indexes, subscriptions, tokenStatistics } =
        localData;
    const { token, publisher, indexId, subscriber } = baseParams;

    const { receipt, timestamp, updatedAtBlockNumber } =
        await executeIDATransactionByTypeAndWaitForIndexer(
            sf,
            provider,
            eventType,
            baseParams,
            units,
            isRevoke,
            sender,
            isDistribute,
            amountOrIndexValue
        );

    let {
        subscriptionId: subscriberEntityId,
        currentIndex,
        currentSubscription,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
    } = getOrInitializeDataForIDA({
        accountTokenSnapshots,
        indexes,
        indexId: indexId.toString(),
        updatedAtTimestamp: timestamp,
        updatedAtBlockNumber,
        publisher,
        subscriptions,
        subscriber,
        token,
        tokenStatistics,
    });

    const hasExistingSubscription = subscriptions[subscriberEntityId] != null;

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
                ? toBN(currentIndex.indexValue).add(indexDelta)
                : toBN(amountOrIndexValue.toString());
    }

    let distributionDelta = toBN(currentSubscription.units).mul(
        toBN(currentIndex.indexValue).sub(
            toBN(currentSubscription.indexValueUntilUpdatedAt)
        )
    );

    const extraEventData: IExtraEventData = {
        units,
        oldIndexValue: currentIndex.indexValue,
        newIndexValue,
        totalUnitsApproved: indexTotalUnitsApproved,
        totalUnitsPending: indexTotalUnitsPending,
        distributionDelta,
    };

    // Claim is tested quite differently as no event is emitted for it (currently)
    // in the future we can write some logic which handles the event in this block
    // as it will require special logic to accurately do so.
    // NOTE: we are keeping this here too cause right now claim event isn't being
    // mapped.
    if (eventType === IDAEventType.SubscriptionDistributionClaimed) {
        const indexSubscription =
            await fetchEntityAndEnsureExistence<IIndexSubscription>(
                getSubscription,
                currentSubscription.id,
                "Subscription"
            );

        const subscriberAddress = ethers.utils.getAddress(
            indexSubscription.subscriber.id
        );

        const [, , , pendingDistribution] = await idaV1.getSubscription(
            token,
            publisher,
            Number(indexSubscription.index.indexId),
            subscriberAddress
        );
        expect(pendingDistribution.toString()).to.equal("0");

        return {
            updatedIndex: currentIndex,
            updatedSubscription: currentSubscription,
            updatedPublisherATS: currentPublisherATS,
            updatedSubscriberATS: currentSubscriberATS,
            updatedTokenStats: currentTokenStats,
        } as IUpdateIDAGlobalObjects;
    }

    let events: IIDAEvents = await fetchIDAEventsAndValidate(
        eventType,
        receipt,
        baseParams,
        extraEventData
    );

    // handles IndexSubscribed, IndexDistributionClaimed, IndexUnitsUpdated, IndexUnsubscribed
    // which also occur on the three events below, respectively
    if (
        [
            IDAEventType.SubscriptionApproved,
            IDAEventType.SubscriptionUnitsUpdated,
            IDAEventType.SubscriptionRevoked,
            IDAEventType.SubscriptionDistributionClaimed,
        ].includes(eventType)
    ) {
        const otherEventType =
            subscriptionEventTypeToIndexEventType.get(eventType);
        if (!otherEventType) {
            throw new Error("Incorrect event type.");
        }

        events = {
            ...events,
            ...(await fetchIDAEventsAndValidate(
                otherEventType,
                receipt,
                baseParams,
                extraEventData
            )),
        };
    }

    const expectedDataParams = {
        token: superToken,
        currentIndex,
        currentSubscription,
        atsArray,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
        updatedAtBlockNumber,
        timestamp,
    };

    const extraData: IExtraExpectedData = {
        ...extraEventData,
        isRevoke,
        totalUnits,
    };
    const {
        updatedIndex,
        updatedPublisherATS,
        updatedSubscription,
        updatedSubscriberATS,
        updatedTokenStats,
    } = await getExpectedDataForIDA(eventType, expectedDataParams, extraData);

    await validateModifyIDA(
        idaV1,
        updatedIndex,
        updatedSubscription,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
        token,
        publisher,
        subscriber,
        eventType,
        events,
        hasExistingSubscription
    );

    return {
        updatedIndex,
        updatedSubscription,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    } as IUpdateIDAGlobalObjects;
}

async function executeIDATransactionByTypeAndWaitForIndexer(
    sf: Framework,
    provider: BaseProvider,
    type: IDAEventType,
    baseParams: ISubscriberDistributionTesterParams,
    units?: BN,
    isRevoke?: boolean,
    sender?: string,
    isDistribute?: boolean,
    amountOrIndexValue?: BN
) {
    let timestamp: string = "";
    let updatedAtBlockNumber: string = "";
    let receipt;
    let txn: any;
    const ida = sf.ida as InstantDistributionAgreementV1Helper;

    const { token, publisher, indexId, userData, subscriber } = baseParams;
    const baseData = {
        superToken: token,
        publisher,
        indexId,
        userData,
        onTransaction: () => {},
    };
    const baseSubscriberData = { ...baseData, subscriber };

    if (type === IDAEventType.IndexCreated) {
        txn = await ida.createIndex({
            ...baseData,
        });
    } else if (type === IDAEventType.IndexUpdated) {
        if (amountOrIndexValue == null || isDistribute == null) {
            throw new Error(
                "You must pass isDistribute and amountOrIndexValue for index updated."
            );
        }

        if (isDistribute) {
            txn = await ida.distribute({
                ...baseData,
                amount: amountOrIndexValue.toString(),
            });
        } else {
            txn = await ida.updateIndex({
                ...baseData,
                indexValue: amountOrIndexValue.toString(),
            });
        }
    } else if (type === IDAEventType.SubscriptionApproved) {
        txn = await ida.approveSubscription({
            ...baseSubscriberData,
        });
    } else if (type === IDAEventType.SubscriptionRevoked) {
        if (isRevoke == null || sender == null) {
            throw new Error(
                "You must pass isRevoke and sender for subscription revoked."
            );
        }

        if (isRevoke) {
            txn = await ida.revokeSubscription({
                ...baseSubscriberData,
            });
        } else {
            txn = await ida.deleteSubscription({
                ...baseSubscriberData,
                sender,
            });
        }
    } else if (type === IDAEventType.SubscriptionDistributionClaimed) {
        txn = await ida.claim({
            ...baseSubscriberData,
            sender: subscriber,
        });
    } else {
        // type === IDAEventType.SubscriptionUnitsUpdated
        if (units == null) {
            throw new Error(
                "You must pass units for SubscriptionUnitsUpdated."
            );
        }

        txn = await ida.updateSubscription({
            ...baseSubscriberData,
            units: units.toString(),
        });
    }

    receipt = txn.receipt;

    const block = await provider.getBlock(receipt.blockNumber);
    await waitUntilBlockIndexed(receipt.blockNumber);

    timestamp = block.timestamp.toString();
    updatedAtBlockNumber = receipt.blockNumber.toString();

    return { receipt, timestamp, updatedAtBlockNumber };
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
        distributionDelta,
    } = extraEventData;

    const subscriptionId = getSubscriptionId(
        subscriber,
        publisher,
        token,
        indexId.toString()
    );
    const indexEntityId = getIndexId(publisher, token, indexId.toString());
    const baseAddresses = [token.toLowerCase(), publisher.toLowerCase()];
    const baseEventData = {
        token: token.toLowerCase(),
        publisher: publisher.toLowerCase(),
        indexId: indexId.toString(),
        userData,
        addresses: baseAddresses
    };
    const baseSubscriptionEventData = {
        ...baseEventData,
        subscription: { id: subscriptionId },
        subscriber: subscriber.toLowerCase(),
        addresses: [...baseAddresses, subscriber.toLowerCase()]
    };
    const baseIndexEventData = {
        ...baseEventData,
        index: { id: indexEntityId },
        subscriber: subscriber.toLowerCase(),
        addresses: [...baseAddresses, subscriber.toLowerCase()]
    };

    if (type === IDAEventType.IndexCreated) {
        return { ...baseEventData, index: { id: indexEntityId } };
    } else if (type === IDAEventType.IndexUpdated) {
        if (
            newIndexValue == null ||
            totalUnitsApproved == null ||
            totalUnitsPending == null
        ) {
            throw new Error(
                "newIndexValue, totalUnitsApproved and totalUnitsPending are required for IndexUpdated"
            );
        }
        return {
            ...baseEventData,
            index: { id: indexEntityId },
            oldIndexValue,
            newIndexValue: newIndexValue.toString(),
            totalUnitsApproved: totalUnitsApproved.toString(),
            totalUnitsPending: totalUnitsPending.toString(),
        };
    } else if (
        [IDAEventType.IndexSubscribed, IDAEventType.IndexUnsubscribed].includes(
            type
        )
    ) {
        return baseIndexEventData;
    } else if (type === IDAEventType.IndexUnitsUpdated) {
        if (units == null) {
            throw new Error("You must pass units for IndexUnitsUpdated");
        }
        return { ...baseIndexEventData, units: units.toString() };
    } else if (
        [
            IDAEventType.SubscriptionApproved,
            IDAEventType.SubscriptionRevoked,
        ].includes(type)
    ) {
        return baseSubscriptionEventData;
    } else if (type === IDAEventType.IndexDistributionClaimed) {
        if (distributionDelta == null) {
            throw new Error(
                "distributionDelta is required for IndexDistributionClaimed"
            );
        }
        const { userData, ...claimEventIndexData } = baseIndexEventData;
        return {
            ...claimEventIndexData,
            distributionDelta: distributionDelta.toString(),
        };
    } else if (type === IDAEventType.SubscriptionDistributionClaimed) {
        if (distributionDelta == null) {
            throw new Error(
                "distributionDelta is required for SubscriptionDistributionClaimed"
            );
        }
        const { userData, ...claimEventSubscriptionData } =
            baseSubscriptionEventData;
        return {
            ...claimEventSubscriptionData,
            distributionDelta: distributionDelta.toString(),
        };
    } else {
        // type === IDAEventType.SubscriptionUnitsUpdated
        if (units == null) {
            throw new Error("You must pass units for SubscriptionUnitsUpdated");
        }
        return {
            ...baseSubscriptionEventData,
            units: units.toString(),
        };
    }
}

async function fetchIDAEventsAndValidate(
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

    return {
        [eventQueryData.queryName]: await fetchEventAndValidate(
            receipt,
            eventDataToValidate,
            eventQueryData.query,
            eventQueryData.queryName
        ),
    };
}

async function getExpectedDataForIDA(
    type: IDAEventType,
    expectedDataParams: IGetExpectedIDADataParams,
    extraData: IExtraExpectedData
) {
    const {
        isRevoke,
        newIndexValue,
        totalUnits,
        totalUnitsApproved,
        totalUnitsPending,
        units,
    } = extraData;

    if (type === IDAEventType.IndexCreated) {
        return getExpectedDataForIndexCreated(expectedDataParams);
    } else if (type === IDAEventType.IndexUpdated) {
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
        return await getExpectedDataForSubscriptionApproved(expectedDataParams);
    } else if (type === IDAEventType.SubscriptionDistributionClaimed) {
        return await getExpectedDataForSubscriptionDistributionClaimed(
            expectedDataParams
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

    if (units == null) {
        throw new Error("units is required for SubscriptionUnitsUpdated");
    }
    // type === IDAEventType.SubscriptionUnitsUpdated
    return await getExpectedDataForSubscriptionUnitsUpdated(
        expectedDataParams,
        units
    );
}
