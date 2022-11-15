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
} from "../interfaces";
import {getFlowUpdatedEvents} from "../queries/eventQueries";
import {fetchEventAndValidate} from "../validation/eventValidators";
import {validateFlowUpdated, validateModifyIDA} from "../validation/validators";
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
    IDAEventType,
    idaEventTypeToEventQueryDataMap,
    subscriptionEventTypeToIndexEventType,
} from "./constants";
import {Framework} from "@superfluid-finance/sdk-core";
import {BigNumber} from "@ethersproject/bignumber";
import {BaseProvider, TransactionResponse} from "@ethersproject/providers";
import {getSubscription} from "../queries/holQueries";
import {ethers} from "hardhat";
import {expect} from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";

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
    // create/update/delete a flow
    const {txnResponse, timestamp, flowRate} =
        await modifyFlowAndReturnCreatedFlowData(
            data.provider,
            data.framework,
            data.actionType,
            data.superToken.address,
            data.sender,
            data.receiver,
            monthlyToSecondRate(data.newFlowRate)
        );
    const lastUpdatedAtTimestamp = timestamp.toString();

    // we know blockNumber is not null as we throw an error if it is
    const lastUpdatedBlockNumber = txnResponse.blockNumber!.toString();
    const tokenId = data.tokenAddress.toLowerCase();

    // get or initialize the data
    const initData = getOrInitializeDataForFlowUpdated({
        updatedAtTimestamp: lastUpdatedAtTimestamp,
        updatedAtBlockNumber: lastUpdatedBlockNumber,
        sender: data.sender,
        receiver: data.receiver,
        token: data.superToken.address,
        accountTokenSnapshots: data.localData.accountTokenSnapshots,
        revisionIndexes: data.localData.revisionIndexes,
        periodRevisionIndexes: data.localData.periodRevisionIndexes,
        streamData: data.localData.streamData,
        tokenStatistics: data.localData.tokenStatistics,
    });

    // update and return updated (expected) data
    const expectedData = await getExpectedDataForFlowUpdated({
        actionType: data.actionType,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        accountTokenSnapshots: data.atsArray,
        flowRate,
        superToken: data.superToken,
        pastStreamData: initData.pastStreamData,
        currentSenderATS: initData.currentSenderATS,
        currentReceiverATS: initData.currentReceiverATS,
        currentTokenStats: initData.currentTokenStats,
        provider: data.provider,
    });

    const streamedAmountSinceUpdatedAt = toBN(
        initData.pastStreamData.oldFlowRate
    ).mul(
        toBN(lastUpdatedAtTimestamp).sub(
            toBN(initData.pastStreamData.updatedAtTimestamp)
        )
    );

    const senderNetFlow = await data.framework.cfaV1.getNetFlow({
        superToken: data.tokenAddress,
        account: data.sender,
        providerOrSigner: data.provider,
    });
    const receiverNetFlow = await data.framework.cfaV1.getNetFlow({
        superToken: data.tokenAddress,
        account: data.receiver,
        providerOrSigner: data.provider,
    });
    // validate FlowUpdatedEvent
    const streamedAmountUntilTimestamp = toBN(
        initData.pastStreamData.streamedUntilUpdatedAt
    ).add(streamedAmountSinceUpdatedAt);
    const event = await fetchEventAndValidate<
        IFlowUpdatedEvent,
        IExpectedFlowUpdateEvent
    >(
        txnResponse,
        {
            flowRate: flowRate.toString(),
            oldFlowRate: initData.pastStreamData.oldFlowRate,
            addresses: [
                data.tokenAddress.toLowerCase(),
                data.sender.toLowerCase(),
                data.receiver.toLowerCase(),
            ],
            sender: data.sender.toLowerCase(),
            receiver: data.receiver.toLowerCase(),
            token: data.tokenAddress.toLowerCase(),
            totalAmountStreamedUntilTimestamp:
                streamedAmountUntilTimestamp.toString(),
            totalReceiverFlowRate: receiverNetFlow,
            totalSenderFlowRate: senderNetFlow,
            type: data.actionType,
        },
        getFlowUpdatedEvents,
        "FlowUpdatedEvents"
    );

    await validateFlowUpdated(
        initData.pastStreamData,
        streamedAmountUntilTimestamp,
        flowRate,
        tokenId,
        expectedData.updatedSenderATS,
        expectedData.updatedReceiverATS,
        expectedData.updatedTokenStats,
        event,
        data.actionType
    );

    let updatedStreamData = getExpectedStreamData(
        initData.pastStreamData,
        data.actionType,
        flowRate.toString(),
        lastUpdatedAtTimestamp,
        streamedAmountSinceUpdatedAt
    );

    return {
        revisionIndexId: initData.revisionIndexId,
        updatedStreamData,
        updatedReceiverATS: expectedData.updatedReceiverATS,
        updatedSenderATS: expectedData.updatedSenderATS,
        updatedTokenStats: expectedData.updatedTokenStats,
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
        framework,
        superToken,
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
    let indexTotalUnitsApproved = toBN(0);
    let indexTotalUnitsPending = toBN(0);
    let newIndexValue = toBN(0);
    let totalUnits = toBN(0);

    const {accountTokenSnapshots, indexes, subscriptions, tokenStatistics} =
        localData;
    const {token, publisher, indexId, subscriber} = baseParams;
    const {txnResponse, timestamp, updatedAtBlockNumber} =
        await executeIDATransactionByTypeAndWaitForIndexer(
            framework,
            provider,
            eventType,
            baseParams,
            units,
            isRevoke,
            sender,
            isDistribute,
            amountOrIndexValue
        );

    let initData = getOrInitializeDataForIDA({
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

    const hasExistingSubscription = subscriptions[initData.subscriptionId] != null;

    if (eventType === IDAEventType.IndexUpdated) {
        if (amountOrIndexValue == null) {
            throw new Error(
                "amountOrIndexValue must be passed for IndexUpdated."
            );
        }
        const index = await framework.idaV1.getIndex({
            superToken: token,
            publisher,
            indexId: indexId.toString(),
            providerOrSigner: provider,
        });
        indexTotalUnitsApproved = toBN(index.totalUnitsApproved);
        indexTotalUnitsPending = toBN(index.totalUnitsPending);
        totalUnits = toBN(initData.currentIndex.totalUnitsApproved).add(
            toBN(initData.currentIndex.totalUnitsPending)
        );

        const indexDelta = totalUnits.gt(toBN(0))
            ? toBN(amountOrIndexValue.toString()).div(totalUnits)
            : toBN(0);
        newIndexValue =
            isDistribute === true
                ? toBN(initData.currentIndex.indexValue).add(indexDelta)
                : toBN(amountOrIndexValue.toString());
    }

    let distributionDelta = toBN(initData.currentSubscription.units).mul(
        toBN(initData.currentIndex.indexValue).sub(
            toBN(initData.currentSubscription.indexValueUntilUpdatedAt)
        )
    );

    const extraEventData: IExtraEventData = {
        units,
        oldIndexValue: initData.currentIndex.indexValue,
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
                initData.currentSubscription.id,
                "Subscription"
            );

        const subscriberAddress = ethers.utils.getAddress(
            indexSubscription.subscriber.id
        );

        const subscription = await framework.idaV1.getSubscription({
            superToken: token,
            publisher,
            indexId: indexSubscription.index.indexId,
            subscriber: subscriberAddress,
            providerOrSigner: provider,
        });

        expect(subscription.pendingDistribution).to.equal("0");

        return {
            updatedIndex: initData.currentIndex,
            updatedSubscription: initData.currentSubscription,
            updatedPublisherATS: initData.currentPublisherATS,
            updatedSubscriberATS: initData.currentSubscriberATS,
            updatedTokenStats: initData.currentTokenStats,
        };
    }

    let events: IIDAEvents = await fetchIDAEventsAndValidate(
        eventType,
        txnResponse,
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
                txnResponse,
                baseParams,
                extraEventData
            )),
        };
    }

    const expectedDataParams = {
        token: superToken,
        currentIndex: initData.currentIndex,
        currentSubscription: initData.currentSubscription,
        atsArray,
        currentPublisherATS: initData.currentPublisherATS,
        currentSubscriberATS: initData.currentSubscriberATS,
        currentTokenStats: initData.currentTokenStats,
        updatedAtBlockNumber,
        timestamp,
        provider,
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
        framework,
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
    };
}

async function executeIDATransactionByTypeAndWaitForIndexer(
    sf: Framework,
    provider: BaseProvider,
    type: IDAEventType,
    baseParams: ISubscriberDistributionTesterParams,
    units?: BigNumber,
    isRevoke?: boolean,
    sender?: string,
    isDistribute?: boolean,
    amountOrIndexValue?: BigNumber
) {
    let timestamp: string = "";
    let updatedAtBlockNumber: string = "";
    let txnResponse: TransactionResponse;
    const ida = sf.idaV1;

    const {token, publisher, indexId, userData, subscriber} = baseParams;
    const baseData = {
        superToken: token,
        publisher,
        indexId: indexId.toString(),
        userData,
    };
    const baseSubscriberData = {...baseData, subscriber};
    let signer: SignerWithAddress;

    if (type === IDAEventType.IndexCreated) {
        signer = await ethers.getSigner(publisher);
        txnResponse = await ida
            .createIndex({
                ...baseData,
            })
            .exec(signer);
    } else if (type === IDAEventType.IndexUpdated) {
        if (amountOrIndexValue == null || isDistribute == null) {
            throw new Error(
                "You must pass isDistribute and amountOrIndexValue for index updated."
            );
        }

        signer = await ethers.getSigner(publisher);

        if (isDistribute) {
            txnResponse = await ida
                .distribute({
                    ...baseData,
                    amount: amountOrIndexValue.toString(),
                })
                .exec(signer);
        } else {
            txnResponse = await ida
                .updateIndexValue({
                    ...baseData,
                    indexValue: amountOrIndexValue.toString(),
                })
                .exec(signer);
        }
    } else if (type === IDAEventType.SubscriptionApproved) {
        signer = await ethers.getSigner(subscriber);
        txnResponse = await ida
            .approveSubscription({
                ...baseSubscriberData,
            })
            .exec(signer);
    } else if (type === IDAEventType.SubscriptionRevoked) {
        if (isRevoke == null || sender == null) {
            throw new Error(
                "You must pass isRevoke and sender for subscription revoked."
            );
        }

        if (isRevoke) {
            signer = await ethers.getSigner(subscriber);
            txnResponse = await ida
                .revokeSubscription({
                    ...baseSubscriberData,
                })
                .exec(signer);
        } else {
            signer = await ethers.getSigner(sender);
            txnResponse = await ida
                .deleteSubscription({
                    ...baseSubscriberData,
                })
                .exec(signer);
        }
    } else if (type === IDAEventType.SubscriptionDistributionClaimed) {
        signer = await ethers.getSigner(subscriber);
        txnResponse = await ida
            .claim({
                ...baseSubscriberData,
            })
            .exec(signer);
    } else {
        // type === IDAEventType.SubscriptionUnitsUpdated
        if (units == null) {
            throw new Error(
                "You must pass units for SubscriptionUnitsUpdated."
            );
        }
        signer = await ethers.getSigner(publisher);

        txnResponse = await ida
            .updateSubscriptionUnits({
                ...baseSubscriberData,
                units: units.toString(),
            })
            .exec(signer);
    }

    if (!txnResponse.blockNumber) {
        throw new Error("No block number");
    }

    const block = await provider.getBlock(txnResponse.blockNumber);
    await waitUntilBlockIndexed(txnResponse.blockNumber);

    timestamp = block.timestamp.toString();
    updatedAtBlockNumber = txnResponse.blockNumber.toString();

    return {txnResponse, timestamp, updatedAtBlockNumber};
}

function getIDAEventDataForValidation(
    type: IDAEventType,
    baseParams: ISubscriberDistributionTesterParams,
    extraEventData: IExtraEventData
) {
    const {token, publisher, indexId, userData, subscriber} = baseParams;

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
        addresses: baseAddresses,
    };
    const baseSubscriptionEventData = {
        ...baseEventData,
        subscription: {id: subscriptionId},
        subscriber: subscriber.toLowerCase(),
        addresses: [...baseAddresses, subscriber.toLowerCase()],
    };
    const baseIndexEventData = {
        ...baseEventData,
        index: {id: indexEntityId},
        subscriber: subscriber.toLowerCase(),
        addresses: [...baseAddresses, subscriber.toLowerCase()],
    };

    if (type === IDAEventType.IndexCreated) {
        return {...baseEventData, index: {id: indexEntityId}};
    } else if (type === IDAEventType.IndexUpdated) {
        if (
            extraEventData.newIndexValue == null ||
            extraEventData.totalUnitsApproved == null ||
            extraEventData.totalUnitsPending == null
        ) {
            throw new Error(
                "newIndexValue, totalUnitsApproved and totalUnitsPending are required for IndexUpdated"
            );
        }
        return {
            ...baseEventData,
            index: {id: indexEntityId},
            oldIndexValue: extraEventData.oldIndexValue,
            newIndexValue: extraEventData.newIndexValue.toString(),
            totalUnitsApproved: extraEventData.totalUnitsApproved.toString(),
            totalUnitsPending: extraEventData.totalUnitsPending.toString(),
        };
    } else if (
        [IDAEventType.IndexSubscribed, IDAEventType.IndexUnsubscribed].includes(
            type
        )
    ) {
        return baseIndexEventData;
    } else if (type === IDAEventType.IndexUnitsUpdated) {
        if (extraEventData.units == null) {
            throw new Error("You must pass units for IndexUnitsUpdated");
        }
        return {...baseIndexEventData, units: extraEventData.units.toString()};
    } else if (
        [
            IDAEventType.SubscriptionApproved,
            IDAEventType.SubscriptionRevoked,
        ].includes(type)
    ) {
        return baseSubscriptionEventData;
    } else if (type === IDAEventType.IndexDistributionClaimed) {
        if (extraEventData.distributionDelta == null) {
            throw new Error(
                "distributionDelta is required for IndexDistributionClaimed"
            );
        }
        const {userData, ...claimEventIndexData} = baseIndexEventData;
        return {
            ...claimEventIndexData,
            distributionDelta: extraEventData.distributionDelta.toString(),
        };
    } else if (type === IDAEventType.SubscriptionDistributionClaimed) {
        if (extraEventData.distributionDelta == null) {
            throw new Error(
                "distributionDelta is required for SubscriptionDistributionClaimed"
            );
        }
        const {userData, ...claimEventSubscriptionData} =
            baseSubscriptionEventData;
        return {
            ...claimEventSubscriptionData,
            distributionDelta: extraEventData.distributionDelta.toString(),
        };
    } else {
        // type === IDAEventType.SubscriptionUnitsUpdated
        if (extraEventData.units == null) {
            throw new Error("You must pass units for SubscriptionUnitsUpdated");
        }
        return {
            ...baseSubscriptionEventData,
            units: extraEventData.units.toString(),
        };
    }
}

async function fetchIDAEventsAndValidate(
    type: IDAEventType,
    txnResponse: TransactionResponse,
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
            txnResponse,
            eventDataToValidate,
            eventQueryData.query,
            eventQueryData.queryName
        ),
    };
}

async function getExpectedDataForIDA(
    type: IDAEventType,
    expectedDataParams: IGetExpectedIDADataParams,
    extraExpectedData: IExtraExpectedData
) {
    if (type === IDAEventType.IndexCreated) {
        return getExpectedDataForIndexCreated(expectedDataParams);
    } else if (type === IDAEventType.IndexUpdated) {
        if (
            extraExpectedData.totalUnits == null ||
            extraExpectedData.newIndexValue == null ||
            extraExpectedData.totalUnitsApproved == null ||
            extraExpectedData.totalUnitsPending == null
        ) {
            throw new Error(
                "totalUnits, newIndexValue, totalUnitsApproved, totalUnitsPending are required for IndexUpdated"
            );
        }
        return await getExpectedDataForIndexUpdated(
            expectedDataParams,
            extraExpectedData.totalUnits,
            extraExpectedData.newIndexValue,
            extraExpectedData.totalUnitsApproved,
            extraExpectedData.totalUnitsPending
        );
    } else if (type === IDAEventType.SubscriptionApproved) {
        return await getExpectedDataForSubscriptionApproved(expectedDataParams);
    } else if (type === IDAEventType.SubscriptionDistributionClaimed) {
        return await getExpectedDataForSubscriptionDistributionClaimed(
            expectedDataParams
        );
    } else if (type === IDAEventType.SubscriptionRevoked) {
        if (extraExpectedData.isRevoke == null) {
            throw new Error("isRevoke is required for SubscriptionRevoked");
        }
        return await getExpectedDataForRevokeOrDeleteSubscription(
            expectedDataParams,
            extraExpectedData.isRevoke
        );
    }

    if (extraExpectedData.units == null) {
        throw new Error("units is required for SubscriptionUnitsUpdated");
    }
    // type === IDAEventType.SubscriptionUnitsUpdated
    return await getExpectedDataForSubscriptionUnitsUpdated(
        expectedDataParams,
        extraExpectedData.units
    );
}
