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
 * - validate HOL/aggregate entities based on the expected data
 *************************************************************************/
import {
    IExpectedFlowOperatorUpdatedEvent,
    IExpectedFlowUpdateEvent,
    IExtraEventData,
    IExtraExpectedData,
    IFlowOperatorUpdatedEvent,
    IFlowUpdatedEvent,
    IGetExpectedIDADataParams as IGetExpectedIDADataParams,
    IIDAEvents,
    ISubscriberDistributionTesterParams,
    ITestModifyFlowData,
    ITestModifyIDAData,
    ITestUpdateFlowOperatorData,
    ITransferEvent,
} from "../interfaces";
import {
    getFlowOperatorUpdatedEvents,
    getFlowUpdatedEvents,
    getTransferEvents,
} from "../queries/eventQueries";
import { fetchEventAndValidate } from "../validation/eventValidators";
import {
    validateFlowUpdated,
    validateModifyIDA,
    validateUpdateFlowOperatorPermissions,
} from "../validation/validators";
import {
    clipDepositNumber,
    fetchEventAndEnsureExistence,
    getFlowOperatorId,
    getIndexId,
    getOrder,
    getSubscriptionId,
    modifyFlowAndReturnCreatedFlowData,
    toBN,
    updateFlowOperatorPermissions,
    waitUntilBlockIndexed,
} from "./helpers";
import {
    getOrInitAccountTokenSnapshot,
    getOrInitializeDataForFlowOperatorUpdated,
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
    FULL_CONTROL,
    IDAEventType,
    idaEventTypeToEventQueryDataMap,
    MAX_FLOW_RATE,
    subscriptionEventTypeToIndexEventType,
} from "./constants";
import { Framework } from "@superfluid-finance/sdk-core";
import { BigNumber } from "@ethersproject/bignumber";
import { BaseProvider, TransactionResponse } from "@ethersproject/providers";
import { ethers } from "hardhat";
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
    const {
        txnResponse,
        timestamp,
        flowRate: newFlowRate,
        deposit,
        logIndex,
    } = await modifyFlowAndReturnCreatedFlowData(data);
    const lastUpdatedAtTimestamp = timestamp.toString();

    // we know blockNumber is not null as we throw an error if it is
    const lastUpdatedBlockNumber = txnResponse.blockNumber!.toString();

    // get or initialize the data
    const initData = getOrInitializeDataForFlowUpdated({
        updatedAtTimestamp: lastUpdatedAtTimestamp,
        updatedAtBlockNumber: lastUpdatedBlockNumber,
        data,
    });
    const newDeposit = clipDepositNumber(newFlowRate.mul(toBN(14400)));
    if (!newDeposit.eq(toBN(deposit))) {
        throw new Error("DEPOSITS ARE NOT EQUAL");
    }
    const depositDelta = newDeposit.sub(toBN(initData.pastStreamData.deposit));
    // update and return updated (expected) data
    const expectedData = await getExpectedDataForFlowUpdated({
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        flowRate: newFlowRate,
        data,
        existingData: initData,
        depositDelta,
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

    const flowUpdatedEvent = await fetchEventAndValidate<
        IFlowUpdatedEvent,
        IExpectedFlowUpdateEvent
    >(
        txnResponse,
        {
            flowRate: newFlowRate.toString(),
            oldFlowRate: initData.pastStreamData.oldFlowRate,
            addresses: [
                data.tokenAddress.toLowerCase(),
                data.sender.toLowerCase(),
                data.receiver.toLowerCase(),
                data.flowOperator.toLowerCase(),
            ],
            sender: data.sender.toLowerCase(),
            receiver: data.receiver.toLowerCase(),
            flowOperator: data.flowOperator.toLowerCase(),
            token: data.tokenAddress.toLowerCase(),
            deposit: newDeposit.toString(),
            totalAmountStreamedUntilTimestamp:
                streamedAmountUntilTimestamp.toString(),
            totalReceiverFlowRate: receiverNetFlow,
            totalSenderFlowRate: senderNetFlow,
            type: data.actionType,
            order: getOrder(txnResponse?.blockNumber, logIndex),
            logIndex: logIndex,
        },
        getFlowUpdatedEvents,
        "FlowUpdatedEvents"
    );

    // if it is a PIC period liquidation
    if (data.liquidator) {
        const transferEvent =
            await fetchEventAndEnsureExistence<ITransferEvent>(
                getTransferEvents,
                txnResponse.hash,
                "TransferEvents"
            );

        expectedData.updatedSenderATS = {
            ...expectedData.updatedSenderATS,
            totalAmountTransferredUntilUpdatedAt: toBN(
                expectedData.updatedSenderATS
                    .totalAmountTransferredUntilUpdatedAt
            )
                .add(toBN(transferEvent.value))
                .toString(),
        };
    }

    await validateFlowUpdated(
        initData.pastStreamData,
        streamedAmountUntilTimestamp,
        newFlowRate,
        expectedData.updatedSenderATS,
        expectedData.updatedReceiverATS,
        expectedData.updatedTokenStats,
        flowUpdatedEvent,
        data.actionType,
        newDeposit.toString()
    );

    let updatedStreamData = getExpectedStreamData(
        initData.pastStreamData,
        data.actionType,
        newFlowRate.toString(),
        newDeposit.toString(),
        lastUpdatedAtTimestamp,
        streamedAmountSinceUpdatedAt
    );

    return {
        revisionIndexId: initData.revisionIndexId,
        updatedStreamData,
        updatedFlowOperator: expectedData.updatedFlowOperator,
        updatedReceiverATS: expectedData.updatedReceiverATS,
        updatedSenderATS: expectedData.updatedSenderATS,
        updatedTokenStats: expectedData.updatedTokenStats,
    };
}

export async function testUpdateFlowOperatorPermissions(
    data: ITestUpdateFlowOperatorData
) {
    let { timestamp, txnResponse, logIndex } =
        await updateFlowOperatorPermissions(data);

    const lastUpdatedAtTimestamp = timestamp.toString();
    const lastUpdatedBlockNumber = txnResponse.blockNumber!.toString();
    const tokenId = data.superToken.address.toLowerCase();
    const senderId = data.sender.toLowerCase();
    const flowOperatorAddress = data.flowOperator.toLowerCase();
    const flowOperatorId = getFlowOperatorId({
        flowOperator: flowOperatorAddress,
        token: data.superToken.address,
        sender: senderId,
    });
    const initData = getOrInitializeDataForFlowOperatorUpdated({
        flowOperatorId,
        flowOperators: data.flowOperators,
        updatedAtBlockNumber: lastUpdatedBlockNumber,
        updatedAtTimestamp: lastUpdatedAtTimestamp,
        token: tokenId,
        accountTokenSnapshots: data.accountTokenSnapshots,
        senderId: senderId,
    });

    const expectedPermissions = data.isFullControl
        ? FULL_CONTROL
        : data.isFullControlRevoke
        ? 0
        : data.permissions;
    const expectedFlowRateAllowance = data.isFullControl
        ? MAX_FLOW_RATE.toString()
        : data.isFullControlRevoke
        ? "0"
        : data.flowRateAllowance;

    const expectedFlowOperatorData = {
        ...initData.flowOperator,
        permissions: expectedPermissions,
        flowRateAllowanceGranted: expectedFlowRateAllowance,
        flowRateAllowanceRemaining: expectedFlowRateAllowance,
    };

    const senderATS = getOrInitAccountTokenSnapshot(
        data.accountTokenSnapshots,
        data.sender,
        data.superToken.address,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    const senderATSFlowOperators = senderATS.flowOperators
        .map((x) => x.id)
        .includes(flowOperatorId)
        ? senderATS.flowOperators
        : [...senderATS.flowOperators, { id: flowOperatorId }];
    const updatedSenderATS = {
        ...senderATS,
        flowOperators: senderATSFlowOperators,
    };

    const flowOperatorUpdatedEvent = await fetchEventAndValidate<
        IFlowOperatorUpdatedEvent,
        IExpectedFlowOperatorUpdatedEvent
    >(
        txnResponse,
        {
            addresses: [tokenId, senderId, flowOperatorAddress.toLowerCase()],
            token: tokenId,
            sender: senderId,
            permissions: expectedPermissions,
            flowRateAllowance: expectedFlowRateAllowance,
            logIndex: logIndex,
            order: getOrder(txnResponse?.blockNumber, logIndex),
        },
        getFlowOperatorUpdatedEvents,
        "FlowOperatorUpdatedEvents"
    );

    await validateUpdateFlowOperatorPermissions({
        event: flowOperatorUpdatedEvent,
        expectedFlowOperator: expectedFlowOperatorData,
        isCreate: data.isCreate,
    });

    return {
        updatedFlowOperatorData: expectedFlowOperatorData,
        updatedSenderATS,
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

    const { accountTokenSnapshots, indexes, subscriptions, tokenStatistics } =
        localData;
    const { token, publisher, indexId, subscriber } = baseParams;
    const { txnResponse, timestamp, updatedAtBlockNumber, logIndex } =
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

    const hasExistingSubscription =
        subscriptions[initData.subscriptionId] != null;

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
        logIndex: logIndex,
        order: getOrder(txnResponse?.blockNumber, logIndex),
    };
    let pendingDistribution: BigNumber = toBN(0);
    if (eventType === IDAEventType.SubscriptionDistributionClaimed) {
        const pendingUnits = await framework.idaV1.getSubscription({
            superToken: data.superToken.address,
            publisher,
            subscriber,
            indexId: indexId.toString(),
            providerOrSigner: provider,
        });
        pendingDistribution = toBN(pendingUnits.pendingDistribution);
    }

    // TODO [FUTURE]: if you want to test an empty distribute (0 units), be wary
    // that no event will be emitted because it will not trigger the code
    // which eimts the event

    // NOTE: when a user does an empty claim, no event is emitted and we have to handle this case
    // throughout
    const isEventEmitted =
        eventType !== IDAEventType.SubscriptionDistributionClaimed ||
        pendingDistribution.gt(toBN(0));
    let events: IIDAEvents = {};

    if (isEventEmitted) {
        events = await fetchIDAEventsAndValidate(
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
        isEventEmitted,
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
    let methodFilter;

    const { token, publisher, indexId, userData, subscriber } = baseParams;
    const baseData = {
        superToken: token,
        publisher,
        indexId: indexId.toString(),
        userData,
    };
    const baseSubscriberData = { ...baseData, subscriber };
    let signer: SignerWithAddress;

    if (type === IDAEventType.IndexCreated) {
        signer = await ethers.getSigner(publisher);
        txnResponse = await ida
            .createIndex({
                ...baseData,
            })
            .exec(signer);
        methodFilter = ida.contract.filters.IndexCreated();
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
        methodFilter = ida.contract.filters.IndexUpdated();
    } else if (type === IDAEventType.SubscriptionApproved) {
        signer = await ethers.getSigner(subscriber);
        txnResponse = await ida
            .approveSubscription({
                ...baseSubscriberData,
            })
            .exec(signer);
        methodFilter = ida.contract.filters.SubscriptionApproved();
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
        methodFilter = ida.contract.filters.SubscriptionRevoked();
    } else if (type === IDAEventType.SubscriptionDistributionClaimed) {
        signer = await ethers.getSigner(subscriber);
        txnResponse = await ida
            .claim({
                ...baseSubscriberData,
            })
            .exec(signer);
        methodFilter = ida.contract.filters.SubscriptionDistributionClaimed();
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
        methodFilter = ida.contract.filters.SubscriptionUnitsUpdated();
    }
    if (!txnResponse.blockNumber) {
        throw new Error("No block number");
    }
    const block = await provider.getBlock(txnResponse.blockNumber);
    await waitUntilBlockIndexed(txnResponse.blockNumber);
    const transactionReceipt = await txnResponse.wait();
    const methodSignature = methodFilter?.topics?.pop();
    const transactionLog = transactionReceipt.logs.find(
        (log) => log.topics[0] === methodSignature
    );
    timestamp = block.timestamp.toString();
    updatedAtBlockNumber = txnResponse.blockNumber.toString();
    return {
        txnResponse,
        timestamp,
        updatedAtBlockNumber,
        logIndex: transactionLog?.logIndex,
    };
}

function getIDAEventDataForValidation(
    type: IDAEventType,
    baseParams: ISubscriberDistributionTesterParams,
    extraEventData: IExtraEventData
) {
    const { token, publisher, indexId, userData, subscriber } = baseParams;

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
        subscription: { id: subscriptionId },
        subscriber: subscriber.toLowerCase(),
        addresses: [...baseAddresses, subscriber.toLowerCase()],
    };
    const baseIndexEventData = {
        ...baseEventData,
        index: { id: indexEntityId },
        subscriber: subscriber.toLowerCase(),
        addresses: [...baseAddresses, subscriber.toLowerCase()],
    };

    if (type === IDAEventType.IndexCreated) {
        return { ...baseEventData, index: { id: indexEntityId } };
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
            index: { id: indexEntityId },
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
        return {
            ...baseIndexEventData,
            units: extraEventData.units.toString(),
        };
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
        const { userData, ...claimEventIndexData } = baseIndexEventData;
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
        const { userData, ...claimEventSubscriptionData } =
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
