import { BigNumber } from "@ethersproject/bignumber";
import { expect } from "chai";
import { ethers } from "ethers";
import { InstantDistributionAgreementV1 } from "../../typechain/InstantDistributionAgreementV1";
import { IDAEventType } from "../helpers/constants";
import { fetchEntityAndEnsureExistence, toBN } from "../helpers/helpers";
import {
    IIndex,
    IStream,
    IStreamData,
    IIndexSubscription,
    IEvent,
    IAccount,
} from "../interfaces";
import {
    getAccount,
    getIndex,
    getStream,
    getSubscription,
} from "../queries/holQueries";
import { validateReverseLookup } from "./validators";

export const fetchStreamAndValidate = async (
    streamData: IStreamData,
    expectedStreamedUntilUpdatedAt: BigNumber,
    flowRate: string,
    event: IEvent,
    isCreate: boolean
) => {
    const streamId = streamData.id;
    const stream = await fetchEntityAndEnsureExistence<IStream>(
        getStream,
        streamId,
        "Stream"
    );

    validateStreamEntity(
        stream,
        expectedStreamedUntilUpdatedAt.toString(),
        streamId,
        flowRate
    );

    // validate flowUpdated reverse lookup on Stream entity
    validateReverseLookup(event, stream.flowUpdatedEvents);

    if (isCreate) {
        // validate accounts reverse lookup on Stream entity creation
        fetchAccountsAndValidate(stream);
    }
};

export const fetchAccountsAndValidate = async (stream: IStream) => {
    const senderAccount = await fetchEntityAndEnsureExistence<IAccount>(
        getAccount,
        stream.sender.id,
        "Account"
    );
    const receiverAccount = await fetchEntityAndEnsureExistence<IAccount>(
        getAccount,
        stream.receiver.id,
        "Account"
    );
    const streamLightEntity = { id: stream.id };
    validateReverseLookup(streamLightEntity, senderAccount.outflows);
    validateReverseLookup(streamLightEntity, receiverAccount.inflows);
};

export const fetchIndexAndValidate = async (
    idaV1: InstantDistributionAgreementV1,
    expectedIndex: IIndex,
    eventType: IDAEventType,
    event: IEvent,
    subscriptionId: string,
    subscriptionExists: boolean
) => {
    const index = await fetchEntityAndEnsureExistence<IIndex>(
        getIndex,
        expectedIndex.id,
        "Index"
    );

    validateIndexEntity(idaV1, index, expectedIndex);

    if (
        eventType === IDAEventType.IndexCreated &&
        index.indexCreatedEvent != null
    ) {
        // We expect a indexCreatedEvent to be created one time
        validateReverseLookup(event, [index.indexCreatedEvent]);
    }

    if (
        eventType === IDAEventType.IndexUpdated &&
        index.indexUpdatedEvents != null
    ) {
        validateReverseLookup(event, index.indexUpdatedEvents);
    }

    if (
        subscriptionExists === false &&
        index.subscriptions != null &&
        (eventType === IDAEventType.SubscriptionUnitsUpdated ||
            eventType === IDAEventType.SubscriptionApproved)
    ) {
        validateReverseLookup({ id: subscriptionId }, index.subscriptions);
        // We expect a new subscriber if they are added the first time
    }
};

export const fetchSubscriptionAndValidate = async (
    idaV1: InstantDistributionAgreementV1,
    expectedSubscription: IIndexSubscription,
    newIndexValue: string,
    eventType: IDAEventType,
    event: IEvent
) => {
    const indexSubscription =
        await fetchEntityAndEnsureExistence<IIndexSubscription>(
            getSubscription,
            expectedSubscription.id,
            "Subscription"
        );

    validateSubscriptionEntity(
        idaV1,
        indexSubscription,
        expectedSubscription,
        newIndexValue
    );

    // For each we expect the event to equal the new event
    if (
        eventType === IDAEventType.SubscriptionApproved &&
        indexSubscription.subscriptionApprovedEvents != null
    ) {
        validateReverseLookup(
            event,
            indexSubscription.subscriptionApprovedEvents
        );
    }
    if (
        eventType === IDAEventType.SubscriptionRevoked &&
        indexSubscription.subscriptionRevokedEvents != null
    ) {
        validateReverseLookup(
            event,
            indexSubscription.subscriptionRevokedEvents
        );
    }
    if (
        eventType === IDAEventType.SubscriptionUnitsUpdated &&
        indexSubscription.subscriptionUnitsUpdatedEvents != null
    ) {
        validateReverseLookup(
            event,
            indexSubscription.subscriptionUnitsUpdatedEvents
        );
    }
};

export const validateStreamEntity = (
    subgraphStream: IStream,
    expectedStreamedUntilUpdatedAt: string,
    streamId: string,
    currentFlowRate: string
) => {
    expect(subgraphStream.id, "Stream: id error").to.be.equal(streamId);
    expect(
        subgraphStream.currentFlowRate,
        "Stream: currentFlowRate error"
    ).to.equal(currentFlowRate);
    expect(
        subgraphStream.streamedUntilUpdatedAt,
        "Stream: streamedUntilUpdatedAt error"
    ).to.be.equal(expectedStreamedUntilUpdatedAt);
};

export const validateIndexEntity = async (
    idaV1: InstantDistributionAgreementV1,
    subgraphIndex: IIndex,
    expectedIndex: IIndex
) => {
    const superToken = ethers.utils.getAddress(subgraphIndex.token.id);
    const publisher = ethers.utils.getAddress(subgraphIndex.publisher.id);
    const [, indexValue, totalUnitsApproved, totalUnitsPending] =
        await idaV1.getIndex(
            superToken,
            publisher,
            Number(expectedIndex.indexId)
        );

    // Check subgraph data against expected data
    expect(subgraphIndex.indexId, "Index: indexId error").to.equal(
        expectedIndex.indexId
    );
    expect(subgraphIndex.indexValue, "Index: index value error").to.equal(
        expectedIndex.indexValue
    );
    expect(
        subgraphIndex.totalSubscriptionsWithUnits,
        "Index: totalSubscriptionWithUnits error"
    ).to.equal(expectedIndex.totalSubscriptionsWithUnits);
    expect(
        subgraphIndex.totalUnitsPending,
        "Index: totalUnitsPending error"
    ).to.equal(expectedIndex.totalUnitsPending);
    expect(
        subgraphIndex.totalUnitsApproved,
        "Index: totalUnitsApproved error"
    ).to.equal(expectedIndex.totalUnitsApproved);
    expect(subgraphIndex.totalUnits, "Index: totalUnits error").to.equal(
        expectedIndex.totalUnits
    );
    expect(
        subgraphIndex.totalAmountDistributedUntilUpdatedAt,
        "Index: totalAmountDistributedUntilUpdatedAt error"
    ).to.equal(expectedIndex.totalAmountDistributedUntilUpdatedAt);

    // Check subgraph data against web3 data
    const totalUnits = totalUnitsPending.add(totalUnitsApproved);
    expect(subgraphIndex.indexValue, "Index: index value error").to.equal(
        indexValue.toString()
    );
    expect(
        subgraphIndex.totalUnitsApproved,
        "Index: totalUnitsApproved error"
    ).to.equal(totalUnitsApproved.toString());
    expect(
        subgraphIndex.totalUnitsPending,
        "Index: totalUnitsPending error"
    ).to.equal(totalUnitsPending);
    expect(subgraphIndex.totalUnits, "Index: totalUnits error").to.equal(
        totalUnits.toString()
    );
};

export const validateSubscriptionEntity = async (
    idaV1: InstantDistributionAgreementV1,
    subgraphSubscription: IIndexSubscription,
    expectedSubscription: IIndexSubscription,
    newIndexValue: string
) => {
    const token = ethers.utils.getAddress(subgraphSubscription.index.token.id);
    const publisher = ethers.utils.getAddress(
        subgraphSubscription.index.publisher.id
    );
    const subscriberAddress = ethers.utils.getAddress(
        subgraphSubscription.subscriber.id
    );

    const [, approved, units, pendingDistribution] =
        await idaV1.getSubscription(
            token,
            publisher,
            Number(subgraphSubscription.index.indexId),
            subscriberAddress
        );

    // Check subgraph data against expected data
    expect(
        subgraphSubscription.approved,
        "Subscription: approved error"
    ).to.equal(expectedSubscription.approved);
    expect(subgraphSubscription.units, "Subscription: units error").to.equal(
        expectedSubscription.units
    );
    expect(
        subgraphSubscription.totalAmountReceivedUntilUpdatedAt,
        "Subscription: totalAmountReceivedUntilUpdatedAt error"
    ).to.equal(expectedSubscription.totalAmountReceivedUntilUpdatedAt);
    expect(
        subgraphSubscription.indexValueUntilUpdatedAt,
        "Subscription: indexValueUntilUpdatedAt error"
    ).to.equal(expectedSubscription.indexValueUntilUpdatedAt);

    // Check subgraph data against web3 data
    expect(
        subgraphSubscription.approved,
        "Subscription: approved error"
    ).to.equal(approved);
    expect(subgraphSubscription.units, "Subscription: units error").to.equal(
        units.toString()
    );
    const calcPendingDistribution = approved
        ? "0"
        : toBN(subgraphSubscription.units).mul(
              toBN(newIndexValue).sub(
                  toBN(subgraphSubscription.indexValueUntilUpdatedAt)
              )
          );
    expect(calcPendingDistribution.toString()).to.equal(
        pendingDistribution.toString()
    );
};
