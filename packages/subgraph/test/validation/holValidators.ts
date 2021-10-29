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
    IIDAEvents,
    ILightEntity,
    IToken,
} from "../interfaces";
import {
    getAccount,
    getIndex,
    getStream,
    getSubscription,
    getToken,
} from "../queries/holQueries";
import { validateReverseLookup } from "./validators";

export const fetchTokenAndValidate = async (
    address: string,
    expectedName: string,
    expectedSymbol: string,
    expectedIsListed: boolean
) => {
    const token = await fetchEntityAndEnsureExistence<IToken>(
        getToken,
        address,
        "Token"
    );

    validateTokenEntity(token, expectedName, expectedSymbol, expectedIsListed);
};

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
        validateAccountReverseLookups({ stream });
    }
};

export const validateAccountReverseLookups = async (data: {
    stream?: IStream;
    index?: IIndex;
    subscription?: IIndexSubscription;
}) => {
    const { stream, index, subscription } = data;
    if (stream) {
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
    }
    if (index) {
        const indexLightEntity = { id: index.id };
        const publisherAccount = await fetchEntityAndEnsureExistence<IAccount>(
            getAccount,
            index.publisher.id,
            "Account"
        );
        validateReverseLookup(
            indexLightEntity,
            publisherAccount.publishedIndexes
        );
    }
    if (subscription) {
        const subscriptionLightEntity = { id: subscription.id };
        const subscriberAccount = await fetchEntityAndEnsureExistence<IAccount>(
            getAccount,
            subscription.subscriber.id,
            "Account"
        );
        validateReverseLookup(
            subscriptionLightEntity,
            subscriberAccount.subscriptions
        );
    }
};

const getIndexEventsMap = (index: IIndex, events: IIDAEvents) => {
    return new Map<
        IDAEventType,
        { event: IEvent | undefined; events: ILightEntity[] | undefined }
    >([
        [
            IDAEventType.IndexUpdated,
            {
                event: events.IndexUpdatedEvent,
                events: index.indexUpdatedEvents,
            },
        ],
        [
            IDAEventType.IndexDistributionClaimed,
            {
                event: events.IndexDistributionClaimedEvent,
                events: index.indexDistributionClaimedEvents,
            },
        ],
        [
            IDAEventType.IndexSubscribed,
            {
                event: events.IndexSubscribedEvent,
                events: index.indexSubscribedEvents,
            },
        ],
        [
            IDAEventType.IndexUnitsUpdated,
            {
                event: events.IndexUnitsUpdatedEvent,
                events: index.indexUnitsUpdatedEvents,
            },
        ],
        [
            IDAEventType.IndexUnsubscribed,
            {
                event: events.IndexSubscribedEvent,
                events: index.indexSubscribedEvents,
            },
        ],
    ]);
};

const getSubscriptionEventsMap = (
    indexSubscription: IIndexSubscription,
    events: IIDAEvents
) => {
    return new Map<
        IDAEventType,
        { event: IEvent | undefined; events: ILightEntity[] | undefined }
    >([
        [
            IDAEventType.SubscriptionApproved,
            {
                event: events.SubscriptionApprovedEvent,
                events: indexSubscription.subscriptionApprovedEvents,
            },
        ],
        [
            IDAEventType.SubscriptionDistributionClaimed,
            {
                event: events.SubscriptionDistributionClaimedEvent,
                events: indexSubscription.subscriptionDistributionClaimedEvents,
            },
        ],
        [
            IDAEventType.SubscriptionRevoked,
            {
                event: events.SubscriptionRevokedEvent,
                events: indexSubscription.subscriptionRevokedEvents,
            },
        ],
        [
            IDAEventType.SubscriptionUnitsUpdated,
            {
                event: events.SubscriptionUnitsUpdatedEvent,
                events: indexSubscription.subscriptionUnitsUpdatedEvents,
            },
        ],
    ]);
};

export const fetchIndexAndValidate = async (
    idaV1: InstantDistributionAgreementV1,
    expectedIndex: IIndex,
    eventType: IDAEventType,
    events: IIDAEvents,
    subscriptionId: string,
    subscriptionExists: boolean
) => {
    const index = await fetchEntityAndEnsureExistence<IIndex>(
        getIndex,
        expectedIndex.id,
        "Index"
    );

    validateIndexEntity(idaV1, index, expectedIndex);
    const eventTypeToDataMap = getIndexEventsMap(index, events);

    if (
        eventType === IDAEventType.IndexCreated &&
        index.indexCreatedEvent != null
    ) {
        if (!events.IndexCreatedEvent) {
            throw new Error("Must have events.IndexCreatedEvents");
        }
        // We expect a indexCreatedEvent to be created one time
        validateReverseLookup(events.IndexCreatedEvent, [
            index.indexCreatedEvent,
        ]);

        // validate newly published index is added
        validateAccountReverseLookups({ index });
    }

    if (Array.from(eventTypeToDataMap.keys()).includes(eventType)) {
        const data = eventTypeToDataMap.get(eventType);
        if (!data || !data.event || !data.events) {
            throw new Error(
                "You must have all the data for validating reverse lookup."
            );
        }
        validateReverseLookup(data.event, data.events);
    }

    if (
        subscriptionExists === false &&
        index.subscriptions != null &&
        (eventType === IDAEventType.SubscriptionUnitsUpdated ||
            eventType === IDAEventType.SubscriptionApproved)
    ) {
        // We only enter here if a subscriber is being added for the first time
        // subscribers who delete or set units to 0 are still in the array
        validateReverseLookup({ id: subscriptionId }, index.subscriptions);
    }
};

export const fetchSubscriptionAndValidate = async (
    idaV1: InstantDistributionAgreementV1,
    expectedSubscription: IIndexSubscription,
    newIndexValue: string,
    eventType: IDAEventType,
    events: IIDAEvents,
    subscriptionExists: boolean
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

    const eventTypeToDataMap = getSubscriptionEventsMap(
        indexSubscription,
        events
    );

    if (Array.from(eventTypeToDataMap.keys()).includes(eventType)) {
        const data = eventTypeToDataMap.get(eventType);
        if (!data || !data.event || !data.events) {
            throw new Error(
                "You must have all the data for validating reverse lookup."
            );
        }
        validateReverseLookup(data.event, data.events);
    }

    if (
        subscriptionExists === false &&
        (eventType === IDAEventType.SubscriptionUnitsUpdated ||
            eventType === IDAEventType.SubscriptionApproved)
    ) {
        // We only enter here if a subscriber is being added for the first time
        // subscribers who delete or set units to 0 are still in the array
        validateAccountReverseLookups({ subscription: indexSubscription });
    }
};

export const validateTokenEntity = (
    subgraphToken: IToken,
    expectedName: string,
    expectedSymbol: string,
    expectedIsListed: boolean
) => {
    expect(subgraphToken.name, "SuperToken: name error").to.be.equal(
        expectedName
    );
    expect(subgraphToken.symbol, "SuperToken: symbol error").to.equal(
        expectedSymbol
    );
    expect(subgraphToken.isListed, "SuperToken: isListed error").to.be.equal(
        expectedIsListed
    );
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
