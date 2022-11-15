import { expect } from "chai";
import { ethers } from "ethers";
import { Framework } from "@superfluid-finance/sdk-core";
import { IDAEventType } from "../../helpers/constants";
import { fetchEntityAndEnsureExistence, toBN } from "../../helpers/helpers";
import {
    IIndexSubscription,
    IEvent,
    IAccount,
    IIDAEvents,
    ILightEntity,
} from "../../interfaces";
import { getAccount, getSubscription } from "../../queries/holQueries";
import { validateReverseLookup } from "../validators";

const validateAccountReverseLookupsForSubscription = async (
    subscription: IIndexSubscription
) => {
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

export const fetchSubscriptionAndValidate = async (
    framework: Framework,
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
        framework,
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
        validateAccountReverseLookupsForSubscription(indexSubscription);
    }
};

export const validateSubscriptionEntity = async (
    framework: Framework,
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

    const subscription = await framework.idaV1.getSubscription({
        superToken: token,
        publisher,
        indexId: subgraphSubscription.index.indexId,
        subscriber: subscriberAddress,
        providerOrSigner: framework.settings.provider,
    });

    const approved = subscription.approved;
    const units = subscription.units;
    const pendingDistribution = subscription.pendingDistribution;

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
        units
    );
    const calcPendingDistribution = approved
        ? "0"
        : toBN(subgraphSubscription.units).mul(
              toBN(newIndexValue).sub(
                  toBN(subgraphSubscription.indexValueUntilUpdatedAt)
              )
          );
    expect(calcPendingDistribution.toString()).to.equal(pendingDistribution);
};
