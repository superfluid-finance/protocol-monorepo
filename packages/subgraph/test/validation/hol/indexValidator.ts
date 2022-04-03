import { expect } from "chai";
import { ethers } from "ethers";
import {Framework} from "@superfluid-finance/sdk-core";
import {
    IDAEventType,
} from "../../helpers/constants";
import { fetchEntityAndEnsureExistence, toBN } from "../../helpers/helpers";
import {
    IIndex,
    IEvent,
    IAccount,
    IIDAEvents,
    ILightEntity,
} from "../../interfaces";
import {
    getAccount,
    getIndex,
} from "../../queries/holQueries";
import { validateReverseLookup } from "../validators";

const validateAccountReverseLookupsForIndex = async (index: IIndex) => {
    const indexLightEntity = { id: index.id };
    const publisherAccount = await fetchEntityAndEnsureExistence<IAccount>(
        getAccount,
        index.publisher.id,
        "Account"
    );
    validateReverseLookup(indexLightEntity, publisherAccount.publishedIndexes);
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

export const fetchIndexAndValidate = async (
    framework: Framework,
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

    validateIndexEntity(framework, index, expectedIndex);
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
        validateAccountReverseLookupsForIndex(index);
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

export const validateIndexEntity = async (
    framework: Framework,
    subgraphIndex: IIndex,
    expectedIndex: IIndex
) => {
    const superToken = ethers.utils.getAddress(subgraphIndex.token.id);
    const publisher = ethers.utils.getAddress(subgraphIndex.publisher.id);
    const index =
        await framework.idaV1.getIndex({
            superToken,
            publisher,
            indexId: expectedIndex.indexId,
            providerOrSigner: framework.settings.provider
        });
    const totalUnitsPending = toBN(index.totalUnitsPending);
    const totalUnitsApproved = toBN(index.totalUnitsApproved);
    const indexValue = toBN(index.indexValue);

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
