import {
    IAccountTokenSnapshot,
    IIndex,
    IStreamData,
    IIndexSubscription,
    ITokenStatistic,
    IEvent,
    ILightEntity,
    IIDAEvents,
} from "../interfaces";
import { fetchStreamPeriodAndValidate } from "./hol/streamPeriodValidator";
import { fetchIndexAndValidate } from "./hol/indexValidator";
import { fetchStreamAndValidate } from "./hol/streamValidator";
import { fetchSubscriptionAndValidate } from "./hol/subscriptionValidator";
import {
    fetchATSAndValidate,
    fetchTokenStatsAndValidate,
} from "./aggregateValidators";
import {Framework} from "@superfluid-finance/sdk-core";
import { BigNumber } from "@ethersproject/bignumber";
import { expect } from "chai";
import { FlowActionType, IDAEventType } from "../helpers/constants";

export async function validateFlowUpdated(
    pastStreamData: IStreamData,
    streamedAmountUntilTimestamp: BigNumber,
    flowRate: BigNumber,
    tokenId: string,
    updatedSenderATS: IAccountTokenSnapshot,
    updatedReceiverATS: IAccountTokenSnapshot,
    updatedTokenStats: ITokenStatistic,
    event: IEvent,
    actionType: FlowActionType
) {
    // validate Stream HOL
    await fetchStreamAndValidate(
        pastStreamData,
        streamedAmountUntilTimestamp,
        flowRate.toString(),
        event,
        actionType === FlowActionType.Create
    );
    // validate StreamPeriod HOL
    await fetchStreamPeriodAndValidate(
        pastStreamData,
        flowRate.toString(),
        event,
        actionType
    );

    // validate sender ATS
    await fetchATSAndValidate(updatedSenderATS.id, updatedSenderATS);

    // validate receiver ATS
    await fetchATSAndValidate(updatedReceiverATS.id, updatedReceiverATS);

    // validate token stats
    await fetchTokenStatsAndValidate(tokenId, updatedTokenStats);
}

export async function validateModifyIDA(
    framework: Framework,
    updatedIndex: IIndex,
    updatedSubscription: IIndexSubscription,
    updatedPublisherATS: IAccountTokenSnapshot,
    updatedSubscriberATS: IAccountTokenSnapshot,
    updatedTokenStats: ITokenStatistic,
    token: string,
    publisher: string,
    subscriberAddress: string,
    eventType: IDAEventType,
    events: IIDAEvents,
    subscriptionExists: boolean
) {
    // We don't want to validate the subscriber for the IndexCreated/IndexUpdated
    // events
    if (subscriberAddress !== "") {
        await fetchSubscriptionAndValidate(
            framework,
            updatedSubscription,
            updatedIndex.indexValue,
            eventType,
            events,
            subscriptionExists
        );
        const subscriberATSId =
            subscriberAddress.toLowerCase() + "-" + token.toLowerCase();
        await fetchATSAndValidate(subscriberATSId, updatedSubscriberATS);
    }
    await fetchIndexAndValidate(
        framework,
        updatedIndex,
        eventType,
        events,
        updatedSubscription.id,
        subscriptionExists
    );
    const publisherATSId = publisher.toLowerCase() + "-" + token.toLowerCase();
    await fetchATSAndValidate(publisherATSId, updatedPublisherATS);
    await fetchTokenStatsAndValidate(token.toLowerCase(), updatedTokenStats);
}

export function validateReverseLookup(
    entity: ILightEntity,
    entities: ILightEntity[]
) {
    const entityToValidate = entities[entities.length - 1];
    expect(entity.id, "reverse lookup error").to.equal(entityToValidate.id);
}
