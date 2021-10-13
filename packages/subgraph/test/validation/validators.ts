import {
    IAccountTokenSnapshot,
    IIndex,
    IStreamData,
    IIndexSubscription,
    ITokenStatistic,
    IEvent,
    ILightEntity,
} from "../interfaces";
import {
    fetchIndexAndValidate,
    fetchStreamAndValidate,
    fetchSubscriptionAndValidate,
} from "./holValidators";
import {
    fetchATSAndValidate,
    fetchTokenStatsAndValidate,
} from "./aggregateValidators";
import { InstantDistributionAgreementV1 } from "../../typechain/InstantDistributionAgreementV1";
import { BigNumber } from "@ethersproject/bignumber";
import { expect } from "chai";
import { IDAEventType } from "../helpers/constants";

export async function validateFlowUpdated(
    pastStreamData: IStreamData,
    streamedAmountUntilTimestamp: BigNumber,
    flowRate: BigNumber,
    tokenId: string,
    updatedSenderATS: IAccountTokenSnapshot,
    updatedReceiverATS: IAccountTokenSnapshot,
    updatedTokenStats: ITokenStatistic,
    event: IEvent,
    isCreate: boolean
) {
    // validate Stream HOL
    await fetchStreamAndValidate(
        pastStreamData,
        streamedAmountUntilTimestamp,
        flowRate.toString(),
        event,
        isCreate
    );

    // validate sender ATS
    await fetchATSAndValidate(updatedSenderATS.id, updatedSenderATS);

    // validate receiver ATS
    await fetchATSAndValidate(updatedReceiverATS.id, updatedReceiverATS);

    // validate token stats
    await fetchTokenStatsAndValidate(tokenId, updatedTokenStats);
}

export async function validateModifyIDA(
    idaV1: InstantDistributionAgreementV1,
    updatedIndex: IIndex,
    updatedSubscription: IIndexSubscription,
    updatedPublisherATS: IAccountTokenSnapshot,
    updatedSubscriberATS: IAccountTokenSnapshot,
    updatedTokenStats: ITokenStatistic,
    token: string,
    publisher: string,
    subscriberAddress: string,
    eventType: IDAEventType,
    event: IEvent,
    subscriptionExists: boolean
) {
    // We don't want to validate the subscriber for the IndexCreated/IndexUpdated
    // events
    if (subscriberAddress !== "") {
        await fetchSubscriptionAndValidate(
            idaV1,
            updatedSubscription,
            updatedIndex.indexValue,
            eventType,
            event
        );
        const subscriberATSId =
            subscriberAddress.toLowerCase() + "-" + token.toLowerCase();
        await fetchATSAndValidate(subscriberATSId, updatedSubscriberATS);
    }
    await fetchIndexAndValidate(
        idaV1,
        updatedIndex,
        eventType,
        event,
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
