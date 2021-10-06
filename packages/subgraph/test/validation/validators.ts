import {
    IAccountTokenSnapshot,
    IIndex,
    IStreamData,
    ISubscriber,
    ITokenStatistic,
} from "../interfaces";
import {
    fetchIndexAndValidate,
    fetchStreamAndValidate,
    fetchSubscriberAndValidate,
} from "./holValidators";
import {
    fetchATSAndValidate,
    fetchTokenStatsAndValidate,
} from "./aggregateValidators";
import { InstantDistributionAgreementV1 } from "../../typechain/InstantDistributionAgreementV1";
import { BigNumber } from "@ethersproject/bignumber";

export async function validateFlowUpdated(
    pastStreamData: IStreamData,
    streamedAmountUntilTimestamp: BigNumber,
    flowRate: BigNumber,
    tokenId: string,
    updatedSenderATS: IAccountTokenSnapshot,
    updatedReceiverATS: IAccountTokenSnapshot,
    updatedTokenStats: ITokenStatistic
) {
    // validate Stream HOL
    await fetchStreamAndValidate(
        pastStreamData,
        streamedAmountUntilTimestamp,
        flowRate.toString()
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
    updatedSubscriber: ISubscriber,
    updatedPublisherATS: IAccountTokenSnapshot,
    updatedSubscriberATS: IAccountTokenSnapshot,
    updatedTokenStats: ITokenStatistic,
    token: string,
    publisher: string,
    subscriber: string
) {
    await fetchIndexAndValidate(idaV1, updatedIndex);
    await fetchSubscriberAndValidate(
        idaV1,
        updatedSubscriber,
        updatedIndex.newIndexValue
    );
    const publisherATSId = publisher.toLowerCase() + "-" + token.toLowerCase();
    const subscriberATSId =
        subscriber.toLowerCase() + "-" + token.toLowerCase();
    await fetchATSAndValidate(publisherATSId, updatedPublisherATS);
    await fetchATSAndValidate(subscriberATSId, updatedSubscriberATS);
    await fetchTokenStatsAndValidate(token.toLowerCase(), updatedTokenStats);
}
