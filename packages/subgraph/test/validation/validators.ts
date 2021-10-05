import { BaseProvider } from "@ethersproject/providers";
import {
    getExpectedDataForFlowUpdated,
    getOrInitializeDataForFlowUpdated,
    modifyFlowAndReturnCreatedFlowData,
    monthlyToSecondRate,
    toBN,
    updateAndReturnStreamData,
} from "../helpers/helpers";
import { FlowActionType } from "../helpers/constants";
import {
    IAccountTokenSnapshot,
    IContracts,
    IExpectedFlowUpdateEvent,
    IFlowUpdated,
    IIndex,
    IStreamLocalData,
    ISubscriber,
    ITokenStatistic,
} from "../interfaces";
import { fetchEventAndValidate } from "./eventValidators";
import {
    fetchIndexAndValidate,
    fetchStreamAndValidate,
    fetchSubscriberAndValidate,
} from "./holValidators";
import {
    fetchATSAndValidate,
    fetchTokenStatsAndValidate,
} from "./aggregateValidators";
import { getFlowUpdatedEvents } from "../queries/eventQueries";
import { InstantDistributionAgreementV1 } from "../../typechain/InstantDistributionAgreementV1";

export async function validateModifyFlow(
    contracts: IContracts,
    localData: IStreamLocalData,
    provider: BaseProvider,
    actionType: FlowActionType,
    newFlowRate: number,
    sender: string,
    receiver: string,
    tokenAddress: string
) {
    // Spread operator the variables
    const { sf, cfaV1, superToken } = contracts;
    const {
        accountTokenSnapshots,
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
        lastUpdatedAtTimestamp,
        lastUpdatedBlockNumber,
        sender,
        receiver,
        token: superToken.address,
        accountTokenSnapshots,
        revisionIndexes,
        streamData,
        tokenStatistics,
    });

    // update and return updated (expected) data
    const accountTokenSnapshotsArray = Object.values(
        accountTokenSnapshots
    ).filter((x) => x != undefined) as IAccountTokenSnapshot[];
    const { updatedSenderATS, updatedReceiverATS, updatedTokenStats } =
        await getExpectedDataForFlowUpdated({
            actionType,
            lastUpdatedBlockNumber,
            lastUpdatedAtTimestamp,
            accountTokenSnapshots: accountTokenSnapshotsArray,
            flowRate,
            superToken,
            pastStreamData,
            currentSenderATS,
            currentReceiverATS,
            currentTokenStats,
        });

    const streamedAmountSinceUpdatedAt = toBN(pastStreamData.oldFlowRate).mul(
        toBN(lastUpdatedAtTimestamp).sub(
            toBN(pastStreamData.lastUpdatedAtTimestamp)
        )
    );

    const senderNetFlow = await cfaV1.getNetFlow(tokenAddress, sender);
    const receiverNetFlow = await cfaV1.getNetFlow(tokenAddress, receiver);
    // validate FlowUpdatedEvent
    // TODO: test totalAmountUntilTimestamp
    const streamedAmountUntilTimestamp = toBN(
        pastStreamData.streamedUntilUpdatedAt
    ).add(streamedAmountSinceUpdatedAt);
    await fetchEventAndValidate<IFlowUpdated, IExpectedFlowUpdateEvent>(
        receipt,
        {
            flowRate: flowRate.toString(),
            oldFlowRate: pastStreamData.oldFlowRate,
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
        "flowUpdateds",
        "FlowUpdated"
    );

    // validate Stream HOL
    await fetchStreamAndValidate(
        pastStreamData,
        streamedAmountUntilTimestamp,
        flowRate.toString()
    );

    // validate sender ATS
    await fetchATSAndValidate(currentSenderATS.id, updatedSenderATS);

    // validate receiver ATS
    await fetchATSAndValidate(currentReceiverATS.id, updatedReceiverATS);

    // validate token stats
    await fetchTokenStatsAndValidate(tokenId, updatedTokenStats);

    let updatedStreamData = updateAndReturnStreamData(
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
