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
import { IAccountTokenSnapshot, IContracts, ILocalData } from "../interfaces";
import { fetchFlowUpdatedEventAndValidate } from "./eventValidators";
import { fetchStreamAndValidate } from "./holValidators";
import {
    fetchATSAndValidate,
    fetchTokenStatsAndValidate,
} from "./aggregateValidators";

export async function validateModifyFlow(
    contracts: IContracts,
    localData: ILocalData,
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

    // validate FlowUpdatedEvent
    await fetchFlowUpdatedEventAndValidate(
        cfaV1,
        receipt,
        tokenAddress,
        sender,
        receiver,
        flowRate.toString(),
        pastStreamData.oldFlowRate,
        actionType
    );

    // validate Stream HOL
    await fetchStreamAndValidate(
        pastStreamData,
        streamedAmountSinceUpdatedAt,
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
