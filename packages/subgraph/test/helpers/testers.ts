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
 * - valiate HOL/aggregate entities based on the expected data
 *************************************************************************/

import {
    IExpectedFlowUpdateEvent,
    IFlowUpdated,
    ITestModifyFlowData,
} from "../interfaces";
import { getFlowUpdatedEvents } from "../queries/eventQueries";
import { fetchEventAndValidate } from "../validation/eventValidators";
import { validateFlowUpdated } from "../validation/validators";
import {
    modifyFlowAndReturnCreatedFlowData,
    monthlyToSecondRate,
    toBN,
} from "./helpers";
import { getOrInitializeDataForFlowUpdated } from "./initializers";
import {
    getExpectedDataForFlowUpdated,
    getExpectedStreamData,
} from "./updaters";

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
export async function testModifyFlow(data: ITestModifyFlowData) {
    const {
        contracts,
        localData,
        provider,
        actionType,
        atsArray,
        newFlowRate,
        sender,
        receiver,
        tokenAddress,
    } = data;

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
    const { updatedSenderATS, updatedReceiverATS, updatedTokenStats } =
        await getExpectedDataForFlowUpdated({
            actionType,
            lastUpdatedBlockNumber,
            lastUpdatedAtTimestamp,
            accountTokenSnapshots: atsArray,
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

    await validateFlowUpdated(
        pastStreamData,
        streamedAmountUntilTimestamp,
        flowRate,
        tokenId,
        updatedSenderATS,
        updatedReceiverATS,
        updatedTokenStats
    );

    let updatedStreamData = getExpectedStreamData(
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
