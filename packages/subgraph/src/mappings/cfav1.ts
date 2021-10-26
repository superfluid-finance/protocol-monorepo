import { BigInt } from "@graphprotocol/graph-ts";
import { FlowUpdated } from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { FlowUpdatedEvent } from "../../generated/schema";
import { createEventID, BIG_INT_ZERO, tokenHasValidHost } from "../utils";
import {
    getOrInitStream,
    getOrInitStreamRevision,
    updateAggregateEntitiesStreamData,
    updateATSStreamedAndBalanceUntilUpdatedAt,
} from "../mappingHelpers";
import { getHostAddress } from "../addresses";

enum FlowActionType {
    create,
    update,
    terminate,
}

function createFlowUpdatedEntity(
    event: FlowUpdated,
    oldFlowRate: BigInt,
    streamId: string,
    totalAmountStreamedUntilTimestamp: BigInt
): void {
    let ev = new FlowUpdatedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.sender = event.params.sender;
    ev.receiver = event.params.receiver;
    ev.flowRate = event.params.flowRate;
    ev.totalSenderFlowRate = event.params.totalSenderFlowRate;
    ev.totalReceiverFlowRate = event.params.totalReceiverFlowRate;
    ev.userData = event.params.userData;
    ev.oldFlowRate = oldFlowRate;
    ev.stream = streamId;
    ev.totalAmountStreamedUntilTimestamp = totalAmountStreamedUntilTimestamp;

    let type = oldFlowRate.equals(BIG_INT_ZERO)
        ? FlowActionType.create
        : event.params.flowRate.equals(BIG_INT_ZERO)
        ? FlowActionType.terminate
        : FlowActionType.update;
    ev.type = type;
    ev.save();
}

export function handleStreamUpdated(event: FlowUpdated): void {
    let senderAddress = event.params.sender;
    let receiverAddress = event.params.receiver;
    let tokenAddress = event.params.token;
    let flowRate = event.params.flowRate;
    let currentTimestamp = event.block.timestamp;
    let hostAddress = getHostAddress();

    let hasValidHost = tokenHasValidHost(hostAddress, tokenAddress);
    if (!hasValidHost) {
        return;
    }

    let stream = getOrInitStream(
        senderAddress,
        receiverAddress,
        tokenAddress,
        event.block
    );
    let oldFlowRate = stream.currentFlowRate;

    let timeSinceLastUpdate = currentTimestamp.minus(stream.updatedAtTimestamp);
    let userAmountStreamedSinceLastUpdate =
        oldFlowRate.times(timeSinceLastUpdate);
    let newStreamedUntilLastUpdate = stream.streamedUntilUpdatedAt.plus(
        userAmountStreamedSinceLastUpdate
    );
    stream.currentFlowRate = flowRate;
    stream.streamedUntilUpdatedAt = newStreamedUntilLastUpdate;
    stream.updatedAtTimestamp = currentTimestamp;
    stream.updatedAtBlockNumber = event.block.number;
    stream.save();

    let senderId = senderAddress.toHex();
    let receiverId = receiverAddress.toHex();
    let tokenId = tokenAddress.toHex();

    let flowRateDelta = flowRate.minus(oldFlowRate);
    let isCreate = oldFlowRate.equals(BIG_INT_ZERO);
    let isDelete = flowRate.equals(BIG_INT_ZERO);
    if (isDelete) {
        let streamRevision = getOrInitStreamRevision(
            senderId,
            receiverId,
            tokenId
        );
        streamRevision.revisionIndex = streamRevision.revisionIndex + 1;
        streamRevision.save();
    }

    // create event entity
    createFlowUpdatedEntity(
        event,
        oldFlowRate,
        stream.id,
        newStreamedUntilLastUpdate
    );

    updateATSStreamedAndBalanceUntilUpdatedAt(senderId, tokenId, event.block);
    updateATSStreamedAndBalanceUntilUpdatedAt(receiverId, tokenId, event.block);

    // update aggregate entities data
    updateAggregateEntitiesStreamData(
        senderId,
        receiverId,
        tokenId,
        flowRate,
        flowRateDelta,
        isCreate,
        isDelete,
        event.block
    );
}
