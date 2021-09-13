import { BigInt } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { FlowUpdated } from "../../generated/schema";
import {
    createEventID,
    getOrInitStream,
    updateATSBalance,
    updateATSFlowRates,
    updateAggregateEntitiesStreamData,
    BIG_INT_ZERO,
    getOrInitStreamRevision,
} from "../utils";

enum FlowActionType {
    create,
    update,
    terminate,
}

function createFlowUpdatedEntity(
    event: FlowUpdatedEvent,
    oldFlowRate: BigInt
): void {
    let ev = new FlowUpdated(createEventID(event));
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

    let type = oldFlowRate.equals(BIG_INT_ZERO)
        ? FlowActionType.create
        : event.params.flowRate.equals(BIG_INT_ZERO)
        ? FlowActionType.terminate
        : FlowActionType.update;
    ev.type = type;
    ev.save();
}

export function handleStreamUpdated(event: FlowUpdatedEvent): void {
    let senderId = event.params.sender.toHex();
    let receiverId = event.params.receiver.toHex();
    let tokenId = event.params.token.toHex();
    let flowRate = event.params.flowRate;
    let currentTimestamp = event.block.timestamp;

    let stream = getOrInitStream(
        senderId,
        receiverId,
        tokenId,
        currentTimestamp
    );
    let oldFlowRate = stream.currentFlowRate;

    let timeSinceLastUpdate = currentTimestamp.minus(stream.updatedAt);
    let newStreamedUntilLastUpdate = stream.streamedUntilUpdatedAt.plus(
        oldFlowRate.times(timeSinceLastUpdate)
    );
    stream.currentFlowRate = flowRate;
    stream.updatedAt = currentTimestamp;
    stream.streamedUntilUpdatedAt = newStreamedUntilLastUpdate;
    stream.save();

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
    createFlowUpdatedEntity(event, oldFlowRate);

    // update aggregate entities data
    updateATSBalance(senderId, tokenId);
    updateATSBalance(receiverId, tokenId);
    updateATSFlowRates(senderId, receiverId, tokenId, flowRateDelta);
    updateAggregateEntitiesStreamData(
        senderId,
        receiverId,
        tokenId,
        flowRateDelta,
        isCreate,
        isDelete
    );
}
