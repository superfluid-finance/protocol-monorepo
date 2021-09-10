import { BigInt } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { FlowUpdated } from "../../generated/schema";
import {
    createEventID,
    getOrInitStream,
    updateATSBalance,
    updateATSFlowRates,
    updateAggregateEntityStreamData,
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
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = event.params.token;
    ev.sender = event.params.sender;
    ev.receiver = event.params.receiver;
    ev.flowRate = event.params.flowRate;
    ev.totalSenderFlowRate = event.params.totalSenderFlowRate;
    ev.totalReceiverFlowRate = event.params.totalReceiverFlowRate;
    ev.userData = event.params.userData;
    ev.oldFlowRate = oldFlowRate;

    // NOTE: ensure that this works as expected
    let type =
        oldFlowRate === BIG_INT_ZERO
            ? FlowActionType.create
            : event.params.flowRate === BIG_INT_ZERO
            ? FlowActionType.terminate
            : FlowActionType.update;
    ev.type = type;
    ev.save();
}

// TODO: token is null, it appears that tokens don't only get created
// in super token factory, I will do a check for if the token exists
// and create if it does, also streamRevision doesn't seem to be
// incrementing - maybe do .plus(1)
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
    let flowRateDelta = flowRate.minus(oldFlowRate);
    let isCreate = oldFlowRate === BIG_INT_ZERO;
    let isDelete = flowRate === BIG_INT_ZERO;
    stream.currentFlowRate = flowRate;
    stream.updatedAt = currentTimestamp;
    stream.streamedUntilUpdatedAt = newStreamedUntilLastUpdate;
    stream.save();
    if (isDelete) {
        let streamRevision = getOrInitStreamRevision(
            senderId,
            receiverId,
            tokenId
        );
        streamRevision.revisionIndex = streamRevision.revisionIndex + 1;
        streamRevision.save();
    }

    createFlowUpdatedEntity(event, oldFlowRate);

    updateATSBalance(senderId, tokenId);
    updateATSBalance(receiverId, tokenId);
    updateATSFlowRates(senderId, receiverId, tokenId, flowRateDelta);
    updateAggregateEntityStreamData(
        senderId,
        receiverId,
        tokenId,
        flowRateDelta,
        isCreate,
        isDelete
    );
}
