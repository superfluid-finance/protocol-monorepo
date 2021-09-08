import { BigInt } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { FlowUpdated } from "../../generated/schema";
import { createEventID, getStream, updateBalance } from "../utils";

function createFlowUpdatedEntity(
    event: FlowUpdatedEvent,
    oldFlowRate: BigInt
): void {
    let ev = new FlowUpdated(createEventID(event));
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = event.params.token.toHex();
    ev.sender = event.params.sender;
    ev.receiver = event.params.receiver;
    ev.flowRate = event.params.flowRate;
    ev.totalSenderFlowRate = event.params.totalSenderFlowRate;
    ev.totalReceiverFlowRate = event.params.totalReceiverFlowRate;
    ev.userData = event.params.userData;
    ev.oldFlowRate = oldFlowRate;

    // NOTE: ensure that this works as expected
    let type =
        oldFlowRate === BigInt.fromI32(0)
            ? "create"
            : event.params.flowRate === BigInt.fromI32(0)
            ? "terminate"
            : "delete";
    ev.type = type;
    ev.save();
}

export function handleStreamUpdated(event: FlowUpdatedEvent): void {
    let senderId = event.params.sender.toHex();
    let receiverId = event.params.receiver.toHex();
    let tokenId = event.params.token.toHex();
    let flowRate = event.params.flowRate;
    let currentTimestamp = event.block.timestamp;

    // Get the existing stream or initialize if it doesn't exist
    let stream = getStream(senderId, receiverId, tokenId, currentTimestamp);
    let oldFlowRate = stream.currentFlowRate;
    let timeSinceLastUpdate = currentTimestamp.minus(stream.updatedAt);
    let newStreamedUntilLastUpdate = stream.streamedUntilLastUpdate.plus(
        oldFlowRate.times(timeSinceLastUpdate)
    );

    stream.currentFlowRate = flowRate;
    stream.updatedAt = currentTimestamp;
    stream.streamedUntilLastUpdate = newStreamedUntilLastUpdate;
    stream.save();

    createFlowUpdatedEntity(event, oldFlowRate);

    // TODO: create/update the necessary aggregate functions in here.
    updateBalance(senderId, tokenId);
    updateBalance(receiverId, tokenId);
}
