import { BigInt } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { FlowUpdated } from "../../generated/schema";
import {
    createAndReturnTxn,
    createEventID,
    fetchStream,
    updateBalance,
} from "../utils";

// this doesn't need to save account because we are doing it in fetchStream
export function handleStreamUpdated(event: FlowUpdatedEvent): void {
    let senderId = event.params.sender.toHex();
    let receiverId = event.params.receiver.toHex();
    let tokenId = event.params.token.toHex();
    let flowRate = event.params.flowRate;

    let currentTimestamp = event.block.timestamp;
    // Get the existing flow / initialize if it doesn't exist
    let stream = fetchStream(senderId, receiverId, tokenId, currentTimestamp);

    let oldFlowRate = stream.currentFlowRate;
    let duration = currentTimestamp.minus(stream.lastUpdate);

    stream.currentFlowRate = flowRate;
    let oldStreamedUntilLastUpdate = stream.streamedUntilLastUpdate;
    let newStreamedUntilLastUpdate = oldStreamedUntilLastUpdate.plus(
        oldFlowRate.times(duration)
    );
    stream.lastUpdate = currentTimestamp;
    stream.streamedUntilLastUpdate = newStreamedUntilLastUpdate;
    stream.save();

    let ev = new FlowUpdated(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = tokenId;
    ev.sender = event.params.sender;
    ev.receiver = event.params.receiver;
    ev.flowRate = flowRate;
    ev.totalSenderFlowRate = event.params.totalSenderFlowRate;
    ev.totalReceiverFlowRate = event.params.totalReceiverFlowRate;
    ev.userData = event.params.userData;
    ev.oldFlowRate = oldFlowRate;

    // NOTE: ensure that this works as expected
    let type =
        oldFlowRate === new BigInt(0)
            ? "create"
            : flowRate === new BigInt(0)
            ? "terminate"
            : "delete";
    ev.type = type;
    ev.save();

    // TODO: create/update the necessary aggregate functions in here.
    updateBalance(senderId, tokenId);
    updateBalance(receiverId, tokenId);
}
