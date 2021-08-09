import { BigDecimal, BigInt, ethereum, log } from "@graphprotocol/graph-ts";

import {
    // IConstantFlowAgreementV1 as ConstantFlowAgreementV1,
    FlowUpdated as FlowUpdatedEvent,
} from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";

import {
    Transaction,
    Account,
    Flow,
    Token,
    FlowUpdated,
} from "../../generated/schema";

import {
    createEventID,
    fetchFlow,
    fetchToken,
    logTransaction,
    updateBalance,
} from "../utils";

export function handleFlowUpdated(event: FlowUpdatedEvent): void {
    let ownerId = event.params.sender.toHex();
    let recipientId = event.params.receiver.toHex();
    let tokenId = event.params.token.toHex();
    let flowRate = event.params.flowRate;

    let currentTimestamp = event.block.timestamp;
    // Get the existing flow
    let flow = fetchFlow(ownerId, recipientId, tokenId, currentTimestamp);
    // TODO: No need for BigDecimal here?
    let oldFlowRate = flow.flowRate;
    let duration = currentTimestamp.minus(flow.lastUpdate).toBigDecimal();

    flow.flowRate = flowRate;
    let sum = flow.sum;
    sum = sum + oldFlowRate.toBigDecimal().times(duration);
    flow.sum = sum;
    flow.save();

    let ev = new FlowUpdated(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.flow = flow.id;
    ev.oldFlowRate = oldFlowRate;
    ev.flowRate = flowRate;
    ev.sum = sum;
    ev.save();

    updateBalance(ownerId, tokenId);
    updateBalance(recipientId, tokenId);
}
