import {
    BigDecimal,
    BigInt,
    EthereumEvent,
    log,
} from "@graphprotocol/graph-ts";

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
    fetchAccount,
    logTransaction,
    toDai,
} from "../utils";

export function handleFlowUpdated(event: FlowUpdatedEvent): void {
    let ownerAddress = event.params.sender.toHex();
    let recipientAddress = event.params.receiver.toHex();
    let tokenAddress = event.params.token.toHex();
    let flowRate = event.params.flowRate;

    let currentTimestamp = event.block.timestamp;

    let flow = fetchFlow(
        ownerAddress,
        recipientAddress,
        tokenAddress,
        currentTimestamp
    );
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
}
