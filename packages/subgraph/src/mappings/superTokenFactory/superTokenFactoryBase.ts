import { Address } from "@graphprotocol/graph-ts";
import {
    CustomSuperTokenCreated,
    SuperTokenCreated,
    SuperTokenLogicCreated,
} from "../../../generated/SuperTokenFactory/ISuperTokenFactory";
import {
    CustomSuperTokenCreatedEvent,
    SuperTokenCreatedEvent,
    SuperTokenLogicCreatedEvent,
} from "../../../generated/schema";
import { createEventID, tokenHasValidHost } from "../../utils";
import { getOrInitSuperToken } from "../../mappingHelpers";

export function handleSuperTokenCreated(
    event: SuperTokenCreated,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let ev = new SuperTokenCreatedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    getOrInitSuperToken(event.params.token, resolverAddress, event.block);
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreated,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let ev = new CustomSuperTokenCreatedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    getOrInitSuperToken(event.params.token, resolverAddress, event.block);
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreated,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.tokenLogic);
    if (!hasValidHost) {
        return;
    }

    let ev = new SuperTokenLogicCreatedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.tokenLogic = event.params.tokenLogic;
    ev.save();
}
