import { Address } from "@graphprotocol/graph-ts";
import {
    CustomSuperTokenCreated as CustomSuperTokenCreatedEvent,
    SuperTokenCreated as SuperTokenCreatedEvent,
    SuperTokenLogicCreated as SuperTokenLogicCreatedEvent,
} from "../../../generated/SuperTokenFactory/ISuperTokenFactory";
import {
    CustomSuperTokenCreated,
    SuperTokenCreated,
    SuperTokenLogicCreated,
} from "../../../generated/schema";
import {
    createEventID,
    getOrInitSuperToken,
    tokenHasValidHost,
} from "../../utils";

export function handleSuperTokenCreated(
    event: SuperTokenCreatedEvent,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let ev = new SuperTokenCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    getOrInitSuperToken(event.params.token, resolverAddress, event.block);
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent,
    hostAddress: Address,
    resolverAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let ev = new CustomSuperTokenCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    getOrInitSuperToken(event.params.token, resolverAddress, event.block);
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.tokenLogic);
    if (!hasValidHost) {
        return;
    }

    let ev = new SuperTokenLogicCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.tokenLogic = event.params.tokenLogic;
    ev.save();
}
