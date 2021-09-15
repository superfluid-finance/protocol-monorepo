import {
    CustomSuperTokenCreated as CustomSuperTokenCreatedEvent,
    SuperTokenCreated as SuperTokenCreatedEvent,
    SuperTokenLogicCreated as SuperTokenLogicCreatedEvent,
} from "../../generated/SuperTokenFactory/ISuperTokenFactory";
import {
    CustomSuperTokenCreated,
    SuperTokenCreated,
    SuperTokenLogicCreated,
} from "../../generated/schema";
import { createEventID, getOrInitToken } from "../utils";

export function handleSuperTokenCreated(event: SuperTokenCreatedEvent): void {
    let ev = new SuperTokenCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    let tokenAddress = event.params.token.toHex();
	// anything that is not expected host
    getOrInitToken(tokenAddress, event.block.timestamp);
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    let ev = new CustomSuperTokenCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    let tokenAddress = event.params.token.toHex();
    getOrInitToken(tokenAddress, event.block.timestamp);
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    let ev = new SuperTokenLogicCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.tokenLogic = event.params.tokenLogic;
    ev.save();
}
