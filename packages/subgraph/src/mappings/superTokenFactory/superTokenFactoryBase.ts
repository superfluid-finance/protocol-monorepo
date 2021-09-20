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
import { createEventID, getOrInitToken } from "../../utils";

export function handleSuperTokenCreated(
    event: SuperTokenCreatedEvent,
    hostAddress: Address
): void {
    let ev = new SuperTokenCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    getOrInitToken(hostAddress, event.params.token, event.block.timestamp);
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent,
    hostAddress: Address
): void {
    let ev = new CustomSuperTokenCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    getOrInitToken(hostAddress, event.params.token, event.block.timestamp);
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent,
    hostAddress: Address
): void {
    let ev = new SuperTokenLogicCreated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.tokenLogic = event.params.tokenLogic;
    ev.save();
}
