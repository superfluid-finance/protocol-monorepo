import {
    CustomSuperTokenCreated,
    SuperTokenCreated,
    SuperTokenLogicCreated,
} from "../../generated/SuperTokenFactory/ISuperTokenFactory";
import {
    CustomSuperTokenCreatedEvent,
    SuperTokenCreatedEvent,
    SuperTokenLogicCreatedEvent,
} from "../../generated/schema";
import { createEventID, tokenHasValidHost } from "../utils";
import { getOrInitSuperToken } from "../mappingHelpers";
import { getHostAddress } from "../addresses";

export function handleSuperTokenCreated(event: SuperTokenCreated): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let ev = new SuperTokenCreatedEvent(
        createEventID("superTokenCreatedEvent", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    getOrInitSuperToken(event.params.token, event.block);
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreated
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let ev = new CustomSuperTokenCreatedEvent(
        createEventID("customSuperTokenCreatedEvent", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.save();

    getOrInitSuperToken(event.params.token, event.block);
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreated
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.tokenLogic);
    if (!hasValidHost) {
        return;
    }

    let ev = new SuperTokenLogicCreatedEvent(
        createEventID("superTokenLogicCreatedEvent", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.tokenLogic = event.params.tokenLogic;
    ev.save();
}
