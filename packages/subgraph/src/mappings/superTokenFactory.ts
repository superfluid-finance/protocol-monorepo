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
import { initializeEventEntity, tokenHasValidHost } from "../utils";
import { getOrInitSuperToken } from "../mappingHelpers";
import { getHostAddress } from "../addresses";

export function handleSuperTokenCreated(event: SuperTokenCreated): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }
    const ev = initializeEventEntity(
        "SuperTokenCreated",
        event,
        [event.params.token]
    ) as SuperTokenCreatedEvent;
    
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
    const ev = initializeEventEntity(
        "CustomSuperTokenCreated",
        event,
        [event.params.token]
    ) as CustomSuperTokenCreatedEvent;
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
    const ev = initializeEventEntity(
        "SuperTokenLogicCreated",
        event,
        []
    ) as SuperTokenLogicCreatedEvent;
    
    ev.tokenLogic = event.params.tokenLogic;
    ev.save();
}
