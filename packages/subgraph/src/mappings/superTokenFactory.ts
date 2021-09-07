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
import { createEventID, createAndReturnTxn, fetchToken } from "../utils";

export function handleSuperTokenCreated(event: SuperTokenCreatedEvent): void {
    let tokenAddress = event.params.token.toHex();
    let token = fetchToken(tokenAddress);
    token.save();

    let ev = new SuperTokenCreated(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = tokenAddress;
    ev.save();
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    let tokenAddress = event.params.token.toHex();
    let token = fetchToken(tokenAddress);
    token.save();

    let ev = new CustomSuperTokenCreated(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = tokenAddress;
    ev.save();
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    let tokenAddress = event.params.tokenLogic.toHex();
    let token = fetchToken(tokenAddress);
    token.save();

    let ev = new SuperTokenLogicCreated(createEventID(event));
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = tokenAddress;
    ev.save();
}
