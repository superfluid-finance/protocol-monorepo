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
import {
    createEventID,
    createTxnAndReturn,
    createOrUpdateToken,
} from "../utils";

export function handleSuperTokenCreated(event: SuperTokenCreatedEvent): void {
    let tokenAddress = event.params.token.toHex();
    createOrUpdateToken(tokenAddress, event.block.timestamp);

    let ev = new SuperTokenCreated(createEventID(event));
    ev.transaction = createTxnAndReturn(event).id;
    ev.token = tokenAddress;
    ev.save();
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    let tokenAddress = event.params.token.toHex();
    createOrUpdateToken(tokenAddress, event.block.timestamp);

    let ev = new CustomSuperTokenCreated(createEventID(event));
    ev.transaction = createTxnAndReturn(event).id;
    ev.token = tokenAddress;
    ev.save();
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    let tokenAddress = event.params.tokenLogic.toHex();
    createOrUpdateToken(tokenAddress, event.block.timestamp);

    let ev = new SuperTokenLogicCreated(createEventID(event));
    ev.transaction = createTxnAndReturn(event).id;
    ev.token = tokenAddress;
    ev.save();
}
