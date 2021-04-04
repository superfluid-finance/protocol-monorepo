import { SuperTokenCreated as SuperTokenCreatedEvent } from "../../generated/SuperTokenFactory/ISuperTokenFactory";

import { SuperTokenCreated } from "../../generated/schema";

import { createEventID, logTransaction, fetchToken } from "../utils";

export function handleSuperTokenCreated(event: SuperTokenCreatedEvent): void {
    let address = event.params.token.toHex();
    let token = fetchToken(address);
    token.save();

    let ev = new SuperTokenCreated(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.address = event.params.token;
    ev.underlyingAddress = token.underlyingAddress;
    ev.symbol = token.symbol;
    ev.name = token.name;
    ev.save();
}
