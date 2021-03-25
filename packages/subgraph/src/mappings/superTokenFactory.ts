import { SuperTokenCreated as SuperTokenCreatedEvent } from "../../generated/SuperTokenFactory/ISuperTokenFactory";
import { SuperToken } from "../../generated/templates";

import { SuperTokenCreated } from "../../generated/schema";

import { createEventID, logTransaction, fetchToken } from "../utils";

export function handleSuperTokenCreated(event: SuperTokenCreatedEvent): void {
    let address = event.params.token.toHex();
    let token = fetchToken(address);
    token.save();

    // Create a dynamic data source instance
    // https://thegraph.com/docs/define-a-subgraph#instantiating-a-data-source-template
    SuperToken.create(event.params.token);

    let ev = new SuperTokenCreated(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.address = event.params.token;
    ev.underlyingAddress = token.underlyingAddress;
    ev.symbol = token.symbol;
    ev.name = token.name;
    ev.save();
}
