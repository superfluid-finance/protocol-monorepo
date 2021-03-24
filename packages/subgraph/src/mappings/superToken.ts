import {
    BigDecimal,
    BigInt,
    EthereumEvent,
    log,
} from "@graphprotocol/graph-ts";

import { Superfluid } from "../generated/Superfluid/Superfluid";

import { SuperTokenCreated as SuperTokenCreatedEvent } from "../generated/Superfluid/ConstantFlowAgreementV1";

import {
    Transaction,
    Account,
    Token,
    SuperTokenCreated,
} from "../generated/schema";

import { fetchToken, fetchAccount, logTransaction, toDai } from "./utils";

export function handleSuperTokenCreated(event: SuperTokenCreatedEvent): void {
    let address = event.params.address.toHex();
    let token = fetchToken(address);
    token.save();

    let ev = new SuperTokenCreated(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.address = address;
    ev.underlyingAddress = token.underlyingAddress;
    ev.symbol = token.symbol;
    ev.name = token.name;
    ev.save();
}
