import { dataSource } from "@graphprotocol/graph-ts";

import {
    // ISuperToken as SuperToken,
    TokenUpgraded as TokenUpgradedEvent,
    TokenDowngraded as TokenDowngradedEvent,
} from "../../generated/templates/SuperToken/ISuperToken";

import { TokenUpgraded, TokenDowngraded } from "../../generated/schema";

import {
    createEventID,
    logTransaction,
    fetchToken,
    fetchAccount,
} from "../utils";

export function handleTokenUpgraded(event: TokenUpgradedEvent): void {
    let account = fetchAccount(event.params.account.toHex());
    account.save();
    let amount = event.params.amount;

    let ev = new TokenUpgraded(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.account = account.id;
    ev.amount = amount;
    ev.token = dataSource.address().toHex();
    ev.save();
}

export function handleTokenDowngraded(event: TokenDowngradedEvent): void {
    let account = fetchAccount(event.params.account.toHex());
    let amount = event.params.amount;

    let ev = new TokenDowngraded(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.account = account.id;
    ev.amount = amount;
    ev.token = dataSource.address().toHex();
    ev.save();
}
