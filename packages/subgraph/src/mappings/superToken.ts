import { dataSource } from "@graphprotocol/graph-ts";

import {
    // ISuperToken as SuperToken,
    TokenUpgraded as TokenUpgradedEvent,
    TokenDowngraded as TokenDowngradedEvent,
    Transfer as TransferEvent,
} from "../../generated/templates/SuperToken/ISuperToken";

import {
    TokenUpgraded,
    TokenDowngraded,
    TokenTransfer,
} from "../../generated/schema";

import {
    createEventID,
    logTransaction,
    fetchToken,
    fetchAccount,
    updateBalance,
} from "../utils";

export function handleTokenUpgraded(event: TokenUpgradedEvent): void {
    let account = fetchAccount(event.params.account.toHex());
    account.save();
    let amount = event.params.amount;
    let tokenId = dataSource.address().toHex();
    let currentTimestamp = event.block.timestamp;

    let ev = new TokenUpgraded(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.account = account.id;
    ev.amount = amount;
    ev.token = tokenId;
    ev.save();

    // Increase user balance
    updateBalance(
        account.id,
        tokenId,
        amount.toBigDecimal(),
        true,
        currentTimestamp
    );
}

export function handleTokenDowngraded(event: TokenDowngradedEvent): void {
    let account = fetchAccount(event.params.account.toHex());
    let amount = event.params.amount;
    let tokenId = dataSource.address().toHex();
    let currentTimestamp = event.block.timestamp;

    let ev = new TokenDowngraded(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.account = account.id;
    ev.amount = amount;
    ev.token = tokenId;
    ev.save();

    // Decrease user balance
    updateBalance(
        account.id,
        tokenId,
        amount.toBigDecimal(),
        false,
        currentTimestamp
    );
}

export function handleTransfer(event: TransferEvent): void {
    let fromAccount = fetchAccount(event.params.from.toHex());
    let toAccount = fetchAccount(event.params.to.toHex());
    let value = event.params.value;
    let tokenId = dataSource.address().toHex();
    let currentTimestamp = event.block.timestamp;

    let ev = new TokenTransfer(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.from = fromAccount.id;
    ev.to = toAccount.id;
    ev.value = value;
    ev.token = tokenId;
    ev.save();

    // Update user balances
    updateBalance(
        toAccount.id,
        tokenId,
        value.toBigDecimal(),
        true,
        currentTimestamp
    );
    updateBalance(
        fromAccount.id,
        tokenId,
        value.toBigDecimal(),
        false,
        currentTimestamp
    );
}
