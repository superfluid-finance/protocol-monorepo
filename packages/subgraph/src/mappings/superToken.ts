import {
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
    fetchAccount,
    updateBalance,
} from "../utils";

export function handleTokenUpgraded(event: TokenUpgradedEvent): void {
    let account = fetchAccount(event.params.account.toHex());
    account.save();
    let amount = event.params.amount;
    let tokenId = event.address.toHex();

    let ev = new TokenUpgraded(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.account = account.id;
    ev.amount = amount;
    ev.token = tokenId;
    ev.save();

    updateBalance(account.id, tokenId);
}

export function handleTokenDowngraded(event: TokenDowngradedEvent): void {
    let account = fetchAccount(event.params.account.toHex());
    account.save();
    let amount = event.params.amount;
    let tokenId = event.address.toHex();

    let ev = new TokenDowngraded(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.account = account.id;
    ev.amount = amount;
    ev.token = tokenId;
    ev.save();

    updateBalance(account.id, tokenId);
}

export function handleTransfer(event: TransferEvent): void {
    let fromAccount = fetchAccount(event.params.from.toHex());
    let toAccount = fetchAccount(event.params.to.toHex());
    fromAccount.save();
    toAccount.save();

    let value = event.params.value;
    let tokenId = event.address.toHex();

    let ev = new TokenTransfer(createEventID(event));
    ev.transaction = logTransaction(event).id;
    ev.from = fromAccount.id.concat("-").concat(tokenId);
    ev.to = toAccount.id.concat("-").concat(tokenId);
    ev.value = value;
    ev.token = tokenId;
    ev.save();

    updateBalance(toAccount.id, tokenId);
    updateBalance(fromAccount.id, tokenId);
}
