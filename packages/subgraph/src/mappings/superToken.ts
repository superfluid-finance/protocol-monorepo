import {
    TokenUpgraded as TokenUpgradedEvent,
    TokenDowngraded as TokenDowngradedEvent,
    Transfer as TransferEvent,
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
} from "../../generated/templates/SuperToken/ISuperToken";
import {
    TokenUpgraded,
    TokenDowngraded,
    Transfer,
    AgreementLiquidatedBy,
} from "../../generated/schema";
import {
    createEventID,
    createOrUpdateAccount,
    updateATSBalance,
} from "../utils";

export function handleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    let ev = new AgreementLiquidatedBy(createEventID(event));
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = event.address.toHex();
    ev.liquidatorAccount = event.params.liquidatorAccount;
    ev.agreementClass = event.params.agreementClass;
    ev.agreementId = event.params.id;
    ev.penaltyAccount = event.params.penaltyAccount;
    ev.bondAccount = event.params.bondAccount;
    ev.rewardAmount = event.params.rewardAmount;
    ev.bailoutAmount = event.params.bailoutAmount;
    ev.save();

    const accounts = [
        event.params.liquidatorAccount,
        event.params.penaltyAccount,
        event.params.bondAccount,
    ];
    for (let i = 0; i < accounts.length; i++) {
        let account = createOrUpdateAccount(
            accounts[i].toHex(),
            event.block.timestamp
        );
        account.save();
        updateATSBalance(account.id, event.address.toHex());
    }
}

export function handleTokenUpgraded(event: TokenUpgradedEvent): void {
    let ev = new TokenUpgraded(createEventID(event));
    let amount = event.params.amount;
    let tokenId = event.address.toHex();

    ev.account = event.params.account;
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = tokenId;
    ev.amount = amount;
    ev.save();

    let account = createOrUpdateAccount(
        event.params.account.toHex(),
        event.block.timestamp
    );
    updateATSBalance(account.id, tokenId);
}

export function handleTokenDowngraded(event: TokenDowngradedEvent): void {
    let ev = new TokenDowngraded(createEventID(event));
    let amount = event.params.amount;
    let tokenId = event.address.toHex();

    ev.account = event.params.account;
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = tokenId;
    ev.amount = amount;
    ev.save();

    let account = createOrUpdateAccount(
        event.params.account.toHex(),
        event.block.timestamp
    );
    updateATSBalance(account.id, tokenId);
}

export function handleTransfer(event: TransferEvent): void {
    let ev = new Transfer(createEventID(event));
    let value = event.params.value;
    let tokenId = event.address.toHex();

    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.from = event.params.from;
    ev.to = event.params.to;
    ev.value = value;
    ev.token = tokenId;
    ev.save();

    let fromAccount = createOrUpdateAccount(
        event.params.from.toHex(),
        event.block.timestamp
    );
    let toAccount = createOrUpdateAccount(
        event.params.to.toHex(),
        event.block.timestamp
    );
    fromAccount.save();
    toAccount.save();
    updateATSBalance(toAccount.id, tokenId);
    updateATSBalance(fromAccount.id, tokenId);
}
