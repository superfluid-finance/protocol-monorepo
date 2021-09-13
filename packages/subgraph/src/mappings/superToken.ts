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
import { createEventID, getOrInitAccount, updateATSBalance } from "../utils";

export function handleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    createAgreementLiquidatedByEntity(event);

    let liquidatorAccount = getOrInitAccount(
        event.params.liquidatorAccount.toHex(),
        event.block.timestamp
    );
    let penaltyAccount = getOrInitAccount(
        event.params.penaltyAccount.toHex(),
        event.block.timestamp
    );
    let bondAccount = getOrInitAccount(
        event.params.bondAccount.toHex(),
        event.block.timestamp
    );
    updateATSBalance(liquidatorAccount.id, event.address.toHex());
    updateATSBalance(penaltyAccount.id, event.address.toHex());
    updateATSBalance(bondAccount.id, event.address.toHex());
}

export function handleTokenUpgraded(event: TokenUpgradedEvent): void {
    createTokenUpgradedEntity(event);

    let account = getOrInitAccount(
        event.params.account.toHex(),
        event.block.timestamp
    );
    let tokenId = event.address.toHex();
    updateATSBalance(account.id, tokenId);
}

export function handleTokenDowngraded(event: TokenDowngradedEvent): void {
    createTokenDowngradedEntity(event);

    let account = getOrInitAccount(
        event.params.account.toHex(),
        event.block.timestamp
    );
    let tokenId = event.address.toHex();
    updateATSBalance(account.id, tokenId);
}

export function handleTransfer(event: TransferEvent): void {
    createTransferEntity(event);

    let fromAccount = getOrInitAccount(
        event.params.from.toHex(),
        event.block.timestamp
    );
    let toAccount = getOrInitAccount(
        event.params.to.toHex(),
        event.block.timestamp
    );
    let tokenId = event.address.toHex();
    updateATSBalance(toAccount.id, tokenId);
    updateATSBalance(fromAccount.id, tokenId);
}

/**************************************************************************
 * Create Event Entity Helper Functions
 *************************************************************************/
function createAgreementLiquidatedByEntity(
    event: AgreementLiquidatedByEvent
): void {
    let ev = new AgreementLiquidatedBy(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.address;
    ev.liquidatorAccount = event.params.liquidatorAccount;
    ev.agreementClass = event.params.agreementClass;
    ev.agreementId = event.params.id;
    ev.penaltyAccount = event.params.penaltyAccount;
    ev.bondAccount = event.params.bondAccount;
    ev.rewardAmount = event.params.rewardAmount;
    ev.bailoutAmount = event.params.bailoutAmount;
    ev.save();
}

function createTokenUpgradedEntity(event: TokenUpgradedEvent): void {
    let ev = new TokenUpgraded(createEventID(event));
    ev.account = event.params.account;
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function createTokenDowngradedEntity(event: TokenDowngradedEvent): void {
    let ev = new TokenDowngraded(createEventID(event));
    ev.account = event.params.account;
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function createTransferEntity(event: TransferEvent): void {
    let ev = new Transfer(createEventID(event));
    let value = event.params.value;
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.from = event.params.from;
    ev.to = event.params.to;
    ev.value = value;
    ev.token = event.address;
    ev.save();
}
