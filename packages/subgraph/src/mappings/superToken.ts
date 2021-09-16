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

    let currentTimestamp = event.block.timestamp;
    let liquidatorAccount = getOrInitAccount(
        event,
        event.params.liquidatorAccount,
        currentTimestamp
    );
    let penaltyAccount = getOrInitAccount(
        event,
        event.params.penaltyAccount,
        currentTimestamp
    );
    let bondAccount = getOrInitAccount(
        event,
        event.params.bondAccount,
        currentTimestamp
    );
    updateATSBalance(
        liquidatorAccount.id,
        event.address.toHex(),
        currentTimestamp
    );
    updateATSBalance(
        penaltyAccount.id,
        event.address.toHex(),
        currentTimestamp
    );
    updateATSBalance(bondAccount.id, event.address.toHex(), currentTimestamp);
}

export function handleTokenUpgraded(event: TokenUpgradedEvent): void {
    createTokenUpgradedEntity(event);

    let currentTimestamp = event.block.timestamp;
    let account = getOrInitAccount(
        event,
        event.params.account,
        currentTimestamp
    );
    let tokenId = event.address.toHex();
    updateATSBalance(account.id, tokenId, currentTimestamp);
}

export function handleTokenDowngraded(event: TokenDowngradedEvent): void {
    createTokenDowngradedEntity(event);

    let currentTimestamp = event.block.timestamp;
    let account = getOrInitAccount(
        event,
        event.params.account,
        currentTimestamp
    );
    let tokenId = event.address.toHex();
    updateATSBalance(account.id, tokenId, currentTimestamp);
}

export function handleTransfer(event: TransferEvent): void {
    createTransferEntity(event);

    let currentTimestamp = event.block.timestamp;
    let fromAccount = getOrInitAccount(
        event,
        event.params.from,
        currentTimestamp
    );
    let toAccount = getOrInitAccount(event, event.params.to, currentTimestamp);
    let tokenId = event.address.toHex();
    updateATSBalance(toAccount.id, tokenId, currentTimestamp);
    updateATSBalance(fromAccount.id, tokenId, currentTimestamp);
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
