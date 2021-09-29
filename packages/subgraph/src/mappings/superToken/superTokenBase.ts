import { Address } from "@graphprotocol/graph-ts";
import {
    TokenUpgraded as TokenUpgradedEvent,
    TokenDowngraded as TokenDowngradedEvent,
    Transfer as TransferEvent,
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import {
    TokenUpgraded,
    TokenDowngraded,
    Transfer,
    AgreementLiquidatedBy,
} from "../../../generated/schema";
import {
    createEventID,
    getOrInitAccount,
    getOrInitToken,
    tokenHasValidHost,
    updateAccountUpdatedAt,
    updateAggregateEntitiesTransferData,
    updateATSBalanceAndUpdatedAt,
    updateATSStreamedUntilUpdatedAt,
    updateTokenStatsStreamedUntilUpdatedAt,
} from "../../utils";

export function handleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createAgreementLiquidatedByEntity(event);

    let liquidatorAccount = getOrInitAccount(
        hostAddress,
        event.params.liquidatorAccount,
        event.block
    );
    let penaltyAccount = getOrInitAccount(
        hostAddress,
        event.params.penaltyAccount,
        event.block
    );
    let bondAccount = getOrInitAccount(
        hostAddress,
        event.params.bondAccount,
        event.block
    );

    updateAccountUpdatedAt(
        hostAddress,
        event.params.liquidatorAccount,
        event.block
    );
    updateAccountUpdatedAt(
        hostAddress,
        event.params.penaltyAccount,
        event.block
    );
    updateAccountUpdatedAt(hostAddress, event.params.bondAccount, event.block);

    getOrInitToken(event.address, event.block);

    updateATSStreamedUntilUpdatedAt(
        liquidatorAccount.id,
        event.address.toHex(),
        event.block
    );
    updateATSStreamedUntilUpdatedAt(
        penaltyAccount.id,
        event.address.toHex(),
        event.block
    );
    updateATSStreamedUntilUpdatedAt(
        bondAccount.id,
        event.address.toHex(),
        event.block
    );

    updateATSBalanceAndUpdatedAt(
        liquidatorAccount.id,
        event.address.toHex(),
        event.block
    );
    updateATSBalanceAndUpdatedAt(
        penaltyAccount.id,
        event.address.toHex(),
        event.block
    );
    updateATSBalanceAndUpdatedAt(
        bondAccount.id,
        event.address.toHex(),
        event.block
    );
}

export function handleTokenUpgraded(
    event: TokenUpgradedEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTokenUpgradedEntity(event);

    let account = getOrInitAccount(
        hostAddress,
        event.params.account,
        event.block
    );

    getOrInitToken(event.address, event.block);

    let tokenId = event.address.toHex();
    updateAccountUpdatedAt(hostAddress, event.params.account, event.block);

    updateATSStreamedUntilUpdatedAt(
        account.id,
        event.address.toHex(),
        event.block
    );

    updateATSBalanceAndUpdatedAt(account.id, tokenId, event.block);
}

export function handleTokenDowngraded(
    event: TokenDowngradedEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTokenDowngradedEntity(event);

    let account = getOrInitAccount(
        hostAddress,
        event.params.account,
        event.block
    );

    getOrInitToken(event.address, event.block);

    let tokenId = event.address.toHex();
    updateAccountUpdatedAt(hostAddress, event.params.account, event.block);
    updateATSStreamedUntilUpdatedAt(
        account.id,
        event.address.toHex(),
        event.block
    );
    updateATSBalanceAndUpdatedAt(account.id, tokenId, event.block);
}

export function handleTransfer(
    event: TransferEvent,
    hostAddress: Address
): void {
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTransferEntity(event);

    let fromAccount = getOrInitAccount(
        hostAddress,
        event.params.from,
        event.block
    );
    let toAccount = getOrInitAccount(hostAddress, event.params.to, event.block);
    let tokenId = event.address.toHex();

    getOrInitToken(event.address, event.block);

    updateAccountUpdatedAt(hostAddress, event.params.from, event.block);
    updateAccountUpdatedAt(hostAddress, event.params.to, event.block);

    updateATSStreamedUntilUpdatedAt(
        toAccount.id,
        event.address.toHex(),
        event.block
    );
    updateATSStreamedUntilUpdatedAt(
        fromAccount.id,
        event.address.toHex(),
        event.block
    );
    updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

    updateATSBalanceAndUpdatedAt(toAccount.id, tokenId, event.block);
    updateATSBalanceAndUpdatedAt(fromAccount.id, tokenId, event.block);

    updateAggregateEntitiesTransferData(
        event.params.from.toHex(),
        tokenId,
        event.params.value,
        event.block
    );
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
