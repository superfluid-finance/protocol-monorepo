import {
    TokenUpgraded,
    TokenDowngraded,
    Transfer,
    AgreementLiquidatedBy,
    Burned,
    Minted,
    Sent,
} from "../../generated/templates/SuperToken/ISuperToken";
import {
    AgreementLiquidatedByEvent,
    BurnedEvent,
    MintedEvent,
    TokenUpgradedEvent,
    TokenDowngradedEvent,
    TransferEvent,
    SentEvent,
} from "../../generated/schema";
import { createEventID, tokenHasValidHost } from "../utils";
import {
    getOrInitAccount,
    getOrInitSuperToken,
    getOrInitTokenStatistic,
    updateAggregateEntitiesTransferData,
    updateATSStreamedAndBalanceUntilUpdatedAt,
    updateTokenStatsStreamedUntilUpdatedAt,
} from "../mappingHelpers";
import { getHostAddress } from "../addresses";

export function handleAgreementLiquidatedBy(
    event: AgreementLiquidatedBy
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createAgreementLiquidatedByEntity(event);

    let liquidatorAccount = getOrInitAccount(
        event.params.liquidatorAccount,
        event.block
    );
    let penaltyAccount = getOrInitAccount(
        event.params.penaltyAccount,
        event.block
    );
    let bondAccount = getOrInitAccount(event.params.bondAccount, event.block);

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        liquidatorAccount.id,
        event.address.toHex(),
        event.block
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        penaltyAccount.id,
        event.address.toHex(),
        event.block
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        bondAccount.id,
        event.address.toHex(),
        event.block
    );
}

export function handleTokenUpgraded(event: TokenUpgraded): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTokenUpgradedEntity(event);

    let account = getOrInitAccount(event.params.account, event.block);

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        account.id,
        event.address.toHex(),
        event.block
    );
}

export function handleTokenDowngraded(event: TokenDowngraded): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTokenDowngradedEntity(event);

    let account = getOrInitAccount(event.params.account, event.block);

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        account.id,
        event.address.toHex(),
        event.block
    );
}

export function handleTransfer(event: Transfer): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTransferEntity(event);

    let fromAccount = getOrInitAccount(event.params.from, event.block);
    let toAccount = getOrInitAccount(event.params.to, event.block);
    let tokenId = event.address.toHex();

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        toAccount.id,
        event.address.toHex(),
        event.block
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        fromAccount.id,
        event.address.toHex(),
        event.block
    );
    updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

    updateAggregateEntitiesTransferData(
        event.params.from.toHex(),
        tokenId,
        event.params.value,
        event.block
    );
}

export function handleSent(event: Sent): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createSentEntity(event);
}

/**
 * This always gets called with the Transfer event, which handles
 * a lot of the logic with the Token, Account, ATS and TokenStatistic
 * entities.
 * @param event
 */
export function handleBurned(event: Burned): void {
    createBurnedEntity(event);
    let tokenStats = getOrInitTokenStatistic(
        event.address.toHex(),
        event.block
    );

    tokenStats.totalSupply = tokenStats.totalSupply.minus(event.params.amount);
    tokenStats.save();
}

/**
 * This always gets called with the Transfer event, which handles
 * a lot of the logic with the Token, Account, ATS and TokenStatistic
 * entities.
 * @param event
 */
export function handleMinted(event: Minted): void {
    createMintedEntity(event);
    let tokenStats = getOrInitTokenStatistic(
        event.address.toHex(),
        event.block
    );

    tokenStats.totalSupply = tokenStats.totalSupply.plus(event.params.amount);
    tokenStats.save();
}

/**************************************************************************
 * Create Event Entity Helper Functions
 *************************************************************************/
function createAgreementLiquidatedByEntity(event: AgreementLiquidatedBy): void {
    let ev = new AgreementLiquidatedByEvent(createEventID(event));
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

function createBurnedEntity(event: Burned): void {
    let ev = new BurnedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.operator = event.params.operator;
    ev.from = event.params.from;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operatorData = event.params.operatorData;
    ev.save();
}

function createMintedEntity(event: Minted): void {
    let ev = new MintedEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.operator = event.params.operator;
    ev.to = event.params.to;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operatorData = event.params.operatorData;
    ev.save();
}

function createSentEntity(event: Sent): void {
    let ev = new SentEvent(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operator = event.params.operator;
    ev.operatorData = event.params.operatorData;
    ev.to = event.params.to;
    ev.save();
}

function createTokenUpgradedEntity(event: TokenUpgraded): void {
    let ev = new TokenUpgradedEvent(createEventID(event));
    ev.account = event.params.account.toHex();
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function createTokenDowngradedEntity(event: TokenDowngraded): void {
    let ev = new TokenDowngradedEvent(createEventID(event));
    ev.account = event.params.account.toHex();
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function createTransferEntity(event: Transfer): void {
    let ev = new TransferEvent(createEventID(event));
    let value = event.params.value;
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.from = event.params.from.toHex();
    ev.to = event.params.to.toHex();
    ev.value = value;
    ev.token = event.address;
    ev.save();
}
