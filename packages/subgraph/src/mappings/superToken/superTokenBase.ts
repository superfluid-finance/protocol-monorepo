import { Address, ethereum } from "@graphprotocol/graph-ts";
import {
    TokenUpgraded as TokenUpgradedEvent,
    TokenDowngraded as TokenDowngradedEvent,
    Transfer as TransferEvent,
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
    Burned as BurnedEvent,
    Minted as MintedEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import {
    AgreementLiquidatedBy,
    Burned,
    Minted,
    TokenUpgraded,
    TokenDowngraded,
    Transfer,
} from "../../../generated/schema";
import {
    createEventID,
    getOrInitAccount,
    getOrInitSuperToken,
    getOrInitTokenStatistic,
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

    getOrInitSuperToken(event.address, event.block);

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

    getOrInitSuperToken(event.address, event.block);

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

    getOrInitSuperToken(event.address, event.block);

    let tokenId = event.address.toHex();
    updateAccountUpdatedAt(hostAddress, event.params.account, event.block);
    updateATSStreamedUntilUpdatedAt(
        account.id,
        event.address.toHex(),
        event.block
    );
    updateATSBalanceAndUpdatedAt(account.id, tokenId, event.block);
}

// TODO: mint, burns (upgrade/downgrades) are transfers as well
// as actual minting of the token. I don't think this captures
// what transfer really is, I believe the Sent event is better for this
// Transfer represents the total amount of funds upgrade/downgraded as
// well as minted and sent from one individual to another
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

    getOrInitSuperToken(event.address, event.block);

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

/**
 * This always gets called with the Transfer event, which handles
 * a lot of the logic with the Token, Account, ATS and TokenStatistic
 * entities.
 * @param event
 */
export function handleBurned(event: BurnedEvent): void {
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
export function handleMinted(event: MintedEvent): void {
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

function createBurnedEntity(event: BurnedEvent): void {
    let ev = new Burned(createEventID(event));
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

function createMintedEntity(event: MintedEvent): void {
    let ev = new Minted(createEventID(event));
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

function createTokenUpgradedEntity(event: TokenUpgradedEvent): void {
    let ev = new TokenUpgraded(createEventID(event));
    ev.account = event.params.account.toHex();
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function createTokenDowngradedEntity(event: TokenDowngradedEvent): void {
    let ev = new TokenDowngraded(createEventID(event));
    ev.account = event.params.account.toHex();
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
    ev.from = event.params.from.toHex();
    ev.to = event.params.to.toHex();
    ev.value = value;
    ev.token = event.address;
    ev.save();
}
