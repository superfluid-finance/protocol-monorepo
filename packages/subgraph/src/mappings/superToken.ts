import {
    AgreementLiquidatedBy,
    AgreementLiquidatedV2,
    Burned,
    Minted,
    Sent,
    TokenDowngraded,
    TokenUpgraded,
    Transfer,
} from "../../generated/templates/SuperToken/ISuperToken";
import {
    AgreementLiquidatedByEvent,
    AgreementLiquidatedV2Event,
    BurnedEvent,
    MintedEvent,
    SentEvent,
    TokenDowngradedEvent,
    TokenUpgradedEvent,
    TransferEvent,
} from "../../generated/schema";
import {
    createEventID,
    getOrder,
    tokenHasValidHost,
    ZERO_ADDRESS,
} from "../utils";
import {
    createAccountTokenSnapshotLogEntity,
    getOrInitAccount,
    getOrInitSuperToken,
    getOrInitTokenStatistic,
    updateAggregateEntitiesTransferData,
    updateATSStreamedAndBalanceUntilUpdatedAt,
    updateTokenStatsStreamedUntilUpdatedAt,
} from "../mappingHelpers";
import { getHostAddress } from "../addresses";
import { Address, BigInt, ethereum, log } from "@graphprotocol/graph-ts";

function updateHOLEntitiesForLiquidation(
    event: ethereum.Event,
    liquidatorAccount: Address,
    targetAccount: Address,
    bondAccount: Address,
    eventName: string
): void {
    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        liquidatorAccount,
        event.address,
        event.block
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        targetAccount,
        event.address,
        event.block
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        bondAccount,
        event.address,
        event.block
    );
    updateTokenStatsStreamedUntilUpdatedAt(event.address, event.block);
    createAccountTokenSnapshotLogEntity(
        event,
        targetAccount,
        event.address,
        eventName
    );
    createAccountTokenSnapshotLogEntity(
        event,
        liquidatorAccount,
        event.address,
        eventName
    );
    createAccountTokenSnapshotLogEntity(
        event,
        bondAccount,
        event.address,
        eventName
    );
}

export function handleAgreementLiquidatedBy(
    event: AgreementLiquidatedBy
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createAgreementLiquidatedByEntity(event);

    updateHOLEntitiesForLiquidation(
        event,
        event.params.liquidatorAccount,
        event.params.penaltyAccount,
        event.params.bondAccount,
        "AgreementLiquidatedBy"
    );
}

export function handleAgreementLiquidatedV2(
    event: AgreementLiquidatedV2
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createAgreementLiquidatedV2Entity(event);

    updateHOLEntitiesForLiquidation(
        event,
        event.params.liquidatorAccount,
        event.params.targetAccount,
        event.params.rewardAmountReceiver,
        "AgreementLiquidatedV2"
    );
}

export function handleTokenUpgraded(event: TokenUpgraded): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTokenUpgradedEntity(event);

    getOrInitAccount(event.params.account, event.block);

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.account,
        event.address,
        event.block
    );
    updateTokenStatsStreamedUntilUpdatedAt(event.address, event.block);
    createAccountTokenSnapshotLogEntity(
        event,
        event.params.account,
        event.address,
        "TokenUpgraded"
    );
}

export function handleTokenDowngraded(event: TokenDowngraded): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTokenDowngradedEntity(event);

    getOrInitAccount(event.params.account, event.block);

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.account,
        event.address,
        event.block
    );
    updateTokenStatsStreamedUntilUpdatedAt(event.address, event.block);
    createAccountTokenSnapshotLogEntity(
        event,
        event.params.account,
        event.address,
        "TokenDowngraded"
    );
}

export function handleTransfer(event: Transfer): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    createTransferEntity(event);

    let tokenId = event.address;

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.to,
        event.address,
        event.block
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.from,
        event.address,
        event.block
    );
    updateTokenStatsStreamedUntilUpdatedAt(tokenId, event.block);

    updateAggregateEntitiesTransferData(
        event.params.from,
        event.address,
        event.params.value,
        event.block
    );

    if (event.params.to.equals(ZERO_ADDRESS)) return;
    if (event.params.from.equals(ZERO_ADDRESS)) return; // Ignoring downgrade and upgrade transfer event logs.
    createAccountTokenSnapshotLogEntity(
        event,
        event.params.to,
        event.address,
        "Transfer"
    );
    createAccountTokenSnapshotLogEntity(
        event,
        event.params.from,
        event.address,
        "Transfer"
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
    let tokenStats = getOrInitTokenStatistic(event.address, event.block);

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
    let tokenStats = getOrInitTokenStatistic(event.address, event.block);

    tokenStats.totalSupply = tokenStats.totalSupply.plus(event.params.amount);
    tokenStats.save();
}

/**************************************************************************
 * Create Event Entity Helper Functions
 *************************************************************************/
function createAgreementLiquidatedByEntity(event: AgreementLiquidatedBy): void {
    let ev = new AgreementLiquidatedByEvent(
        createEventID("AgreementLiquidatedBy", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.name = "AgreementLiquidatedBy";
    ev.addresses = [
        event.address,
        event.params.liquidatorAccount,
        event.params.penaltyAccount,
        event.params.bondAccount,
    ];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
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

function createAgreementLiquidatedV2Entity(event: AgreementLiquidatedV2): void {
    let ev = new AgreementLiquidatedV2Event(
        createEventID("AgreementLiquidatedV2", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.name = "AgreementLiquidatedV2";
    ev.addresses = [
        event.address,
        event.params.liquidatorAccount,
        event.params.targetAccount,
        event.params.rewardAmountReceiver,
    ];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.address;
    ev.liquidatorAccount = event.params.liquidatorAccount;
    ev.agreementClass = event.params.agreementClass;
    ev.agreementId = event.params.id;
    ev.targetAccount = event.params.targetAccount;
    ev.rewardAmountReceiver = event.params.rewardAmountReceiver;
    ev.rewardAccount = event.params.rewardAmountReceiver;
    ev.rewardAmount = event.params.rewardAmount;
    ev.targetAccountBalanceDelta = event.params.targetAccountBalanceDelta;

    let decoded = ethereum.decode(
        "(uint256,uint256)",
        event.params.liquidationTypeData
    ) as ethereum.Value;
    let tuple = decoded.toTuple();
    let version = tuple[0].toBigInt();
    let liquidationType = tuple[1].toI32();
    if (version != BigInt.fromI32(1)) {
        log.error("Version type is incorrect = {}", [version.toString()]);
    }

    // if version is 0, this means that something went wrong
    ev.version = version == BigInt.fromI32(1) ? version : BigInt.fromI32(0);

    ev.liquidationType = liquidationType;
    ev.save();
}

function createBurnedEntity(event: Burned): void {
    let ev = new BurnedEvent(createEventID("Burned", event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.name = "Burned";
    ev.addresses = [event.address, event.params.from];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.operator = event.params.operator;
    ev.from = event.params.from;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operatorData = event.params.operatorData;
    ev.save();
}

function createMintedEntity(event: Minted): void {
    let ev = new MintedEvent(createEventID("Minted", event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.name = "Minted";
    ev.addresses = [event.address, event.params.operator, event.params.to];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.operator = event.params.operator;
    ev.to = event.params.to;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operatorData = event.params.operatorData;
    ev.save();
}

function createSentEntity(event: Sent): void {
    let ev = new SentEvent(createEventID("Sent", event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.name = "Sent";
    ev.addresses = [event.address, event.params.operator, event.params.to];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operator = event.params.operator;
    ev.operatorData = event.params.operatorData;
    ev.from = event.params.from;
    ev.to = event.params.to;
    ev.save();
}

function createTokenUpgradedEntity(event: TokenUpgraded): void {
    let ev = new TokenUpgradedEvent(createEventID("TokenUpgraded", event));
    ev.account = event.params.account.toHex();
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.name = "TokenUpgraded";
    ev.addresses = [event.address, event.params.account];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function createTokenDowngradedEntity(event: TokenDowngraded): void {
    let ev = new TokenDowngradedEvent(createEventID("TokenDowngraded", event));
    ev.account = event.params.account.toHex();
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.name = "TokenDowngraded";
    ev.addresses = [event.address, event.params.account];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function createTransferEntity(event: Transfer): void {
    let ev = new TransferEvent(createEventID("Transfer", event));
    let value = event.params.value;
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.name = "Transfer";
    ev.addresses = [event.address, event.params.from, event.params.to];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.from = event.params.from.toHex();
    ev.to = event.params.to.toHex();
    ev.value = value;
    ev.token = event.address;
    ev.save();
}
