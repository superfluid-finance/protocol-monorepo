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
    BIG_INT_ZERO,
    createEventID,
    getOrder,
    tokenHasValidHost,
    ZERO_ADDRESS,
} from "../utils";
import {
    _createAccountTokenSnapshotLogEntity,
    _createTokenStatisticLogEntity,
    getOrInitAccount,
    getOrInitSuperToken,
    getOrInitTokenStatistic,
    updateAggregateEntitiesTransferData,
    updateATSStreamedAndBalanceUntilUpdatedAt,
    updateTokenStatsStreamedUntilUpdatedAt,
} from "../mappingHelpers";
import { getHostAddress } from "../addresses";
import { Address, BigInt, ethereum, log } from "@graphprotocol/graph-ts";

/******************
 * Event Handlers *
 *****************/
export function handleAgreementLiquidatedBy(
    event: AgreementLiquidatedBy
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    _createAgreementLiquidatedByEventEntity(event);

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

    _createAgreementLiquidatedV2EventEntity(event);

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

    _createTokenUpgradedEventEntity(event);

    getOrInitAccount(event.params.account, event.block);

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.account,
        event.address,
        event.block,
        null // will always do final RPC - override accounting done in handleTransfer
    );
    updateTokenStatsStreamedUntilUpdatedAt(event.address, event.block);
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.account,
        event.address,
        "TokenUpgraded"
    );
    _createTokenStatisticLogEntity(event, event.address, "TokenUpgraded");
}

export function handleTokenDowngraded(event: TokenDowngraded): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    _createTokenDowngradedEventEntity(event);

    getOrInitAccount(event.params.account, event.block);

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.account,
        event.address,
        event.block,
        null // will always do final RPC - override accounting done in handleTransfer
    );
    updateTokenStatsStreamedUntilUpdatedAt(event.address, event.block);
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.account,
        event.address,
        "TokenDowngraded"
    );
    _createTokenStatisticLogEntity(event, event.address, "TokenDowngraded");
}

export function handleTransfer(event: Transfer): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    _createTransferEventEntity(event);

    let tokenId = event.address;

    getOrInitSuperToken(event.address, event.block);

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.to,
        event.address,
        event.block,
        null // manual accounting (overriden in upgrade/downgrade)
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.from,
        event.address,
        event.block,
        null // manual accounting (overriden in upgrade/downgrade)
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
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.to,
        event.address,
        "Transfer"
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.from,
        event.address,
        "Transfer"
    );
    _createTokenStatisticLogEntity(event, event.address, "Transfer");
}

export function handleSent(event: Sent): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.address);
    if (!hasValidHost) {
        return;
    }

    _createSentEventEntity(event);
}

/**
 * This always gets called with the Transfer event, which handles
 * a lot of the logic with the Token, Account, ATS and TokenStatistic
 * entities.
 * @param event
 */
export function handleBurned(event: Burned): void {
    _createBurnedEventEntity(event);
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
    _createMintedEventEntity(event);
    let tokenStats = getOrInitTokenStatistic(event.address, event.block);

    tokenStats.totalSupply = tokenStats.totalSupply.plus(event.params.amount);
    tokenStats.save();
}

/**************************************
 * HOL Entity Updater Helper Function *
 *************************************/
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
        event.block,
        null // will always do RPC - don't want to leak liquidation logic here
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        targetAccount,
        event.address,
        event.block,
        null // will always do RPC - don't want to leak liquidation logic here
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        bondAccount,
        event.address,
        event.block,
        null // will always do RPC - don't want to leak liquidation logic here
    );
    updateTokenStatsStreamedUntilUpdatedAt(event.address, event.block);
    _createAccountTokenSnapshotLogEntity(
        event,
        targetAccount,
        event.address,
        eventName
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        liquidatorAccount,
        event.address,
        eventName
    );
    _createAccountTokenSnapshotLogEntity(
        event,
        bondAccount,
        event.address,
        eventName
    );
    _createTokenStatisticLogEntity(event, event.address, eventName);
}

/****************************************
 * Create Event Entity Helper Functions *
 ***************************************/
function _createAgreementLiquidatedByEventEntity(
    event: AgreementLiquidatedBy
): void {
    let ev = new AgreementLiquidatedByEvent(
        createEventID("AgreementLiquidatedBy", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.gasUsed = event.receipt
        ? (event.receipt as ethereum.TransactionReceipt).gasUsed
        : BIG_INT_ZERO;
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

function _createAgreementLiquidatedV2EventEntity(
    event: AgreementLiquidatedV2
): void {
    let ev = new AgreementLiquidatedV2Event(
        createEventID("AgreementLiquidatedV2", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.gasUsed = event.receipt
        ? (event.receipt as ethereum.TransactionReceipt).gasUsed
        : BIG_INT_ZERO;
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

function _createBurnedEventEntity(event: Burned): void {
    let ev = new BurnedEvent(createEventID("Burned", event));
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.gasUsed = event.receipt
        ? (event.receipt as ethereum.TransactionReceipt).gasUsed
        : BIG_INT_ZERO;
    ev.timestamp = event.block.timestamp;
    ev.name = "Burned";
    ev.addresses = [event.address, event.params.from];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.address;
    ev.operator = event.params.operator;
    ev.from = event.params.from;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operatorData = event.params.operatorData;
    ev.save();
}

function _createMintedEventEntity(event: Minted): void {
    let ev = new MintedEvent(createEventID("Minted", event));
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.gasUsed = event.receipt
        ? (event.receipt as ethereum.TransactionReceipt).gasUsed
        : BIG_INT_ZERO;
    ev.timestamp = event.block.timestamp;
    ev.name = "Minted";
    ev.addresses = [event.address, event.params.operator, event.params.to];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.address;
    ev.operator = event.params.operator;
    ev.to = event.params.to;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operatorData = event.params.operatorData;
    ev.save();
}

function _createSentEventEntity(event: Sent): void {
    let ev = new SentEvent(createEventID("Sent", event));
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.gasUsed = event.receipt
        ? (event.receipt as ethereum.TransactionReceipt).gasUsed
        : BIG_INT_ZERO;
    ev.timestamp = event.block.timestamp;
    ev.name = "Sent";
    ev.addresses = [event.address, event.params.operator, event.params.to];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.token = event.address;
    ev.operator = event.params.operator;
    ev.operatorData = event.params.operatorData;
    ev.from = event.params.from;
    ev.to = event.params.to;
    ev.save();
}

function _createTokenUpgradedEventEntity(event: TokenUpgraded): void {
    let ev = new TokenUpgradedEvent(createEventID("TokenUpgraded", event));
    ev.account = event.params.account.toHex();
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.gasUsed = event.receipt
        ? (event.receipt as ethereum.TransactionReceipt).gasUsed
        : BIG_INT_ZERO;
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

function _createTokenDowngradedEventEntity(event: TokenDowngraded): void {
    let ev = new TokenDowngradedEvent(createEventID("TokenDowngraded", event));
    ev.account = event.params.account.toHex();
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.gasUsed = event.receipt
        ? (event.receipt as ethereum.TransactionReceipt).gasUsed
        : BIG_INT_ZERO;
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

function _createTransferEventEntity(event: Transfer): void {
    let ev = new TransferEvent(createEventID("Transfer", event));
    let value = event.params.value;
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.gasUsed = event.receipt
        ? (event.receipt as ethereum.TransactionReceipt).gasUsed
        : BIG_INT_ZERO;
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
