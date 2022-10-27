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
    initializeEventEntity,
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
    const eventId = createEventID("AgreementLiquidatedBy", event);
    const ev = new AgreementLiquidatedByEvent(eventId);
    initializeEventEntity(ev, event, [
        event.address,
        event.params.liquidatorAccount,
        event.params.penaltyAccount,
        event.params.bondAccount,
    ]) as AgreementLiquidatedByEvent;

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
    const eventId = createEventID("AgreementLiquidatedV2", event);
    const ev = new AgreementLiquidatedV2Event(eventId);
    initializeEventEntity(ev, event, [
        event.address,
        event.params.liquidatorAccount,
        event.params.targetAccount,
        event.params.rewardAmountReceiver,
    ]);

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
    const eventId = createEventID("Burned", event);
    const ev = new BurnedEvent(eventId);
    initializeEventEntity(ev, event, [event.address, event.params.from]);

    ev.token = event.address;
    ev.operator = event.params.operator;
    ev.from = event.params.from;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operatorData = event.params.operatorData;
    ev.save();
}

function _createMintedEventEntity(event: Minted): void {
    const eventId = createEventID("Minted", event);
    const ev = new MintedEvent(eventId);
    initializeEventEntity(ev, event, [
        event.address,
        event.params.operator,
        event.params.to,
    ]);

    ev.token = event.address;
    ev.operator = event.params.operator;
    ev.to = event.params.to;
    ev.amount = event.params.amount;
    ev.data = event.params.data;
    ev.operatorData = event.params.operatorData;
    ev.save();
}

function _createSentEventEntity(event: Sent): void {
    const eventId = createEventID("Sent", event);
    const ev = new SentEvent(eventId);
    initializeEventEntity(ev, event, [
        event.address,
        event.params.operator,
        event.params.to,
    ]);
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
    const eventId = createEventID("TokenUpgraded", event);
    const ev = new TokenUpgradedEvent(eventId);
    initializeEventEntity(ev, event, [event.address, event.params.account]);

    ev.account = event.params.account.toHex();
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function _createTokenDowngradedEventEntity(event: TokenDowngraded): void {
    const eventId = createEventID("TokenDowngraded", event);
    const ev = new TokenDowngradedEvent(eventId);
    initializeEventEntity(ev, event, [event.address, event.params.account]);
    ev.account = event.params.account.toHex();
    ev.token = event.address;
    ev.amount = event.params.amount;
    ev.save();
}

function _createTransferEventEntity(event: Transfer): void {
    const eventId = createEventID("Transfer", event);
    const ev = new TransferEvent(eventId);
    initializeEventEntity(ev, event, [
        event.address,
        event.params.from,
        event.params.to,
    ]);
    ev.from = event.params.from.toHex();
    ev.to = event.params.to.toHex();
    ev.value = event.params.value;
    ev.token = event.address;
    ev.save();
}
