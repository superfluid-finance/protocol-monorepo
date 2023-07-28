import {
    BufferAdjusted,
    FlowDistributionUpdated,
    InstantDistributionUpdated,
    PoolConnectionUpdated,
    PoolCreated,
} from "../../generated/GeneralDistributionAgreementV1/IGeneralDistributionAgreementV1";
import {
    BufferAdjustedEvent,
    FlowDistributionUpdatedEvent,
    InstantDistributionUpdatedEvent,
    PoolConnectionUpdatedEvent,
    PoolCreatedEvent,
} from "../../generated/schema";
import { SuperfluidPool as SuperfluidPoolTemplate } from "../../generated/templates";
import {
    _createAccountTokenSnapshotLogEntity,
    _createTokenStatisticLogEntity,
    getOrInitPool,
    getOrInitPoolDistributor,
    getOrInitPoolMember,
    getOrInitTokenStatistic,
    updateATSStreamedAndBalanceUntilUpdatedAt,
    updatePoolDistributorTotalAmountFlowedAndDistributed,
    updatePoolTotalAmountFlowedAndDistributed,
    updateTokenStatsStreamedUntilUpdatedAt,
} from "../mappingHelpers";
import { BIG_INT_ZERO, createEventID, initializeEventEntity } from "../utils";

// @note use deltas where applicable

export function handlePoolCreated(event: PoolCreated): void {
    const eventName = "PoolCreated";
    // Create Event Entity
    _createPoolCreatedEntity(event);

    const pool = getOrInitPool(event, event.params.pool.toHex());
    pool.token = event.params.token.toHex();
    pool.admin = event.params.admin.toHex();
    pool.save();

    // Note: this is necessary otherwise we will not be able to capture
    // template data source events.
    SuperfluidPoolTemplate.create(event.params.pool);

    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);

    const tokenStatistic = getOrInitTokenStatistic(
        event.params.token,
        event.block
    );
    tokenStatistic.totalNumberOfPools = tokenStatistic.totalNumberOfPools + 1;

    tokenStatistic.save();

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.admin,
        event.params.token,
        event.block,
        null
    );

    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.admin,
        event.params.token,
        eventName
    );

    _createTokenStatisticLogEntity(event, event.params.token, eventName);
}

export function handlePoolConnectionUpdated(
    event: PoolConnectionUpdated
): void {
    // Create Event Entity
    _createPoolConnectionUpdatedEntity(event);

    // Update Pool Member Entity
    let poolMember = getOrInitPoolMember(
        event,
        event.params.pool,
        event.params.account
    );
    const previousIsConnected = poolMember.isConnected;
    const memberConnectedStatusUpdated =
        previousIsConnected !== event.params.connected;
    poolMember.isConnected = event.params.connected;
    poolMember.save();

    // Update Pool
    let pool = getOrInitPool(event, event.params.pool.toHex());
    pool = updatePoolTotalAmountFlowedAndDistributed(event, pool);
    if (poolMember.units.gt(BIG_INT_ZERO)) {
        if (memberConnectedStatusUpdated) {
            // disconnected -> connected case
            if (event.params.connected) {
                pool.totalConnectedUnits = pool.totalConnectedUnits.plus(
                    poolMember.units
                );
                pool.totalDisconnectedUnits = pool.totalDisconnectedUnits.minus(
                    poolMember.units
                );
                pool.totalConnectedMembers = pool.totalConnectedMembers + 1;
                pool.totalDisconnectedMembers =
                    pool.totalDisconnectedMembers - 1;
            } else {
                // connected -> disconnected case
                pool.totalConnectedUnits = pool.totalConnectedUnits.minus(
                    poolMember.units
                );
                pool.totalDisconnectedUnits = pool.totalDisconnectedUnits.plus(
                    poolMember.units
                );
                pool.totalConnectedMembers = pool.totalConnectedMembers - 1;
                pool.totalDisconnectedMembers =
                    pool.totalDisconnectedMembers + 1;
            }
        }
    }
    pool.save();

    _handlePoolConnectionUpdatedAggregateEntities(
        event,
        memberConnectedStatusUpdated
    );
}

function _handlePoolConnectionUpdatedAggregateEntities(
    event: PoolConnectionUpdated,
    memberConnectedStatusUpdated: boolean
): void {
    const eventName = "PoolConnectionUpdated";
    // Update Aggregate
    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);

    const tokenStatistic = getOrInitTokenStatistic(
        event.params.token,
        event.block
    );

    if (memberConnectedStatusUpdated) {
        if (event.params.connected) {
            tokenStatistic.totalConnectedMemberships =
                tokenStatistic.totalConnectedMemberships + 1;
        } else {
            tokenStatistic.totalConnectedMemberships =
                tokenStatistic.totalConnectedMemberships - 1;
        }
    }

    tokenStatistic.save();

    // TODO: @note we need updateAggregateIDASubscriptionsData equivalent
    // for GDA here as well...

    updateATSStreamedAndBalanceUntilUpdatedAt(
        event.params.account,
        event.params.token,
        event.block,
        null
    );

    _createAccountTokenSnapshotLogEntity(
        event,
        event.params.account,
        event.params.token,
        eventName
    );

    _createTokenStatisticLogEntity(event, event.params.token, eventName);
}

export function handleBufferAdjusted(event: BufferAdjusted): void {
    // Create Event Entity
    _createBufferAdjustedEntity(event);

    // Update Pool Distributor
    let poolDistributor = getOrInitPoolDistributor(
        event,
        event.params.pool,
        event.params.from
    );
    poolDistributor = updatePoolDistributorTotalAmountFlowedAndDistributed(
        event,
        poolDistributor
    );
    poolDistributor.totalBuffer = event.params.newBufferAmount;
    poolDistributor.save();

    // Update Pool
    let pool = getOrInitPool(event, event.params.pool.toHex());
    pool = updatePoolTotalAmountFlowedAndDistributed(event, pool);
    pool.totalBuffer = pool.totalBuffer.plus(event.params.bufferDelta);
    pool.save();

    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}

export function handleFlowDistributionUpdated(
    event: FlowDistributionUpdated
): void {
    // Create Event Entity
    _createFlowDistributionUpdatedEntity(event);

    // Update Pool Distributor
    let poolDistributor = getOrInitPoolDistributor(
        event,
        event.params.pool,
        event.params.distributor
    );
    poolDistributor = updatePoolDistributorTotalAmountFlowedAndDistributed(
        event,
        poolDistributor
    );
    poolDistributor.flowRate = event.params.newDistributorToPoolFlowRate;
    poolDistributor.save();

    // Update Pool
    let pool = getOrInitPool(event, event.params.pool.toHex());
    pool = updatePoolTotalAmountFlowedAndDistributed(event, pool);
    pool.flowRate = event.params.newTotalDistributionFlowRate;
    pool.adjustmentFlowRate = event.params.adjustmentFlowRate;
    pool.save();

    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}

export function handleInstantDistributionUpdated(
    event: InstantDistributionUpdated
): void {
    // Create Event Entity
    _createInstantDistributionUpdatedEntity(event);

    // Update Pool Distributor
    let poolDistributor = getOrInitPoolDistributor(
        event,
        event.params.pool,
        event.params.distributor
    );
    poolDistributor = updatePoolDistributorTotalAmountFlowedAndDistributed(
        event,
        poolDistributor
    );
    poolDistributor.totalAmountInstantlyDistributedUntilUpdatedAt =
        poolDistributor.totalAmountInstantlyDistributedUntilUpdatedAt.plus(
            event.params.actualAmount
        );
    poolDistributor.totalAmountDistributedUntilUpdatedAt =
        poolDistributor.totalAmountDistributedUntilUpdatedAt.plus(
            event.params.actualAmount
        );
    poolDistributor.save();

    // Update Pool
    let pool = getOrInitPool(event, event.params.pool.toHex());
    pool = updatePoolTotalAmountFlowedAndDistributed(event, pool);
    pool.totalAmountInstantlyDistributedUntilUpdatedAt =
        pool.totalAmountInstantlyDistributedUntilUpdatedAt.plus(
            event.params.actualAmount
        );
    pool.totalAmountDistributedUntilUpdatedAt =
        pool.totalAmountDistributedUntilUpdatedAt.plus(
            event.params.actualAmount
        );
    pool.save();

    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}

// Event Entity Creation Functions

function _createPoolCreatedEntity(event: PoolCreated): PoolCreatedEvent {
    const ev = new PoolCreatedEvent(createEventID("PoolCreated", event));
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.pool,
        event.transaction.from,
        event.params.admin,
    ]);

    ev.token = event.params.token;
    ev.caller = event.transaction.from;
    ev.admin = event.params.admin;
    ev.pool = event.params.pool.toHex();

    ev.save();

    return ev;
}

function _createPoolConnectionUpdatedEntity(
    event: PoolConnectionUpdated
): PoolConnectionUpdatedEvent {
    const ev = new PoolConnectionUpdatedEvent(
        createEventID("PoolConnectionUpdated", event)
    );
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.pool,
        event.params.account,
    ]);

    ev.token = event.params.token;
    ev.connected = event.params.connected;
    ev.pool = event.params.pool.toHex();
    ev.poolMember = event.params.account.toHex();

    ev.save();

    return ev;
}

function _createBufferAdjustedEntity(
    event: BufferAdjusted
): BufferAdjustedEvent {
    const ev = new BufferAdjustedEvent(createEventID("BufferAdjusted", event));
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.pool,
        event.params.from,
    ]);

    ev.token = event.params.token;
    ev.bufferDelta = event.params.bufferDelta;
    ev.newBufferAmount = event.params.newBufferAmount;
    ev.totalBufferAmount = event.params.totalBufferAmount;
    ev.pool = event.params.pool.toHex();
    ev.poolDistributor = event.params.from.toHex();

    ev.save();

    return ev;
}

function _createInstantDistributionUpdatedEntity(
    event: InstantDistributionUpdated
): InstantDistributionUpdatedEvent {
    const ev = new InstantDistributionUpdatedEvent(
        createEventID("InstantDistributionUpdated", event)
    );
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.pool,
        event.params.distributor,
        event.params.operator,
    ]);

    ev.token = event.params.token;
    ev.operator = event.params.operator;
    ev.requestedAmount = event.params.requestedAmount;
    ev.actualAmount = event.params.actualAmount;
    ev.pool = event.params.pool.toHex();
    ev.poolDistributor = event.params.distributor.toHex();

    ev.save();

    return ev;
}

function _createFlowDistributionUpdatedEntity(
    event: FlowDistributionUpdated
): FlowDistributionUpdatedEvent {
    const ev = new FlowDistributionUpdatedEvent(
        createEventID("FlowDistributionUpdated", event)
    );
    initializeEventEntity(ev, event, [
        event.params.token,
        event.params.pool,
        event.params.distributor,
        event.params.operator,
    ]);

    ev.token = event.params.token;
    ev.operator = event.params.operator;
    ev.oldFlowRate = event.params.oldFlowRate;
    ev.newDistributorToPoolFlowRate = event.params.newDistributorToPoolFlowRate;
    ev.newTotalDistributionFlowRate = event.params.newTotalDistributionFlowRate;
    ev.adjustmentFlowRecipient = event.params.adjustmentFlowRecipient;
    ev.adjustmentFlowRate = event.params.adjustmentFlowRate;
    ev.pool = event.params.pool.toHex();
    ev.poolDistributor = event.params.distributor.toHex();

    ev.save();

    return ev;
}
