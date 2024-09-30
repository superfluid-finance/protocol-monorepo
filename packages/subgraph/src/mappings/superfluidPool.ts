import { BigInt } from "@graphprotocol/graph-ts";
import {
    DistributionClaimed,
    MemberUnitsUpdated,
} from "../../generated/GeneralDistributionAgreementV1/ISuperfluidPool";
import { DistributionClaimedEvent, MemberUnitsUpdatedEvent, Pool, PoolMember } from "../../generated/schema";
import {
    _createAccountTokenSnapshotLogEntity,
    _createTokenStatisticLogEntity,
    getOrInitPool,
    getOrInitOrUpdatePoolMember,
    settlePDPoolMemberMU,
    updateATSStreamedAndBalanceUntilUpdatedAt,
    updateAggregateDistributionAgreementData,
    updatePoolParticleAndTotalAmountFlowedAndDistributed,
    updateTokenStatsStreamedUntilUpdatedAt,
    ensureAccountAndPoolInteractionExists,
} from "../mappingHelpers";
import { BIG_INT_ZERO, createEventID, divideOrZero, initializeEventEntity } from "../utils";

// @note use deltas where applicable

export function handleDistributionClaimed(event: DistributionClaimed): void {
    const token = event.params.token;

    // Update Pool
    let pool = getOrInitPool(event, event.address.toHex());
    let poolMember = getOrInitOrUpdatePoolMember(event, event.address, event.params.member);

    const totalAmountReceivedFromPoolBeforeUpdate = poolMember.totalAmountReceivedUntilUpdatedAt;

    // settle pool and pool member
    pool = updatePoolParticleAndTotalAmountFlowedAndDistributed(event, pool);
    settlePDPoolMemberMU(pool, poolMember, event.block);
    
    // Update PoolMember
    poolMember.totalAmountClaimed = event.params.totalClaimed;
    
    pool.save();
    poolMember.save();

    
    // Update Token Statistics
    const eventName = "DistributionClaimed";
    updateTokenStatsStreamedUntilUpdatedAt(token, event.block);
    _createTokenStatisticLogEntity(event, token, eventName);
    
    // Update ATS
    updateATSStreamedAndBalanceUntilUpdatedAt(event.params.member, token, event.block, event.params.claimedAmount);
    _createAccountTokenSnapshotLogEntity(event, event.params.member, token, eventName);
    
    // Create Event Entity
    const amountDeltaReceivedSinceLastUpdate = poolMember.totalAmountReceivedUntilUpdatedAt.minus(totalAmountReceivedFromPoolBeforeUpdate);
    _createDistributionClaimedEntity(event, pool, poolMember);
}

export function handleMemberUnitsUpdated(event: MemberUnitsUpdated): void {
    let pool = getOrInitPool(event, event.address.toHex());
    let poolMember = getOrInitOrUpdatePoolMember(event, event.address, event.params.member);
    const totalAmountReceivedFromPoolBeforeUpdate = poolMember.totalAmountReceivedUntilUpdatedAt;

    const previousUnits = poolMember.units;
    const unitsDelta = event.params.newUnits.minus(previousUnits);
    const oldTotalUnits = pool.totalUnits;
    const oldPerUnitFlowRate = pool.perUnitFlowRate;
    const newTotalUnits = pool.totalUnits.plus(unitsDelta);

    pool = updatePoolParticleAndTotalAmountFlowedAndDistributed(event, pool);
    settlePDPoolMemberMU(pool, poolMember, event.block);

    ensureAccountAndPoolInteractionExists(pool.token, poolMember.account, pool.id, event.block);

    const oldEffectivePoolFlowRate = oldPerUnitFlowRate.times(oldTotalUnits);
    
    const newPerUnitFlowRate = divideOrZero(oldEffectivePoolFlowRate, newTotalUnits);
    pool.perUnitFlowRate = newPerUnitFlowRate;

    const newEffectivePoolFlowRate = newPerUnitFlowRate.times(newTotalUnits); // This will either be equal or less than the previous one.
    pool.adjustmentFlowRate = pool.flowRate.minus(newEffectivePoolFlowRate);
    
    pool.totalUnits = newTotalUnits;
    poolMember.syncedPerUnitFlowRate = newPerUnitFlowRate;
    poolMember.units = event.params.newUnits;

    if (poolMember.isConnected) {
        pool.totalConnectedUnits = pool.totalConnectedUnits.plus(unitsDelta);
    } else {
        pool.totalDisconnectedUnits = pool.totalDisconnectedUnits.plus(unitsDelta);
    }

    // 0 units to > 0 units
    const didPoolMemberBecomeActive = previousUnits.equals(BIG_INT_ZERO) && event.params.newUnits.gt(BIG_INT_ZERO);
    if (didPoolMemberBecomeActive) {
        pool.totalMembers = pool.totalMembers + 1;
        // if the member is connected with units now, we add one to connected
        if (poolMember.isConnected) {
            pool.totalConnectedMembers = pool.totalConnectedMembers + 1;
        } else {
            // if the member is disconnected with units now, we add one to disconnected
            pool.totalDisconnectedMembers = pool.totalDisconnectedMembers + 1;
        }

        updateAggregateDistributionAgreementData(
            event.params.member,
            event.params.token,
            true, // has units
            poolMember.isConnected,
            true, // only place we increment subWithUnits
            false, // not deleting
            false, // not deleting
            false, // not connecting
            event.block,
            false // isIDA
        );
    }

    // > 0 units to 0 units
    const didPoolMemberBecomeInactive = previousUnits.gt(BIG_INT_ZERO) && poolMember.units.equals(BIG_INT_ZERO);
    if (didPoolMemberBecomeInactive) {
        pool.totalMembers = pool.totalMembers - 1;
        // if the member is connected with no units now, we subtract one from connected
        if (poolMember.isConnected) {
            pool.totalConnectedMembers = pool.totalConnectedMembers - 1;
        } else {
            // if the member is disconnected with no units now, we subtract one from disconnected
            pool.totalDisconnectedMembers = pool.totalDisconnectedMembers - 1;
        }

        updateAggregateDistributionAgreementData(
            event.params.member,
            event.params.token,
            false, // has units
            poolMember.isConnected,
            false, // don't increment memberWithUnits
            false, // not disconnecting membership
            true, // only place we decrement membershipWithUnits IF member has memberShipWithUnits
            false, // not connecting
            event.block,
            false // isIDA
        );
    }

    poolMember.save();
    pool.save();

    // Create Event Entity
    const amountDeltaReceivedSinceLastUpdate = poolMember.totalAmountReceivedUntilUpdatedAt.minus(totalAmountReceivedFromPoolBeforeUpdate);
    _createMemberUnitsUpdatedEntity(event, pool, poolMember);

    // Other entity updates
    const eventName = "MemberUnitsUpdated";
    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);
    _createTokenStatisticLogEntity(event, event.params.token, eventName);

    updateATSStreamedAndBalanceUntilUpdatedAt(event.params.member, event.params.token, event.block, BigInt.fromI32(0));
    _createAccountTokenSnapshotLogEntity(event, event.params.member, event.params.token, eventName);
}

function _createDistributionClaimedEntity(
    event: DistributionClaimed,
    pool: Pool,
    poolMember: PoolMember
): DistributionClaimedEvent 
{
    const ev = new DistributionClaimedEvent(createEventID("DistributionClaimed", event));
    initializeEventEntity(ev, event, [event.params.token, event.address, event.params.member]);

    ev.token = event.params.token;
    ev.claimedAmount = event.params.claimedAmount;
    ev.totalClaimed = event.params.totalClaimed;
    ev.pool = event.address.toHex();
    ev.poolMember = poolMember.id;

    ev.pool_perUnitSettledValue = pool.perUnitSettledValue;
    ev.pool_perUnitFlowRate = pool.perUnitFlowRate;
    ev.pool_adjustmentFlowRate = pool.adjustmentFlowRate;
    ev.pool_totalUnits = pool.totalUnits;

    ev.poolMember_units = poolMember.units;

    ev.save();

    return ev;
}

function _createMemberUnitsUpdatedEntity(
    event: MemberUnitsUpdated,
    pool: Pool,
    poolMember: PoolMember
): MemberUnitsUpdatedEvent {
    const ev = new MemberUnitsUpdatedEvent(createEventID("MemberUnitsUpdated", event));
    initializeEventEntity(ev, event, [event.params.token, event.address, event.params.member]);

    ev.token = event.params.token;
    ev.oldUnits = event.params.oldUnits;
    ev.units = event.params.newUnits;
    ev.totalUnits = pool.totalUnits;
    ev.pool = event.address.toHex();
    ev.poolMember = poolMember.id;

    ev.pool_perUnitSettledValue = pool.perUnitSettledValue;
    ev.pool_perUnitFlowRate = pool.perUnitFlowRate;
    ev.pool_adjustmentFlowRate = pool.adjustmentFlowRate;
    ev.pool_totalUnits = pool.totalUnits;

    ev.poolMember_units = poolMember.units;

    ev.save();

    return ev;
}
