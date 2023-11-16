import {
    DistributionClaimed,
    MemberUnitsUpdated,
} from "../../generated/GeneralDistributionAgreementV1/ISuperfluidPool";
import { DistributionClaimedEvent, MemberUnitsUpdatedEvent } from "../../generated/schema";
import {
    _createAccountTokenSnapshotLogEntity,
    _createTokenStatisticLogEntity,
    getOrInitPool,
    getOrInitPoolMember,
    updateATSStreamedAndBalanceUntilUpdatedAt,
    updateAggregateDistributionAgreementData,
    updatePoolTotalAmountFlowedAndDistributed,
    updateTokenStatsStreamedUntilUpdatedAt,
} from "../mappingHelpers";
import { BIG_INT_ZERO, createEventID, initializeEventEntity, membershipWithUnitsExists } from "../utils";

// @note use deltas where applicable

export function handleDistributionClaimed(event: DistributionClaimed): void {
    const token = event.params.token;

    // Update Pool
    let pool = getOrInitPool(event, event.address.toHex());
    pool = updatePoolTotalAmountFlowedAndDistributed(event, pool);
    pool.save();

    // Update PoolMember
    const poolMember = getOrInitPoolMember(event, event.address, event.params.member);
    poolMember.totalAmountClaimed = event.params.totalClaimed;
    poolMember.save();

    // Update Token Statistics
    const eventName = "DistributionClaimed";
    updateTokenStatsStreamedUntilUpdatedAt(token, event.block);
    _createTokenStatisticLogEntity(event, token, eventName);

    // Update ATS
    updateATSStreamedAndBalanceUntilUpdatedAt(event.params.member, token, event.block, null);
    _createAccountTokenSnapshotLogEntity(event, event.params.member, token, eventName);

    // Create Event Entity
    _createDistributionClaimedEntity(event, poolMember.id);
}

export function handleMemberUnitsUpdated(event: MemberUnitsUpdated): void {
    // - PoolMember
    // - units
    const poolMember = getOrInitPoolMember(event, event.address, event.params.member);
    const hasMembershipWithUnits = membershipWithUnitsExists(poolMember.id);

    const previousUnits = poolMember.units;
    const unitsDelta = event.params.newUnits.minus(previousUnits);
    poolMember.units = event.params.newUnits;

    poolMember.save();

    const eventName = "MemberUnitsUpdated";
    updateTokenStatsStreamedUntilUpdatedAt(event.params.token, event.block);
    _createTokenStatisticLogEntity(event, event.params.token, eventName);

    updateATSStreamedAndBalanceUntilUpdatedAt(event.params.member, event.params.token, event.block, null);
    _createAccountTokenSnapshotLogEntity(event, event.params.member, event.params.token, eventName);

    let pool = getOrInitPool(event, event.address.toHex());
    pool = updatePoolTotalAmountFlowedAndDistributed(event, pool);
    if (poolMember.isConnected) {
        pool.totalConnectedUnits = pool.totalConnectedUnits.plus(unitsDelta);
    } else {
        pool.totalDisconnectedUnits = pool.totalDisconnectedUnits.plus(unitsDelta);
    }
    pool.totalUnits = pool.totalUnits.plus(unitsDelta);
    pool.save();

    // 0 units to > 0 units
    if (previousUnits.equals(BIG_INT_ZERO) && event.params.newUnits.gt(BIG_INT_ZERO)) {
        pool.totalMembers = pool.totalMembers + 1;
        // if the member is connected with units now, we add one to connected
        if (poolMember.isConnected) {
            pool.totalConnectedMembers = pool.totalConnectedMembers + 1;
        } else {
            // if the member is disconnected with units now, we add one to disconnected
            pool.totalDisconnectedMembers = pool.totalDisconnectedMembers + 1;
        }
        pool.save();

        updateAggregateDistributionAgreementData(
            event.params.member,
            event.params.token,
            hasMembershipWithUnits,
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
    if (previousUnits.gt(BIG_INT_ZERO) && poolMember.units.equals(BIG_INT_ZERO)) {
        pool.totalMembers = pool.totalMembers - 1;
        // if the member is connected with no units now, we subtract one from connected
        if (poolMember.isConnected) {
            pool.totalConnectedMembers = pool.totalConnectedMembers - 1;
        } else {
            // if the member is disconnected with no units now, we subtract one from disconnected
            pool.totalDisconnectedMembers = pool.totalDisconnectedMembers - 1;
        }
        pool.save();

        updateAggregateDistributionAgreementData(
            event.params.member,
            event.params.token,
            hasMembershipWithUnits,
            poolMember.isConnected,
            false, // don't increment memberWithUnits
            false, // not disconnecting membership
            true, // only place we decrement membershipWithUnits IF member has memberShipWithUnits
            false, // not connecting
            event.block,
            false // isIDA
        );
    }

    // Create Event Entity
    _createMemberUnitsUpdatedEntity(event, poolMember.id);
}

function _createDistributionClaimedEntity(event: DistributionClaimed, poolMemberId: string): DistributionClaimedEvent {
    const ev = new DistributionClaimedEvent(createEventID("DistributionClaimed", event));
    initializeEventEntity(ev, event, [event.params.token, event.address, event.params.member]);

    ev.token = event.params.token;
    ev.claimedAmount = event.params.claimedAmount;
    ev.totalClaimed = event.params.totalClaimed;
    ev.pool = event.address.toHex();
    ev.poolMember = poolMemberId;
    ev.save();

    return ev;
}

function _createMemberUnitsUpdatedEntity(event: MemberUnitsUpdated, poolMemberId: string): MemberUnitsUpdatedEvent {
    const ev = new MemberUnitsUpdatedEvent(createEventID("MemberUnitsUpdated", event));
    initializeEventEntity(ev, event, [event.params.token, event.address, event.params.member]);

    ev.token = event.params.token;
    ev.oldUnits = event.params.oldUnits;
    ev.units = event.params.newUnits;
    ev.pool = event.address.toHex();
    ev.poolMember = poolMemberId;
    ev.save();

    return ev;
}
