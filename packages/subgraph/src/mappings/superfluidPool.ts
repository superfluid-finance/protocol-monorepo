import {
    DistributionClaimed,
    MemberUnitsUpdated,
} from "../../generated/GeneralDistributionAgreementV1/ISuperfluidPool";
import {
    DistributionClaimedEvent,
    MemberUnitsUpdatedEvent,
} from "../../generated/schema";
import {
    getOrInitPool,
    getOrInitPoolMember,
    updatePoolTotalAmountFlowedAndDistributed,
} from "../mappingHelpers";
import { BIG_INT_ZERO, createEventID, initializeEventEntity } from "../utils";

// @note use deltas where applicable

export function handleDistributionClaimed(event: DistributionClaimed): void {
    // Create Event Entity
    _createDistributionClaimedEntity(event);

    // Update Pool
    let pool = getOrInitPool(event, event.address.toHex());
    pool = updatePoolTotalAmountFlowedAndDistributed(event, pool);
    pool.save();

    // Update PoolMember
    let poolMember = getOrInitPoolMember(
        event,
        event.address,
        event.params.member
    );
    poolMember.totalAmountClaimed = event.params.totalClaimed;
    poolMember.save();

    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}
export function handleMemberUnitsUpdated(event: MemberUnitsUpdated): void {
    // Create Event Entity
    _createMemberUnitsUpdatedEntity(event);

    // - PoolMember
    // - units
    let poolMember = getOrInitPoolMember(
        event,
        event.address,
        event.params.member
    );
    const previousUnits = poolMember.units;
    const unitsDelta = event.params.units.minus(poolMember.units);
    poolMember.units = event.params.units;

    poolMember.save();

    let pool = getOrInitPool(event, event.address.toHex());
    pool = updatePoolTotalAmountFlowedAndDistributed(event, pool);
    if (poolMember.isConnected) {
        pool.totalConnectedUnits = pool.totalConnectedUnits.plus(unitsDelta);
    } else {
        pool.totalDisconnectedUnits =
            pool.totalDisconnectedUnits.plus(unitsDelta);
    }

    // 0 units to > 0 units
    if (
        previousUnits.equals(BIG_INT_ZERO) &&
        poolMember.units.gt(BIG_INT_ZERO)
    ) {
        pool.totalMembers = pool.totalMembers + 1;
        // if the member is connected with units now, we add one to connected
        if (poolMember.isConnected) {
            pool.totalConnectedMembers = pool.totalConnectedMembers + 1;
        } else {
            // if the member is disconnected with units now, we add one to disconnected
            pool.totalDisconnectedMembers = pool.totalDisconnectedMembers + 1;
        }
    }

    // > 0 units to 0 units
    if (
        previousUnits.gt(BIG_INT_ZERO) &&
        poolMember.units.equals(BIG_INT_ZERO)
    ) {
        pool.totalMembers = pool.totalMembers - 1;
        // if the member is connected with no units now, we subtract one from connected
        if (poolMember.isConnected) {
            pool.totalConnectedMembers = pool.totalConnectedMembers - 1;
        } else {
            // if the member is disconnected with no units now, we subtract one from disconnected
            pool.totalDisconnectedMembers = pool.totalDisconnectedMembers - 1;
        }
    }
    pool.totalUnits.plus(unitsDelta);
    pool.save();

    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}

function _createDistributionClaimedEntity(
    event: DistributionClaimed
): DistributionClaimedEvent {
    const ev = new DistributionClaimedEvent(
        createEventID("DistributionClaimed", event)
    );
    initializeEventEntity(ev, event, [
        event.params.token,
        event.address,
        event.params.member,
    ]);

    ev.token = event.params.token;
    ev.claimedAmount = event.params.claimedAmount;
    ev.totalClaimed = event.params.totalClaimed;
    ev.pool = event.address.toHex();
    ev.poolMember = event.params.member.toHex();
    ev.save();

    return ev;
}

function _createMemberUnitsUpdatedEntity(
    event: MemberUnitsUpdated
): MemberUnitsUpdatedEvent {
    const ev = new MemberUnitsUpdatedEvent(
        createEventID("MemberUnitsUpdated", event)
    );
    initializeEventEntity(ev, event, [
        event.params.token,
        event.address,
        event.params.member,
    ]);

    ev.token = event.params.token;
    ev.units = event.params.units;
    ev.pool = event.address.toHex();
    ev.poolMember = event.params.member.toHex();
    ev.save();

    return ev;
}
