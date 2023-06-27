import {
    DistributionClaimed,
    MemberUpdated,
} from "../../generated/GeneralDistributionAgreementV1/ISuperfluidPool";

// @note use deltas where applicable

export function handleDistributionClaimed(event: DistributionClaimed): void {
    // event entity: creates DistributionClaimedEvent entity
    // hol entity:
    // - Pool
        // - updatedAtTimestamp
        // - updatedAtBlockNumber
        // - distributionClaimedEvents will have another event
        // - totalAmountFlowedDistributedUntilUpdatedAt
        // - totalAmountDistributedUntilUpdatedAt
    // - PoolMember
        // - distributionClaimedEvents will have another event
        // - totalAmountFlowedDistributedUntilUpdatedAt
        // - totalAmountDistributedUntilUpdatedAt
    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}
export function handleMemberUpdated(event: MemberUpdated): void {
    // event entity: creates BufferAdjustedEvent entity
    // hol entity:
    // - Pool
        // - updatedAtTimestamp
        // - updatedAtBlockNumber
        // - totalUnits
        // - totalConnectedUnits
        // - totalDisconnectedUnits
        // - poolMemberUpdatedEvents will have another event
        // - totalAmountFlowedDistributedUntilUpdatedAt
        // - totalAmountDistributedUntilUpdatedAt
    // - PoolMember
        // - units
    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}
