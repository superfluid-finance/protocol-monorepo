import {
    BufferAdjusted,
    FlowDistributionUpdated,
    InstantDistributionUpdated,
    PoolConnectionUpdated,
    PoolCreated,
} from "../../generated/GeneralDistributionAgreementV1/IGeneralDistributionAgreementV1";

// @note use deltas where applicable

export function handlePoolConnectionUpdated(
    event: PoolConnectionUpdated
): void {
    // event entity: creates PoolConnectionUpdatedEvent entity
    // hol entity:
    // - Pool
    //      - updatedAtTimestamp
    //      - updatedAtBlockNumber
    //      - totalConnectedUnits
    //      - totalDisconnectedUnits
    //      - totalConnectedMembers
    //      - totalDisconnectedMembers
    //      - totalAmountFlowedDistributedUntilUpdatedAt
    //      - totalAmountDistributedUntilUpdatedAt
    // - PoolMember
    //      - if non-existent: initialize
    //      - else: isConnected
    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}

export function handlePoolCreated(event: PoolCreated): void {
    // event entity: creates PoolCreatedEvent entity
    // hol entity:
    // - Pool
    //      - All fields initialized
    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}

export function handleBufferAdjusted(event: BufferAdjusted): void {
    // event entity: creates BufferAdjustedEvent entity
    // hol entity:
    // - Pool
    //      - updatedAtTimestamp
    //      - updatedAtBlockNumber
    //      - totalBuffer
    //      - bufferAdjustedEvents will have another event
    //      - totalAmountFlowedDistributedUntilUpdatedAt
    //      - totalAmountDistributedUntilUpdatedAt
    // - PoolDistributor
    //      - totalBuffer
    //      - bufferAdjustedEvents will have another event
    //      - totalAmountFlowedDistributedUntilUpdatedAt
    //      - totalAmountDistributedUntilUpdatedAt
    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}

export function handleFlowDistributionUpdated(
    event: FlowDistributionUpdated
): void {
    // event entity: creates FlowDistributionUpdatedEvent entity
    // hol entity:
    // - Pool
    //      - updatedAtTimestamp
    //      - updatedAtBlockNumber
    //      - flowRate
    //      - adjustmentFlowRate
    //      - totalAmountFlowedDistributedUntilUpdatedAt
    //      - totalAmountDistributedUntilUpdatedAt
    // - PoolDistributor
    //      - totalAmountFlowedDistributedUntilUpdatedAt
    //      - totalAmountDistributedUntilUpdatedAt
    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}

export function handleInstantDistributionUpdated(
    event: InstantDistributionUpdated
): void {
    // event entity: creates InstantDistributionUpdatedEvent entity
    // hol entity:
    // - Pool
    //      - updatedAtTimestamp
    //      - updatedAtBlockNumber
    //      - totalAmountFlowedDistributedUntilUpdatedAt
    //      - totalAmountInstantlyDistributedUntilUpdatedAt
    //      - totalAmountDistributedUntilUpdatedAt
    // - PoolDistributor
    //      - updatedAtTimestamp
    //      - updatedAtBlockNumber
    //      - totalAmountFlowedDistributedUntilUpdatedAt
    //      - totalAmountInstantlyDistributedUntilUpdatedAt
    //      - totalAmountDistributedUntilUpdatedAt
    // aggregate (TBD):
    // - AccountTokenSnapshot
    // - AccountTokenSnapshotLog
    // - TokenStatistic
    // - TokenStatisticLog
}
