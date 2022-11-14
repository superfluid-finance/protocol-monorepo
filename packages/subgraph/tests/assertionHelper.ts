import { BigInt, ethereum } from "@graphprotocol/graph-ts";
import { assert } from "matchstick-as/assembly/index";
import { createEventID, getOrder } from "../src/utils";

/**
 * Asserts that the "base" properties on our Event entity are correct
 * @param event The event we are checking
 * @param eventName The name of the event
 * @returns The id of the event (based on our createEventID function)
 */
export function assertEventBaseProperties(
    event: ethereum.Event,
    eventName: string
): string {
    const entityType = eventName + "Event";
    const id = createEventID(eventName, event);
    const order = getOrder(event.block.number, event.logIndex);

    assert.fieldEquals(entityType, id, "id", id);
    assert.fieldEquals(entityType, id, "blockNumber", event.block.number.toString());
    assert.fieldEquals(entityType, id, "logIndex", event.logIndex.toString());
    assert.fieldEquals(entityType, id, "order", order.toString());
    assert.fieldEquals(entityType, id, "timestamp", event.block.timestamp.toString());
    assert.fieldEquals(entityType, id, "transactionHash", event.transaction.hash.toHex());
    assert.fieldEquals(entityType, id, "gasPrice", event.transaction.gasPrice.toString());
    return id;
}

/**
 * Asserts that the "base" properties on our Higher Order entity are correct
 * @param entityName The name of the entity
 * @param id the id of the entity
 * @param createdAtTimestamp timestamp retrieved from the event
 * @param createdAtBlockNumber block number retrieved from the event
 * @param updatedAtTimestamp timestamp retrieved from the event
 * @param updatedAtBlockNumber block number retrieved from the event
 */
export function assertHigherOrderBaseProperties(
    entityName: string,
    id: string,
    createdAtTimestamp: BigInt,
    createdAtBlockNumber: BigInt,
    updatedAtTimestamp: BigInt,
    updatedAtBlockNumber: BigInt
): void {
    assertAggregateBaseProperties(entityName, id, updatedAtTimestamp, updatedAtBlockNumber);
    assert.fieldEquals(entityName, id, "createdAtTimestamp", createdAtTimestamp.toString());
    assert.fieldEquals(entityName, id, "createdAtBlockNumber", createdAtBlockNumber.toString());
}

/**
 * Asserts that the "base" properties on our Aggregate entity are correct.
 * Note that this is a subset of our Higher Order entity "base" properties.
 * @param entityName The name of the entity
 * @param id the id of the entity
 * @param updatedAtTimestamp timestamp retrieved from the event
 * @param updatedAtBlockNumber block number retrieved from the event
 */
export function assertAggregateBaseProperties(
    entityName: string,
    id: string,
    updatedAtTimestamp: BigInt,
    updatedAtBlockNumber: BigInt
): void {
    assert.fieldEquals(entityName, id, "id", id);
    assert.fieldEquals(entityName, id, "updatedAtTimestamp", updatedAtTimestamp.toString());
    assert.fieldEquals(entityName, id, "updatedAtBlockNumber", updatedAtBlockNumber.toString());
}

/**
 * Asserts that the properties on a TokenStatistic entity are correct.
 * @param id the token address
 * @param updatedAtTimestamp timestamp retrieved from the event
 * @param updatedAtBlockNumber block number retrieved from the event
 * @param totalNumberOfActiveStreams expected count of active streams for the token
 * @param totalNumberOfClosedStreams expected count of closed streams for the token
 * @param totalNumberOfIndexes expected count of indexes for the token
 * @param totalNumberOfActiveIndexes expected count of active indexes for the token
 * @param totalSubscriptionsWithUnits expected count of subscriptions with allocated units for the token
 * @param totalApprovedSubscriptions expected totalNumber of approved subscriptions for the token
 * @param totalDeposit expected total deposit amount
 * @param totalOutflowRate expected total outflow rate
 * @param totalAmountStreamedUntilUpdatedAt expected total amount streamed until updated at timestamp
 * @param totalAmountTransferredUntilUpdatedAt expected total amount transferred until updated at timestamp
 * @param totalAmountDistributedUntilUpdatedAt expected total amount distributed (with IDA) until updated at timestamp
 * @param totalSupply expected total supply
 * @param token expected token address
 */
export function assertTokenStatisticProperties(
    id: string,
    updatedAtTimestamp: BigInt,
    updatedAtBlockNumber: BigInt,
    totalNumberOfActiveStreams: i32,
    totalNumberOfClosedStreams: i32,
    totalNumberOfIndexes: i32,
    totalNumberOfActiveIndexes: i32,
    totalSubscriptionsWithUnits: i32,
    totalApprovedSubscriptions: i32,
    totalDeposit: BigInt,
    totalOutflowRate: BigInt,
    totalAmountStreamedUntilUpdatedAt: BigInt,
    totalAmountTransferredUntilUpdatedAt: BigInt,
    totalAmountDistributedUntilUpdatedAt: BigInt,
    totalSupply: BigInt,
    token: string
): void {
    const entityName = "TokenStatistic";
    assertAggregateBaseProperties(entityName, id, updatedAtTimestamp, updatedAtBlockNumber);
    assert.fieldEquals(entityName, id, "totalNumberOfActiveStreams", totalNumberOfActiveStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfClosedStreams", totalNumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfIndexes", totalNumberOfIndexes.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfActiveIndexes", totalNumberOfActiveIndexes.toString());
    assert.fieldEquals(entityName, id, "totalSubscriptionsWithUnits", totalSubscriptionsWithUnits.toString());
    assert.fieldEquals(entityName, id, "totalApprovedSubscriptions", totalApprovedSubscriptions.toString());
    assert.fieldEquals(entityName, id, "totalDeposit", totalDeposit.toString());
    assert.fieldEquals(entityName, id, "totalOutflowRate", totalOutflowRate.toString());
    assert.fieldEquals(entityName, id, "totalAmountStreamedUntilUpdatedAt", totalAmountStreamedUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalAmountTransferredUntilUpdatedAt", totalAmountTransferredUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalAmountDistributedUntilUpdatedAt", totalAmountDistributedUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalSupply", totalSupply.toString());
    assert.fieldEquals(entityName, id, "token", token);
}

/**
 * Asserts that the "base" properties on an Aggregate Log entity are correct.
 * Note that this is a subset of our Higher Order entity "base" properties.
 * @param entityName The name of the entity
 * @param id the id of the entity
 * @param timestamp timestamp retrieved from the event
 * @param blockNumber block number retrieved from the event
 */
 export function assertAggregateLogBaseProperties(
    entityName: string,
    id: string,
    timestamp: BigInt,
    blockNumber: BigInt
): void {
    assert.fieldEquals(entityName, id, "id", id);
    assert.fieldEquals(entityName, id, "timestamp", timestamp.toString());
    assert.fieldEquals(entityName, id, "blockNumber", blockNumber.toString());
}

/**
 * Asserts that the properties on a TokenStatisticLog entity are correct.
 * @param entityName The name of the entity
 * @param id token address
 * @param event ethereum event object
 * @param triggeredByEventName name of the event which triggered the creation of this log
 * @param totalNumberOfActiveStreams expected count of active streams for the token
 * @param totalNumberOfClosedStreams expected count of closed streams for the token
 * @param totalNumberOfIndexes expected count of indexes for the token
 * @param totalNumberOfActiveIndexes expected count of active indexes for the token
 * @param totalSubscriptionsWithUnits expected count of subscriptions with allocated units for the token
 * @param totalApprovedSubscriptions expected totalNumber of approved subscriptions for the token
 * @param totalDeposit expected total deposit amount
 * @param totalOutflowRate expected total outflow rate
 * @param totalAmountStreamedUntilUpdatedAt expected total amount streamed until updated at timestamp
 * @param totalAmountTransferredUntilUpdatedAt expected total amount transferred until updated at timestamp
 * @param totalAmountDistributedUntilUpdatedAt expected total amount distributed (with IDA) until updated at timestamp
 * @param totalSupply expected total supply
 * @param token expected token address
 */
export function assertTokenStatisticLogProperties(
    entityName: string,
    id: string,
    event: ethereum.Event,
    triggeredByEventName: string,
    totalNumberOfActiveStreams: i32,
    totalNumberOfClosedStreams: i32,
    totalNumberOfIndexes: i32,
    totalNumberOfActiveIndexes: i32,
    totalSubscriptionsWithUnits: i32,
    totalApprovedSubscriptions: i32,
    totalDeposit: BigInt,
    totalOutflowRate: BigInt,
    totalAmountStreamedUntilUpdatedAt: BigInt,
    totalAmountTransferredUntilUpdatedAt: BigInt,
    totalAmountDistributedUntilUpdatedAt: BigInt,
    totalSupply: BigInt,
    token: string
): void {
    const order = getOrder(event.block.number, event.logIndex);
    assert.fieldEquals(entityName, id, "timestamp", event.block.timestamp.toString());
    assert.fieldEquals(entityName, id, "blockNumber", event.block.number.toString());
    assert.fieldEquals(entityName, id, "transactionHash", event.transaction.hash.toHex());
    assert.fieldEquals(entityName, id, "logIndex", event.logIndex.toString());
    assert.fieldEquals(entityName, id, "order", order.toString());
    assert.fieldEquals(entityName, id, "triggeredByEventName", triggeredByEventName);
    assert.fieldEquals(entityName, id, "totalNumberOfActiveStreams", totalNumberOfActiveStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfClosedStreams", totalNumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfIndexes", totalNumberOfIndexes.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfActiveIndexes", totalNumberOfActiveIndexes.toString());
    assert.fieldEquals(entityName, id, "totalSubscriptionsWithUnits", totalSubscriptionsWithUnits.toString());
    assert.fieldEquals(entityName, id, "totalApprovedSubscriptions", totalApprovedSubscriptions.toString());
    assert.fieldEquals(entityName, id, "totalDeposit", totalDeposit.toString());
    assert.fieldEquals(entityName, id, "totalOutflowRate", totalOutflowRate.toString());
    assert.fieldEquals(entityName, id, "totalAmountStreamedUntilUpdatedAt", totalAmountStreamedUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalAmountTransferredUntilUpdatedAt", totalAmountTransferredUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalAmountDistributedUntilUpdatedAt", totalAmountDistributedUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalSupply", totalSupply.toString());
    assert.fieldEquals(entityName, id, "token", token);
}