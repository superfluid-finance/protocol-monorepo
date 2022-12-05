import { Address, BigInt, ethereum } from "@graphprotocol/graph-ts";
import { assert, log } from "matchstick-as/assembly/index";
import { createEventID, createLogID, getIndexID, getOrder } from "../src/utils";

// General Assertion Helpers

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
    assert.fieldEquals(entityType, id, "name", eventName);
    assert.fieldEquals(entityType, id, "transactionHash", event.transaction.hash.toHex());
    assert.fieldEquals(entityType, id, "gasPrice", event.transaction.gasPrice.toString());
    if (event.receipt) {
        const receipt = event.receipt as ethereum.TransactionReceipt;
        assert.fieldEquals(entityType, id, "gasUsed", receipt.gasUsed.toString());
    }
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

// IDA Event Assertion Helpers

/**
 * Asserts that "base" event properties are correct as well as properties
 * specific to all IDA events.
 * @param event 
 * @param eventName
 * @param superToken 
 * @param indexId 
 * @param publisher 
 */
export function assertIDAEventBaseProperties(
    event: ethereum.Event,
    eventName: string,
    superToken: string,
    indexId: string,
    publisher: string,
): string {
    const id = assertEventBaseProperties(event, eventName);
    const entityName = eventName + "Event";

    assert.fieldEquals(entityName, id, "token", superToken);
    assert.fieldEquals(entityName, id, "indexId", indexId);
    assert.fieldEquals(entityName, id, "publisher", publisher);

    return id;
}

/**
 * Asserts that "base" event properties are correct as well as properties
 * specific to all IDA events and properties specific to IDA "Index" events.
 * @param event 
 * @param eventName 
 * @param superToken 
 * @param indexId 
 * @param publisher 
 * @param userData 
 * @returns id of the event
 */
 export function assertIDAIndexEventBaseProperties(
    event: ethereum.Event,
    eventName: string,
    superToken: string,
    indexId: string,
    publisher: string,
    userData: string,
): string {
    const id = assertEventBaseProperties(event, eventName);
    assertIDAEventBaseProperties(event, eventName, superToken, indexId, publisher);
    const indexEntityId = getIndexID(
        Address.fromString(publisher),
        Address.fromString(superToken),
        BigInt.fromString(indexId)
    );
    const entityName = eventName + "Event";
    
    assert.fieldEquals(entityName, id, "index", indexEntityId);
    assert.fieldEquals(entityName, id, "userData", userData);

    return id;
}

// Aggregate Entity Assertion Helpers

/**
 * Asserts that the properties on a TokenStatistic entity are correct.
 * @param id the token address
 * @param event if event is passed, we validate TokenStatisticLog
 * @param triggeredByEventName if triggeredByEventName is passed, we validate TokenStatisticLog
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
 */
export function assertTokenStatisticProperties(
    event: ethereum.Event | null,
    triggeredByEventName: string | null,
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
    totalSupply: BigInt
): void {
    if (event && ! triggeredByEventName || !event && triggeredByEventName) {
        log.error("You cannot pass event OR triggeredByEventName, you must pass both.", []);
    }

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
    assert.fieldEquals(entityName, id, "token", id); // NOTE: id of tokenStatistic is token address

    if (event && triggeredByEventName) {
        assertTokenStatisticLogProperties(
            event,
            triggeredByEventName,
            totalNumberOfActiveStreams,
            totalNumberOfClosedStreams,
            totalNumberOfIndexes,
            totalNumberOfActiveIndexes,
            totalSubscriptionsWithUnits,
            totalApprovedSubscriptions,
            totalDeposit,
            totalOutflowRate,
            totalAmountStreamedUntilUpdatedAt,
            totalAmountTransferredUntilUpdatedAt,
            totalAmountDistributedUntilUpdatedAt,
            totalSupply,
            id
        );
    }
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
 * @param totalAmountStreamed expected total amount streamed until timestamp
 * @param totalAmountTransferred expected total amount transferred until timestamp
 * @param totalAmountDistributed expected total amount distributed (with IDA) until timestamp
 * @param totalSupply expected total supply
 * @param tokenAddress expected token address
 */
export function assertTokenStatisticLogProperties(
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
    totalAmountStreamed: BigInt,
    totalAmountTransferred: BigInt,
    totalAmountDistributed: BigInt,
    totalSupply: BigInt,
    tokenAddress: string
): void {
    const entityName = "TokenStatisticLog";
    const timestamp = event.block.timestamp;
    const blockNumber = event.block.number;
    const id = createLogID("TSLog", tokenAddress, event);
    assertAggregateLogBaseProperties(entityName, id, timestamp, blockNumber);
    const order = getOrder(event.block.number, event.logIndex);
    assert.fieldEquals(entityName, id, "timestamp", timestamp.toString());
    assert.fieldEquals(entityName, id, "blockNumber", blockNumber.toString());
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
    assert.fieldEquals(entityName, id, "totalAmountStreamed", totalAmountStreamed.toString());
    assert.fieldEquals(entityName, id, "totalAmountTransferred", totalAmountTransferred.toString());
    assert.fieldEquals(entityName, id, "totalAmountDistributed", totalAmountDistributed.toString());
    assert.fieldEquals(entityName, id, "totalSupply", totalSupply.toString());
    assert.fieldEquals(entityName, id, "token", tokenAddress);
    assert.fieldEquals(entityName, id, "tokenStatistic", tokenAddress);
}