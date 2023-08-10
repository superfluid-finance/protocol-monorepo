import { Address, BigInt, ethereum } from "@graphprotocol/graph-ts";
import { assert, log } from "matchstick-as/assembly/index";
import { BIG_INT_ZERO, createEventID, createLogID, getIndexID, getOrder } from "../src/utils";

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
    const receipt = event.receipt;
    if (receipt) {
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
    event: ethereum.Event,
): void {
    assertAggregateBaseProperties(entityName, id, event.block.timestamp, event.block.number);
    assert.fieldEquals(entityName, id, "createdAtTimestamp", event.block.timestamp.toString());
    assert.fieldEquals(entityName, id, "createdAtBlockNumber", event.block.number.toString());
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
 * @param totalNumberOfActiveStreams expected count of active streams for the token for all flow agreements
 * @param totalCFANumberOfActiveStreams expected count of active streams for the token for the CFA
 * @param totalGDANumberOfActiveStreams expected count of active streams for the token for the GDA
 * @param totalNumberOfClosedStreams expected count of closed streams for the token for all flow agreements
 * @param totalNumberOfCFAClosedStreams expected count of closed streams for the token for the CFA
 * @param totalNumberOfGDAClosedStreams expected count of closed streams for the token for the GDA
 * @param totalNumberOfIndexes expected count of indexes for the token
 * @param totalNumberOfActiveIndexes expected count of active indexes for the token
 * @param totalSubscriptionsWithUnits expected count of subscriptions with allocated units for the token
 * @param totalApprovedSubscriptions expected totalNumber of approved subscriptions for the token
 * @param totalDeposit expected total deposit amount for all flow agreements
 * @param totalCFADeposit expected total deposit amount for the CFA
 * @param totalGDADeposit expected total deposit amount for the GDA
 * @param totalOutflowRate expected total outflow rate for all flow agreements
 * @param totalCFAOutflowRate expected total outflow rate for the CFA
 * @param totalGDAOutflowRate expected total outflow rate for the GDA
 * @param totalAmountStreamedUntilUpdatedAt expected total amount streamed until updated at timestamp for all flow agreements
 * @param totalCFAAmountStreamedUntilUpdatedAt expected total amount streamed until updated at timestamp for the CFA
 * @param totalGDAAmountStreamedUntilUpdatedAt expected total amount streamed until updated at timestamp for the GDA
 * @param totalAmountTransferredUntilUpdatedAt expected total amount transferred until updated at timestamp
 * @param totalAmountDistributedUntilUpdatedAt expected total amount distributed (with IDA) until updated at timestamp
 * @param totalSupply expected total supply
 * @param totalNumberOfAccounts total number of unique accounts
 * @param totalNumberOfHolders total number of balance holders
 */
export function assertTokenStatisticProperties(
    event: ethereum.Event | null,
    triggeredByEventName: string | null,
    id: string,
    updatedAtTimestamp: BigInt,
    updatedAtBlockNumber: BigInt,
    totalNumberOfActiveStreams: i32,
    totalCFANumberOfActiveStreams: i32,
    totalGDANumberOfActiveStreams: i32,
    totalNumberOfClosedStreams: i32,
    totalCFANumberOfClosedStreams: i32,
    totalGDANumberOfClosedStreams: i32,
    totalNumberOfIndexes: i32,
    totalNumberOfActiveIndexes: i32,
    totalSubscriptionsWithUnits: i32,
    totalApprovedSubscriptions: i32,
    totalDeposit: BigInt,
    totalCFADeposit: BigInt,
    totalGDADeposit: BigInt,
    totalOutflowRate: BigInt,
    totalCFAOutflowRate: BigInt,
    totalGDAOutflowRate: BigInt,
    totalAmountStreamedUntilUpdatedAt: BigInt,
    totalCFAAmountStreamedUntilUpdatedAt: BigInt,
    totalGDAAmountStreamedUntilUpdatedAt: BigInt,
    totalAmountTransferredUntilUpdatedAt: BigInt,
    totalAmountDistributedUntilUpdatedAt: BigInt,
    totalSupply: BigInt,
    totalNumberOfAccounts: i32,
    totalNumberOfHolders: i32
): void {
    if (event && ! triggeredByEventName || !event && triggeredByEventName) {
        log.error("You cannot pass event OR triggeredByEventName, you must pass both.", []);
    }

    const entityName = "TokenStatistic";
    assertAggregateBaseProperties(entityName, id, updatedAtTimestamp, updatedAtBlockNumber);
    assert.fieldEquals(entityName, id, "totalNumberOfActiveStreams", totalNumberOfActiveStreams.toString());
    assert.fieldEquals(entityName, id, "totalCFANumberOfActiveStreams", totalCFANumberOfActiveStreams.toString());
    assert.fieldEquals(entityName, id, "totalGDANumberOfActiveStreams", totalGDANumberOfActiveStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfClosedStreams", totalNumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalCFANumberOfClosedStreams", totalCFANumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalGDANumberOfClosedStreams", totalGDANumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfIndexes", totalNumberOfIndexes.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfActiveIndexes", totalNumberOfActiveIndexes.toString());
    assert.fieldEquals(entityName, id, "totalSubscriptionsWithUnits", totalSubscriptionsWithUnits.toString());
    assert.fieldEquals(entityName, id, "totalApprovedSubscriptions", totalApprovedSubscriptions.toString());
    assert.fieldEquals(entityName, id, "totalDeposit", totalDeposit.toString());
    assert.fieldEquals(entityName, id, "totalCFADeposit", totalCFADeposit.toString());
    assert.fieldEquals(entityName, id, "totalGDADeposit", totalGDADeposit.toString());
    assert.fieldEquals(entityName, id, "totalOutflowRate", totalOutflowRate.toString());
    assert.fieldEquals(entityName, id, "totalCFAOutflowRate", totalCFAOutflowRate.toString());
    assert.fieldEquals(entityName, id, "totalGDAOutflowRate", totalGDAOutflowRate.toString());
    assert.fieldEquals(entityName, id, "totalAmountStreamedUntilUpdatedAt", totalAmountStreamedUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalCFAAmountStreamedUntilUpdatedAt", totalCFAAmountStreamedUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalGDAAmountStreamedUntilUpdatedAt", totalGDAAmountStreamedUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalAmountTransferredUntilUpdatedAt", totalAmountTransferredUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalAmountDistributedUntilUpdatedAt", totalAmountDistributedUntilUpdatedAt.toString());
    assert.fieldEquals(entityName, id, "totalSupply", totalSupply.toString());
    assert.fieldEquals(entityName, id, "token", id); // NOTE: id of tokenStatistic is token address
    assert.fieldEquals(entityName, id, "totalNumberOfAccounts", totalNumberOfAccounts.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfHolders", totalNumberOfHolders.toString());


    if (event && triggeredByEventName) {
        assertTokenStatisticLogProperties(
            event,
            triggeredByEventName,
            totalNumberOfActiveStreams,
            totalCFANumberOfActiveStreams,
            totalGDANumberOfActiveStreams,
            totalNumberOfClosedStreams,
            totalCFANumberOfClosedStreams,
            totalGDANumberOfClosedStreams,
            totalNumberOfIndexes,
            totalNumberOfActiveIndexes,
            totalSubscriptionsWithUnits,
            totalApprovedSubscriptions,
            totalDeposit,
            totalCFADeposit,
            totalGDADeposit,
            totalOutflowRate,
            totalCFAOutflowRate,
            totalGDAOutflowRate,
            totalAmountStreamedUntilUpdatedAt,
            totalCFAAmountStreamedUntilUpdatedAt,
            totalGDAAmountStreamedUntilUpdatedAt,
            totalAmountTransferredUntilUpdatedAt,
            totalAmountDistributedUntilUpdatedAt,
            totalSupply,
            id,
            totalNumberOfAccounts,
            totalNumberOfHolders
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
 * @param totalNumberOfActiveStreams expected count of active streams for the token for all flow agreements
 * @param totalCFANumberOfActiveStreams expected count of active streams for the token for the CFA
 * @param totalGDANumberOfActiveStreams expected count of active streams for the token for the GDA
 * @param totalNumberOfClosedStreams expected count of closed streams for the token for all flow agreements
 * @param totalNumberOfCFAClosedStreams expected count of closed streams for the token for the CFA
 * @param totalNumberOfGDAClosedStreams expected count of closed streams for the token for the GDA
 * @param totalNumberOfIndexes expected count of indexes for the token
 * @param totalNumberOfActiveIndexes expected count of active indexes for the token
 * @param totalSubscriptionsWithUnits expected count of subscriptions with allocated units for the token
 * @param totalApprovedSubscriptions expected totalNumber of approved subscriptions for the token
 * @param totalDeposit expected total deposit amount for all flow agreements
 * @param totalCFADeposit expected total deposit amount for the CFA
 * @param totalGDADeposit expected total deposit amount for the GDA
 * @param totalOutflowRate expected total outflow rate for all flow agreements
 * @param totalCFAOutflowRate expected total outflow rate for the CFA
 * @param totalGDAOutflowRate expected total outflow rate for the GDA
 * @param totalAmountStreamed expected total amount streamed until timestamp for all flow agreements
 * @param totalCFAAmountStreamed expected total amount streamed until timestamp for the CFA
 * @param totalGDAAmountStreamed expected total amount streamed until timestamp for the GDA
 * @param totalAmountTransferred expected total amount transferred until timestamp
 * @param totalAmountDistributed expected total amount distributed (with IDA) until timestamp
 * @param totalSupply expected total supply
 * @param tokenAddress expected token address
 * @param totalNumberOfAccounts total number of unique accounts
 * @param totalNumberOfHolders total number of balance holders
 */
export function assertTokenStatisticLogProperties(
    event: ethereum.Event,
    triggeredByEventName: string,
    totalNumberOfActiveStreams: i32,
    totalCFANumberOfActiveStreams: i32,
    totalGDANumberOfActiveStreams: i32,
    totalNumberOfClosedStreams: i32,
    totalCFANumberOfClosedStreams: i32,
    totalGDANumberOfClosedStreams: i32,
    totalNumberOfIndexes: i32,
    totalNumberOfActiveIndexes: i32,
    totalSubscriptionsWithUnits: i32,
    totalApprovedSubscriptions: i32,
    totalDeposit: BigInt,
    totalCFADeposit: BigInt,
    totalGDADeposit: BigInt,
    totalOutflowRate: BigInt,
    totalCFAOutflowRate: BigInt,
    totalGDAOutflowRate: BigInt,
    totalAmountStreamed: BigInt,
    totalCFAAmountStreamed: BigInt,
    totalGDAAmountStreamed: BigInt,
    totalAmountTransferred: BigInt,
    totalAmountDistributed: BigInt,
    totalSupply: BigInt,
    tokenAddress: string,
    totalNumberOfAccounts: i32,
    totalNumberOfHolders: i32,
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
    assert.fieldEquals(entityName, id, "totalCFANumberOfActiveStreams", totalCFANumberOfActiveStreams.toString());
    assert.fieldEquals(entityName, id, "totalGDANumberOfActiveStreams", totalGDANumberOfActiveStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfClosedStreams", totalNumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalCFANumberOfClosedStreams", totalCFANumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalGDANumberOfClosedStreams", totalGDANumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfClosedStreams", totalNumberOfClosedStreams.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfIndexes", totalNumberOfIndexes.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfActiveIndexes", totalNumberOfActiveIndexes.toString());
    assert.fieldEquals(entityName, id, "totalSubscriptionsWithUnits", totalSubscriptionsWithUnits.toString());
    assert.fieldEquals(entityName, id, "totalApprovedSubscriptions", totalApprovedSubscriptions.toString());
    assert.fieldEquals(entityName, id, "totalDeposit", totalDeposit.toString());
    assert.fieldEquals(entityName, id, "totalCFADeposit", totalCFADeposit.toString());
    assert.fieldEquals(entityName, id, "totalGDADeposit", totalGDADeposit.toString());
    assert.fieldEquals(entityName, id, "totalOutflowRate", totalOutflowRate.toString());
    assert.fieldEquals(entityName, id, "totalCFAOutflowRate", totalCFAOutflowRate.toString());
    assert.fieldEquals(entityName, id, "totalGDAOutflowRate", totalGDAOutflowRate.toString());
    assert.fieldEquals(entityName, id, "totalAmountStreamed", totalAmountStreamed.toString());
    assert.fieldEquals(entityName, id, "totalCFAAmountStreamed", totalCFAAmountStreamed.toString());
    assert.fieldEquals(entityName, id, "totalGDAAmountStreamed", totalGDAAmountStreamed.toString());
    assert.fieldEquals(entityName, id, "totalAmountTransferred", totalAmountTransferred.toString());
    assert.fieldEquals(entityName, id, "totalAmountDistributed", totalAmountDistributed.toString());
    assert.fieldEquals(entityName, id, "totalSupply", totalSupply.toString());
    assert.fieldEquals(entityName, id, "token", tokenAddress);
    assert.fieldEquals(entityName, id, "tokenStatistic", tokenAddress);
    assert.fieldEquals(entityName, id, "totalNumberOfAccounts", totalNumberOfAccounts.toString());
    assert.fieldEquals(entityName, id, "totalNumberOfHolders", totalNumberOfHolders.toString());
}

/**
 * Asserts that the properties on an "empty" initialized TokenStatistic entity are correct.
 * @param id the token address
 * @param event if event is passed, we validate TokenStatisticLog
 * @param triggeredByEventName if triggeredByEventName is passed, we validate TokenStatisticLog
 * @param updatedAtTimestamp timestamp retrieved from the event
 * @param updatedAtBlockNumber block number retrieved from the event
 * @param totalSupply expected total supply
 */
export function assertEmptyTokenStatisticProperties(
    event: ethereum.Event | null,
    triggeredByEventName: string | null,
    id: string,
    updatedAtTimestamp: BigInt,
    updatedAtBlockNumber: BigInt,
    totalSupply: BigInt
): void {
    assertTokenStatisticProperties(
        event,
        triggeredByEventName,
        id,
        updatedAtTimestamp,
        updatedAtBlockNumber,
        0, // totalNumberOfActiveStreams
        0, // totalCFANumberOfActiveStreams
        0, // totalGDANumberOfActiveStreams
        0, // totalNumberOfClosedStreams
        0, // totalCFANumberOfClosedStreams
        0, // totalGDANumberOfClosedStreams
        0, // totalNumberOfIndexes
        0, // totalNumberOfActiveIndexes
        0, // totalSubscriptionsWithUnits
        0, // totalApprovedSubscriptions
        BIG_INT_ZERO, // totalDeposit
        BIG_INT_ZERO, // totalCFADeposit
        BIG_INT_ZERO, // totalGDADeposit
        BIG_INT_ZERO, // totalOutflowRate
        BIG_INT_ZERO, // totalCFAOutflowRate
        BIG_INT_ZERO, // totalGDAOutflowRate
        BIG_INT_ZERO, // totalAmountStreamedUntilUpdatedAt
        BIG_INT_ZERO, // totalCFAAmountStreamedUntilUpdatedAt
        BIG_INT_ZERO, // totalGDAAmountStreamedUntilUpdatedAt
        BIG_INT_ZERO, // totalAmountTransferredUntilUpdatedAt
        BIG_INT_ZERO, // totalAmountDistributedUntilUpdatedAt
        totalSupply, // totalSupply
        0,           // totalNumberOfAccounts
        0            // totalNumberOfHolders
    )
}