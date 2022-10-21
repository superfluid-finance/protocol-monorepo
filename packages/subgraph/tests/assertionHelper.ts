import { ethereum } from "@graphprotocol/graph-ts";
import {
    assert,
} from "matchstick-as/assembly/index";
import { createEventID, getOrder } from "../src/utils";

/**
 * Asserts that the "base" properties on our Event entity are correct
 * @param event The event we are checking
 * @param eventName The name of the event
 * @returns The id of the event (based on our createEventID function)
 */
export function eventBasePropertyAssertion(event: ethereum.Event, eventName: string): string {
    const entityType = eventName + "Event";
    const id = createEventID(
        eventName,
        event
    );
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