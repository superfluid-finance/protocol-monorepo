import { TransactionResponse } from "@ethersproject/providers";
import { expect } from "chai";
import { fetchEventAndEnsureExistence } from "../helpers/helpers";
import { IEvent } from "../interfaces";

// Event Entity Validator Functions

export const fetchEventAndValidate = async <
    EventType extends IEvent,
    ExpectedDataType
>(
    txnResponse: TransactionResponse,
    expectedData: ExpectedDataType,
    query: string,
    eventName: string
) => {
    const event = await fetchEventAndEnsureExistence<EventType>(
        query,
        txnResponse.hash,
        eventName
    );

    // Note: we parse the name of the query (e.g. FlowUpdatedEvent)
    // and use this to validate that the name property has been set properly.
    const parsedQueryName = eventName.split("Event")[0];
    validateEventData(event, expectedData, txnResponse, parsedQueryName);

    return event;
};

export const validateData = <T>(
    queriedData: T,
    expectedData: { [key: string]: any }
) => {
    const propertiesToValidate = Object.keys(expectedData);
    for (let i = 0; i < propertiesToValidate.length; i++) {
        expect(String((queriedData as any)[propertiesToValidate[i]])).to.eql(
            String(expectedData[propertiesToValidate[i]]),
            propertiesToValidate[i] + " expect error for event"
        );
    }
};

export const validateBaseEventData = (
    queriedEvent: IEvent,
    txnResponse: TransactionResponse,
    queryName: string
) => {
    expect(txnResponse.hash.toLowerCase()).to.eq(queriedEvent.transactionHash);
    expect(txnResponse.blockNumber!.toString()).to.eq(queriedEvent.blockNumber);
    expect(txnResponse.gasPrice!.toString()).to.eq(queriedEvent.gasPrice);
    expect(queriedEvent.name).to.eq(queryName);
};

export const validateEventData = (
    queriedEvent: IEvent,
    expectedData: { [key: string]: any },
    txnResponse: TransactionResponse,
    queryName: string
) => {
    validateBaseEventData(queriedEvent, txnResponse, queryName);
    validateData(queriedEvent, expectedData);
};
