import { ContractReceipt } from "@ethersproject/contracts";
import { expect } from "chai";
import { fetchEventAndEnsureExistence } from "../helpers/helpers";
import { IEvent } from "../interfaces";

// Event Entity Validator Functions

export const fetchEventAndValidate = async <
    EventType extends IEvent,
    ExpectedDataType
>(
    receipt: ContractReceipt,
    expectedData: ExpectedDataType,
    query: string,
    queryName: string
) => {
    const event = await fetchEventAndEnsureExistence<EventType>(
        query,
        receipt.transactionHash,
        queryName
    );

    // Note: we parse the name of the query (e.g. FlowUpdatedEvent)
    // and use this to validate that the name property has been set properly.
    const parsedQueryName = queryName.split("Event")[0];
    validateEventData(event, expectedData, receipt, parsedQueryName);

    return event;
};

export const validateData = <T>(
    queriedData: T,
    expectedData: { [key: string]: any }
) => {
    const propertiesToValidate = Object.keys(expectedData);
    for (let i = 0; i < propertiesToValidate.length; i++) {
        expect((queriedData as any)[propertiesToValidate[i]]).to.eql(
            expectedData[propertiesToValidate[i]],
            propertiesToValidate[i] + " expect error for event"
        );
    }
};

export const validateBaseEventData = (
    queriedEvent: IEvent,
    receipt: ContractReceipt,
    queryName: string
) => {
    expect(receipt.transactionHash.toLowerCase()).to.eq(
        queriedEvent.transactionHash
    );
    expect(receipt.blockNumber.toString()).to.eq(queriedEvent.blockNumber);
    expect(queriedEvent.name).to.eq(queryName);
};

export const validateEventData = (
    queriedEvent: IEvent,
    expectedData: { [key: string]: any },
    receipt: ContractReceipt,
    queryName: string
) => {
    validateBaseEventData(queriedEvent, receipt, queryName);
    validateData(queriedEvent, expectedData);
};
