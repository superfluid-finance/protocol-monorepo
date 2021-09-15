import { expect } from "chai";
import { IEvent } from "../interfaces";
import { ContractReceipt } from "ethers";
import { getEventId } from "../helpers/helpers";

export const validateData = <T>(
    queriedData: T,
    expectedData: { [key: string]: any }
) => {
    const propertiesToValidate = Object.keys(expectedData);
    for (let i = 0; i < propertiesToValidate.length; i++) {
        expect(queriedData[propertiesToValidate[i]]).to.eql(
            expectedData[propertiesToValidate[i]]
        );
    }
};

export const validateBaseEventData = (
    queriedEvent: IEvent,
    receipt: ContractReceipt
) => {
    const eventId = getEventId(receipt);
    expect(eventId).to.eq(queriedEvent.id);
    expect(receipt.transactionHash.toLowerCase()).to.eq(
        queriedEvent.transactionHash
    );
    expect(receipt.blockNumber.toString()).to.eq(queriedEvent.blockNumber);
};

export const validateEventData = (
    queriedEvent: IEvent,
    expectedData: { [key: string]: any },
    receipt: ContractReceipt
) => {
    validateBaseEventData(queriedEvent, receipt);
    validateData(queriedEvent, expectedData);
};
