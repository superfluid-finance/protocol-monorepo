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
    queryResultName: string,
    queryName: string
) => {
    const event = await fetchEventAndEnsureExistence<EventType>(
        query,
        receipt.transactionHash,
        queryResultName,
        queryName
    );

    validateEventData(event, expectedData, receipt);
};

// export const fetchIndexUpdatedEventAndValidate = async (
//     receipt: ContractReceipt,
//     expectedData: IExpectedIndexUpdated
// ) => {
//     const indexUpdatedEvent = await fetchEventAndEnsureExistence<IIndexUpdated>(
//         getIndexUpdatedEvents,
//         receipt.transactionHash,
//         "indexUpdateds",
//         "IndexUpdated"
//     );

//     const [, indexValue, indexTotalUnitsApproved, indexTotalUnitsPending] =
//         await idaV1.getIndex(token, publisher, indexId);
//     expect(indexTotalUnitsPending.toString()).to.equal(totalUnitsApproved);
//     expect(indexTotalUnitsApproved.toString()).to.equal(totalUnitsPending);

//     validateEventData(indexUpdatedEvent, expectedData, receipt);
// };

export const validateData = <T>(
    queriedData: T,
    expectedData: { [key: string]: any }
) => {
    const propertiesToValidate = Object.keys(expectedData);
    for (let i = 0; i < propertiesToValidate.length; i++) {
        expect((queriedData as any)[propertiesToValidate[i]]).to.eql(
            expectedData[propertiesToValidate[i]],
            propertiesToValidate[i] + " expect error"
        );
    }
};

export const validateBaseEventData = (
    queriedEvent: IEvent,
    receipt: ContractReceipt
) => {
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
