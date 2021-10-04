import { Contract, ContractReceipt } from "@ethersproject/contracts";
import { expect } from "chai";
import { ConstantFlowAgreementV1 } from "../../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../../typechain/InstantDistributionAgreementV1";
import { FlowActionType } from "../helpers/constants";
import { fetchEventAndEnsureExistence } from "../helpers/helpers";
import {
    IBaseIDAEvent,
    IEvent,
    IExpectedIndexUpdated,
    IExpectedSubscriberEvent,
    IExpectedSubscriptionUnitsUpdated,
    IFlowUpdated,
    IIndexCreated,
    IIndexUpdated,
    ISubscriptionApproved,
    ISubscriptionUnitsUpdated,
} from "../interfaces";
import {
    getFlowUpdatedEvents,
    getIndexCreatedEvents,
    getIndexUpdatedEvents,
    getSubscriptionApprovedEvents,
    getSubscriptionRevokedEvents,
    getSubscriptionUnitsUpdatedEvents,
} from "../queries/eventQueries";

// Event Entity Validator Functions
/**
 * Query The Graph for FlowUpdated event and validate
 * that the properties on the event are as expected.
 * @param cfaV1
 * @param receipt
 * @param token
 * @param sender
 * @param receiver
 * @param flowRate
 * @param oldFlowRate
 * @param actionType
 */
export const fetchFlowUpdatedEventAndValidate = async (
    cfaV1: ConstantFlowAgreementV1,
    receipt: ContractReceipt,
    token: string,
    sender: string,
    receiver: string,
    flowRate: string,
    oldFlowRate: string,
    actionType: FlowActionType
) => {
    const flowUpdatedEvent = await fetchEventAndEnsureExistence<IFlowUpdated>(
        getFlowUpdatedEvents,
        receipt.transactionHash,
        "flowUpdateds",
        "FlowUpdated"
    );

    const senderNetFlow = await cfaV1.getNetFlow(token, sender);
    const receiverNetFlow = await cfaV1.getNetFlow(token, receiver);

    // validate the event data
    validateEventData(
        flowUpdatedEvent,
        {
            token: token.toLowerCase(),
            sender: sender.toLowerCase(),
            receiver: receiver.toLowerCase(),
            flowRate,
            totalSenderFlowRate: senderNetFlow.toString(),
            totalReceiverFlowRate: receiverNetFlow.toString(),
            oldFlowRate,
            type: actionType,
        },
        receipt
    );
};

export const fetchIDAEventAndValidate = async <
    EventType extends IEvent,
    ExpectedDataType
>(
    receipt: ContractReceipt,
    expectedData: ExpectedDataType,
    query: string,
    queryResultName: string,
    queryName: string
) => {
    const idaEvent = await fetchEventAndEnsureExistence<EventType>(
        query,
        receipt.transactionHash,
        queryResultName,
        queryName
    );

    validateEventData(idaEvent, expectedData, receipt);
};

// export const fetchIndexCreatedEventAndValidate = async (
//     receipt: ContractReceipt,
//     expectedData: IBaseIDAEvent
// ) => {
//     const indexCreatedEvent = await fetchEventAndEnsureExistence<IIndexCreated>(
//         getIndexCreatedEvents,
//         receipt.transactionHash,
//         "indexCreateds",
//         "IndexCreated"
//     );

//     validateEventData(indexCreatedEvent, expectedData, receipt);
// };

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

// export const fetchSubscriptionUnitsUpdatedEventAndValidate = async (
//     receipt: ContractReceipt,
//     expectedData: IExpectedSubscriptionUnitsUpdated
// ) => {
//     const subscriptionUnitsUpdatedEvent =
//         await fetchEventAndEnsureExistence<ISubscriptionUnitsUpdated>(
//             getSubscriptionUnitsUpdatedEvents,
//             receipt.transactionHash,
//             "subscriptionUnitsUpdateds",
//             "SubscriptionUnitsUpdated"
//         );
//     validateEventData(subscriptionUnitsUpdatedEvent, expectedData, receipt);
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
