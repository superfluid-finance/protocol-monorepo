import { expect } from "chai";
import { ethers } from "hardhat";
import { ContractReceipt } from "ethers";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import {
    IAccountTokenSnapshot,
    IEvent,
    IFlowUpdated,
    IStream,
    ITokenStatistic,
} from "../interfaces";
import {
    getStreamId,
    monthlyToSecondRate,
    subgraphRequest,
    waitUntilBlockIndexed,
} from "../helpers/helpers";
import { ConstantFlowAgreementV1 } from "../../typechain/ConstantFlowAgreementV1";
import { getFlowUpdatedEvents, getSingleEvent } from "../queries/eventQueries";
import { getStream } from "../queries/holQueries";
import {
    getAccountTokenSnapshot,
    getTokenStatistic,
} from "../queries/aggregateQueries";

// Event Entity Validator Functions

export const validateData = <T>(
    queriedData: T,
    expectedData: { [key: string]: any }
) => {
    const propertiesToValidate = Object.keys(expectedData);
    for (let i = 0; i < propertiesToValidate.length; i++) {
        expect((queriedData as any)[propertiesToValidate[i]]).to.eql(
            expectedData[propertiesToValidate[i]]
        );
    }
};

// TODO: figure out why the logIndex is inconsistent between
// receipt and queried data.
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
const enum FlowActionType {
    Create,
    Update,
    Delete,
}

const INITIAL_ATS: IAccountTokenSnapshot = {
    id: ethers.constants.AddressZero,
    updatedAtBlock: "0",
    updatedAtTimestamp: "0",
    totalNumberOfActiveStreams: 0,
    totalNumberOfClosedStreams: 0,
    totalSubscriptions: 0,
    totalApprovedSubscriptions: 0,
    balanceUntilUpdatedAt: "0",
    totalNetFlowRate: "0",
    totalInflowRate: "0",
    totalOutflowRate: "0",
    totalAmountStreamedUntilUpdatedAt: "0",
    totalAmountTransferredUntilUpdatedAt: "0",
    account: { id: ethers.constants.AddressZero },
    token: { id: ethers.constants.AddressZero },
};
// HOL Entity Validator Functions

interface IStreamTestParams {
    readonly actionType: FlowActionType;
    readonly flowRate: number;
    readonly oldFlowRate: string;
    readonly previousUpdatedAt?: number;
    readonly revisionIndex: string;
}

const validateStreamEntity = (
    subgraphStream: IStream,
    flowedAmountSinceUpdatedAt: number,
    streamId: string,
    receiverFlowRate: string
) => {
    const expectedStreamedUntilUpdatedAt =
        Number(subgraphStream.streamedUntilUpdatedAt) +
        flowedAmountSinceUpdatedAt;

    expect(subgraphStream.id).to.be.equal(streamId);
    expect(subgraphStream.currentFlowRate).to.equal(receiverFlowRate);
    expect(subgraphStream.streamedUntilUpdatedAt).to.be.equal(
        expectedStreamedUntilUpdatedAt.toString()
    );
};

// TODO: refactor this to multiple smaller functions
// make sure it's composable and easy to utilize the different parts
/**
 * Validates Create/Update/Delete flow.
 * Validates the event entity.
 * Validates the Stream entity.
 * Validates the relevant properties on the aggregate entities.
 * @param sf
 * @param tokenAddress
 * @param sender
 * @param receiver
 * @param cfaV1
 * @param testParams
 * @param atsData
 * @param tokenStatsData
 * @returns object containing updatedATSData, updatedTokenStatsData
 */
export const validateModifyFlow = async (
    sf: Framework,
    tokenAddress: string,
    sender: string,
    receiver: string,
    cfaV1: ConstantFlowAgreementV1,
    testParams: IStreamTestParams,
    atsData: { [id: string]: IAccountTokenSnapshot },
    tokenStatsData: { [id: string]: ITokenStatistic }
) => {
    console.log("********************** CREATE A FLOW **********************");
    const {
        actionType,
        flowRate,
        oldFlowRate,
        previousUpdatedAt,
        revisionIndex,
    } = testParams;
    const formattedFlowRate = monthlyToSecondRate(flowRate);
    const flowedAmountSinceUpdatedAt =
        previousUpdatedAt == null
            ? 0
            : (previousUpdatedAt - Number(previousUpdatedAt)) *
              Number(oldFlowRate);

    const txn = await sf.cfa!.createFlow({
        superToken: tokenAddress,
        sender,
        receiver,
        flowRate: formattedFlowRate,
    });
    const hexTokenAddress = tokenAddress.toLowerCase();
    const hexSender = sender.toLowerCase();
    const hexReceiver = receiver.toLowerCase();
    const receipt: ContractReceipt = txn.receipt;

    await waitUntilBlockIndexed(receipt.blockNumber);
    const [, onChainFlowRate] = await cfaV1.getFlow(
        tokenAddress,
        sender,
        receiver
    );
    const receiverFlowRate = onChainFlowRate.toString();
    const senderFlowRate = onChainFlowRate
        .mul(ethers.BigNumber.from(-1))
        .toString();

    const flowUpdatedVars = {
        sender,
        receiver,
    };
    const { flowUpdateds } = await subgraphRequest<{
        flowUpdateds: IFlowUpdated[];
    }>(getFlowUpdatedEvents, flowUpdatedVars);
    const flowUpdatedEvent = getSingleEvent<IFlowUpdated>(
        flowUpdateds,
        receipt.transactionHash
    );

    if (!flowUpdatedEvent) {
        throw new Error("FlowUpdated entity not found.");
    }

    console.log(
        "********************** Validate FlowUpdated Entity **********************"
    );
    // validate the event data
    validateEventData(
        flowUpdatedEvent,
        {
            token: hexTokenAddress,
            sender: hexSender,
            receiver: hexReceiver,
            flowRate: receiverFlowRate,
            totalSenderFlowRate: senderFlowRate,
            totalReceiverFlowRate: receiverFlowRate,
            oldFlowRate: oldFlowRate,
            type: actionType,
        },
        receipt
    );

    console.log(
        "********************** Validate Stream Entity **********************"
    );

    const streamId = getStreamId(
        hexSender,
        hexReceiver,
        hexTokenAddress,
        revisionIndex
    );

    const { stream } = await subgraphRequest<{ stream: IStream | undefined }>(
        getStream,
        {
            id: streamId,
        }
    );

    if (!stream) {
        throw new Error("Stream entity not found.");
    }

    validateStreamEntity(
        stream,
        flowedAmountSinceUpdatedAt,
        streamId,
        receiverFlowRate
    );

    console.log(
        "********************** Validate Aggregate Entities For Stream **********************"
    );

    const activeStreamsDelta =
        actionType === FlowActionType.Create
            ? 1
            : actionType === FlowActionType.Update
            ? 0
            : -1;
    const closedStreamsDelta = actionType === FlowActionType.Delete ? 1 : 0;
    const flowRateDelta = formattedFlowRate - Number(oldFlowRate);
    const senderATS = atsData[sender] || INITIAL_ATS;
    const receiverATS = atsData[receiver] || INITIAL_ATS;

    const senderATSVars = {
        id: hexSender + "-" + hexTokenAddress,
    };
    const receiverATSVars = {
        id: hexReceiver + "-" + hexTokenAddress,
    };
    const { accountTokenSnapshot: senderAccountTokenSnapshot } =
        await subgraphRequest<{
            accountTokenSnapshot: IAccountTokenSnapshot | undefined;
        }>(getAccountTokenSnapshot, senderATSVars);
    const { accountTokenSnapshot: receiverAccountTokenSnapshot } =
        await subgraphRequest<{
            accountTokenSnapshot: IAccountTokenSnapshot | undefined;
        }>(getAccountTokenSnapshot, receiverATSVars);

    if (!senderAccountTokenSnapshot || !receiverAccountTokenSnapshot) {
        throw new Error("ATS entity not found.");
    }

    const expectedSenderATS = {
        id: senderATSVars.id,
        updatedAtBlock: senderAccountTokenSnapshot.updatedAtBlock,
        updatedAtTimestamp: senderAccountTokenSnapshot.updatedAtTimestamp,
        totalNumberOfActiveStreams:
            senderATS.totalNumberOfActiveStreams + activeStreamsDelta,
        totalNumberOfClosedStreams:
            senderATS.totalNumberOfClosedStreams + closedStreamsDelta,
        totalNetFlowRate: (
            Number(senderATS.totalNetFlowRate) - flowRateDelta
        ).toString(),
        totalInflowRate: senderATS.totalInflowRate,
        totalOutflowRate: (
            Number(senderATS.totalOutflowRate) + flowRateDelta
        ).toString(),
        totalAmountStreamedUntilUpdatedAt: (
            Number(senderATS.totalAmountStreamedUntilUpdatedAt) +
            flowedAmountSinceUpdatedAt
        ).toString(),
        account: { id: sender },
        token: { id: tokenAddress },
    };
    console.log("expectedSenderATS", expectedSenderATS);
    validateATSEntityForFlowUpdated(
        senderAccountTokenSnapshot,
        flowedAmountSinceUpdatedAt,
        expectedSenderATS
    );
    const expectedReceiverATS = {
        id: receiverATSVars.id,
        updatedAtBlock: receiverAccountTokenSnapshot.updatedAtBlock,
        updatedAtTimestamp: receiverAccountTokenSnapshot.updatedAtTimestamp,
        totalNumberOfActiveStreams:
            receiverATS.totalNumberOfActiveStreams + activeStreamsDelta,
        totalNumberOfClosedStreams:
            receiverATS.totalNumberOfClosedStreams + closedStreamsDelta,
        totalNetFlowRate: (
            Number(receiverATS.totalNetFlowRate) + flowRateDelta
        ).toString(),
        totalInflowRate: (
            Number(receiverATS.totalInflowRate) + flowRateDelta
        ).toString(),
        totalOutflowRate: receiverATS.totalOutflowRate,
        totalAmountStreamedUntilUpdatedAt: (
            Number(senderATS.totalAmountStreamedUntilUpdatedAt) +
            flowedAmountSinceUpdatedAt
        ).toString(),
        account: { id: receiver },
        token: { id: tokenAddress },
    };
    console.log("expectedReceiverATS", expectedReceiverATS);
    validateATSEntityForFlowUpdated(
        receiverAccountTokenSnapshot,
        flowedAmountSinceUpdatedAt,
        expectedReceiverATS
    );

    const updatedSenderATS = {
        ...senderATS,
        ...expectedSenderATS,
    };
    const updatedReceiverATS = {
        ...receiverATS,
        ...expectedReceiverATS,
    };
    const updatedATSData = {
        ...atsData,
        [senderATSVars.id]: updatedSenderATS,
        [receiverATSVars.id]: updatedReceiverATS,
    };
    const hexToken = tokenAddress.toLowerCase();
    const { tokenStatistic: updatedTokenStatsData } = await subgraphRequest<{
        tokenStatistic: ITokenStatistic | undefined;
    }>(getTokenStatistic, { id: hexToken });

    if (!updatedTokenStatsData) {
        throw new Error("TokenStats entity not found.");
    }
    // TODO: token stats tests for flowUpdated to be implemented

    return {
        updatedATS: updatedATSData,
        tokenStatistic: updatedTokenStatsData,
    };
};

// Aggregate Validator Functions

interface IExpectedTokenStatsData {
    readonly activeStreams: number;
    readonly closedStreams: number;
    readonly outflowRate: string;
}

interface IExpectedATSData {
    readonly totalNumberOfActiveStreams: number;
    readonly totalNumberOfClosedStreams: number;
    readonly totalInflowRate: string;
    readonly totalOutflowRate: string;
    readonly totalNetFlowRate: string;
}

export const validateATSEntityForFlowUpdated = (
    accountTokenSnapshot: IAccountTokenSnapshot,
    flowedAmountSinceUpdatedAt: number,
    expectedATSData: IExpectedATSData
) => {
    const expectedATSStreamedUntilAt = (
        Number(accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt) +
        flowedAmountSinceUpdatedAt
    ).toString();
    const {
        totalNumberOfActiveStreams,
        totalNumberOfClosedStreams,
        totalInflowRate,
        totalOutflowRate,
        totalNetFlowRate,
    } = expectedATSData;
    expect(accountTokenSnapshot.totalNumberOfActiveStreams).to.equal(
        totalNumberOfActiveStreams
    );
    expect(accountTokenSnapshot.totalNumberOfClosedStreams).to.equal(
        totalNumberOfClosedStreams
    );
    expect(accountTokenSnapshot.totalInflowRate).to.equal(totalInflowRate);
    expect(accountTokenSnapshot.totalOutflowRate).to.equal(totalOutflowRate);
    expect(accountTokenSnapshot.totalNetFlowRate).to.equal(totalNetFlowRate);
    expect(accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt).to.equal(
        expectedATSStreamedUntilAt
    );
};
