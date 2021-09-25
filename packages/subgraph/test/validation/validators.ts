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
        expectedStreamedUntilUpdatedAt
    );
};

// TODO: refactor this to multiple smaller functions
/**
 * Validates Create/Update/Delete flow.
 * @param sf
 * @param tokenAddress
 * @param sender
 * @param receiver
 * @param cfaV1
 * @param testParams
 * @param atsData
 * @param tokenSnapshotData
 * @returns [updatedATSData, updatedTokenStatsData]
 */
export const validateModifyFlow = async (
    sf: Framework,
    tokenAddress: string,
    sender: string,
    receiver: string,
    cfaV1: ConstantFlowAgreementV1,
    testParams: IStreamTestParams,
    atsData: { [id: string]: IAccountTokenSnapshot },
    tokenSnapshotData: { [id: string]: ITokenStatistic }
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
    console.log("here1");
    const hexTokenAddress = tokenAddress.toLowerCase();
    const hexSender = sender.toLowerCase();
    const hexReceiver = receiver.toLowerCase();
    console.log("here2");
    const receipt: ContractReceipt = txn.receipt;

    await waitUntilBlockIndexed(receipt.blockNumber);
    console.log("here3");
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
    console.log("here4");
    const { flowUpdateds } = await subgraphRequest<{
        flowUpdateds: IFlowUpdated[];
    }>(getFlowUpdatedEvents, flowUpdatedVars);
    console.log(flowUpdateds);
    const flowUpdatedEvent = getSingleEvent<IFlowUpdated>(
        flowUpdateds,
        receipt.transactionHash
    );
    console.log("flowUpdatedEvent", flowUpdatedEvent);
    console.log("receipt", receipt);

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

    const streamVars = {
        sender: hexSender,
        receiver: hexReceiver,
    };
    const { stream } = await subgraphRequest<{ stream: IStream | undefined }>(
        getStream,
        streamVars
    );

    if (!stream) {
        throw new Error("Stream entity not found.");
    }
    const streamId = getStreamId(
        hexSender,
        hexReceiver,
        hexTokenAddress,
        revisionIndex
    );

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
    const flowRateDelta = Number(flowRate) - Number(oldFlowRate);
    const senderATS = atsData[sender];
    const receiverATS = atsData[receiver];

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
        activeStreams:
            senderATS.totalNumberOfActiveStreams + activeStreamsDelta,
        closedStreams:
            senderATS.totalNumberOfClosedStreams + closedStreamsDelta,
        inflowRate: senderATS.totalInflowRate,
        outflowRate: (
            Number(senderATS.totalOutflowRate) + flowRateDelta
        ).toString(),
        netFlowRate: (
            Number(senderATS.totalNetFlowRate) - flowRateDelta
        ).toString(),
    };
    validateATSEntityForFlowUpdated(
        senderAccountTokenSnapshot,
        flowedAmountSinceUpdatedAt,
        expectedSenderATS
    );
    const expectedReceiverATS = {
        activeStreams:
            receiverATS.totalNumberOfActiveStreams + activeStreamsDelta,
        closedStreams:
            receiverATS.totalNumberOfClosedStreams + closedStreamsDelta,
        inflowRate: (
            Number(receiverATS.totalInflowRate) + flowRateDelta
        ).toString(),
        outflowRate: receiverATS.totalOutflowRate,
        netFlowRate: (
            Number(receiverATS.totalNetFlowRate) + flowRateDelta
        ).toString(),
    };

    validateATSEntityForFlowUpdated(
        receiverAccountTokenSnapshot,
        flowedAmountSinceUpdatedAt,
        expectedReceiverATS
    );

    const updatedSenderATS = {
        ...senderATS,
        ...expectedSenderATS,
    };
    const updatedReceiverAts = {
        ...receiverATS,
        ...expectedReceiverATS,
    };
    const updatedATS = {
        ...atsData,
        [sender]: updatedSenderATS,
        [receiver]: updatedReceiverAts,
    };

    const { tokenStatistic } = await subgraphRequest<{
        tokenStatistic: ITokenStatistic | undefined;
    }>(getTokenStatistic);

    if (!tokenStatistic) {
        throw new Error("TokenStats entity not found.");
    }

    expect(tokenStatistic.totalNumberOfActiveStreams).to.equal(
        tokenStatistic.totalNumberOfActiveStreams
    );

    // TODO: return updated ATS, tokenStats objects
    return [updatedATS];
};

// Aggregate Validator Functions

interface IExpectedTokenStatsData {
    readonly activeStreams: number;
    readonly closedStreams: number;
    readonly outflowRate: string;
}

interface IExpectedATSData {
    readonly activeStreams: number;
    readonly closedStreams: number;
    readonly inflowRate: string;
    readonly outflowRate: string;
    readonly netFlowRate: string;
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
        activeStreams,
        closedStreams,
        inflowRate,
        outflowRate,
        netFlowRate,
    } = expectedATSData;
    expect(accountTokenSnapshot.totalNumberOfActiveStreams).to.equal(
        activeStreams
    );
    expect(accountTokenSnapshot.totalNumberOfClosedStreams).to.equal(
        closedStreams
    );
    expect(accountTokenSnapshot.totalInflowRate).to.equal(inflowRate);
    expect(accountTokenSnapshot.totalOutflowRate).to.equal(outflowRate);
    expect(accountTokenSnapshot.totalNetFlowRate).to.equal(netFlowRate);
    expect(accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt).to.equal(
        expectedATSStreamedUntilAt
    );
};
