import { ethers } from "hardhat";
import { ContractReceipt } from "ethers";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import {
    IAccountTokenSnapshot,
    IFlowUpdated,
    IStream,
    IStreamHistory,
    IStreamTestParams,
    ITokenStatistic,
} from "../interfaces";
import {
    getStreamId,
    subgraphRequest,
    waitUntilBlockIndexed,
} from "../helpers/helpers";
import { ConstantFlowAgreementV1 } from "../../typechain/ConstantFlowAgreementV1";
import { getFlowUpdatedEvents, getSingleEvent } from "../queries/eventQueries";
import { getStream } from "../queries/holQueries";
import { getTokenStatistic } from "../queries/aggregateQueries";
import {
    FlowActionType,
    INITIAL_ATS,
    INITIAL_TOKEN_STATS,
} from "../helpers/constants";
import { validateEventData } from "./eventValidators";
import { validateStreamEntity } from "./holValidators";
import {
    getATSDataForFlowUpdated,
    getTokenStatsData,
    validateATSEntityForFlowUpdated,
    validateTokenStatsEntityForFlowUpdated,
} from "./aggregateValidators";
import { ConstantFlowAgreementV1Helper } from "@superfluid-finance/js-sdk/src/ConstantFlowAgreementV1Helper";

/**
 * Create/Update/Delete a flow between a sender and receiver.
 * @param sf
 * @param cfaV1
 * @param actionType
 * @param token
 * @param sender
 * @param receiver
 * @param newFlowRate
 * @returns txnReceipt, flow updatedAt (on-chain), flowRate (current on-chain)
 */
export const modifyFlow = async (
    sf: Framework,
    cfaV1: ConstantFlowAgreementV1,
    actionType: FlowActionType,
    token: string,
    sender: string,
    receiver: string,
    newFlowRate: number
) => {
    const actionToTypeStringMap = new Map([
        [FlowActionType.Create, "Create"],
        [FlowActionType.Update, "Update"],
        [FlowActionType.Delete, "Delete"],
    ]);
    console.log(
        `********************** ${actionToTypeStringMap.get(
            actionType
        )} a flow **********************`
    );
    const sfCFA = sf.cfa as ConstantFlowAgreementV1Helper;
    // any because it the txn.receipt doesn't exist on
    // Transaction
    const txn: any =
        actionType === FlowActionType.Create
            ? await sfCFA.createFlow({
                  superToken: token,
                  sender,
                  receiver,
                  flowRate: newFlowRate.toString(),
                  userData: "0x",
                  onTransaction: () => {},
              })
            : actionType === FlowActionType.Update
            ? await sfCFA.updateFlow({
                  superToken: token,
                  sender,
                  receiver,
                  flowRate: newFlowRate.toString(),
                  userData: "0x",
                  onTransaction: () => {},
              })
            : await sfCFA.deleteFlow({
                  superToken: token,
                  sender,
                  receiver,
                  by: "",
                  userData: "0x",
                  onTransaction: () => {},
              });

    const receipt: ContractReceipt = txn.receipt;

    await waitUntilBlockIndexed(receipt.blockNumber);

    const [updatedAt, onChainFlowRate] = await cfaV1.getFlow(
        token,
        sender,
        receiver
    );
    const stringFlowRate = onChainFlowRate.toString();

    return { receipt, updatedAt, flowRate: stringFlowRate };
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
    const { actionType, flowRate: newFlowRate, streamHistory } = testParams;
    const { oldFlowRate, revisionIndex, previousUpdatedAt } = streamHistory;

    const { receipt, updatedAt, flowRate } = await modifyFlow(
        sf,
        cfaV1,
        actionType,
        tokenAddress,
        sender,
        receiver,
        newFlowRate
    );

    const hexToken = tokenAddress.toLowerCase();
    const hexSender = sender.toLowerCase();
    const hexReceiver = receiver.toLowerCase();

    // ********************** Validate FlowUpdated Entity **********************
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

    const totalSenderFlowRate = await cfaV1.getNetFlow(tokenAddress, sender);
    const totalReceiverFlowRate = await cfaV1.getNetFlow(
        tokenAddress,
        receiver
    );

    // validate the event data
    validateEventData(
        flowUpdatedEvent,
        {
            token: hexToken,
            sender: hexSender,
            receiver: hexReceiver,
            flowRate,
            totalSenderFlowRate: totalSenderFlowRate.toString(),
            totalReceiverFlowRate: totalReceiverFlowRate.toString(),
            oldFlowRate: oldFlowRate,
            type: actionType,
        },
        receipt
    );
    // ********************** Validate Stream Entity **********************

    const streamId = getStreamId(
        hexSender,
        hexReceiver,
        hexToken,
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

    const flowedAmountSinceUpdatedAt =
        previousUpdatedAt == null
            ? 0
            : (Number(updatedAt.toString()) - Number(previousUpdatedAt)) *
              Number(oldFlowRate);

    validateStreamEntity(
        stream,
        flowedAmountSinceUpdatedAt,
        streamId,
        flowRate
    );

    const updatedStreamHistory: IStreamHistory = {
        oldFlowRate: newFlowRate.toString(),
        revisionIndex:
            actionType === FlowActionType.Delete
                ? (Number(revisionIndex) + 1).toString()
                : revisionIndex,
        previousUpdatedAt: Number(updatedAt.toString()),
    };

    // ********************** Validate Aggregate Entities For Stream **********************

    const flowRateDelta = newFlowRate - Number(oldFlowRate);
    const senderATSId = hexSender + "-" + hexToken;
    const receiverATSId = hexReceiver + "-" + hexToken;
    const senderATS = atsData[senderATSId] || INITIAL_ATS;
    const receiverATS = atsData[receiverATSId] || INITIAL_ATS;
    const tokenStats = tokenStatsData[hexToken] || INITIAL_TOKEN_STATS;
	console.log("atsData", atsData);
	console.log("senderATS", senderATS);

    const {
        expectedATS: expectedSenderATS,
        graphATS: graphSenderATS,
        updatedATS: updatedSenderATS,
    } = await getATSDataForFlowUpdated(
        senderATSId,
        senderATS,
        actionType,
        flowRateDelta,
        flowedAmountSinceUpdatedAt,
        true
    );

	console.log("expectedSenderATS", expectedSenderATS);

    validateATSEntityForFlowUpdated(
        graphSenderATS,
        flowedAmountSinceUpdatedAt,
        expectedSenderATS
    );
    const {
        expectedATS: expectedReceiverATS,
        graphATS: graphReceiverATS,
        updatedATS: updatedReceiverATS,
    } = await getATSDataForFlowUpdated(
        receiverATSId,
        receiverATS,
        actionType,
        flowRateDelta,
        flowedAmountSinceUpdatedAt,
        false
    );

    validateATSEntityForFlowUpdated(
        graphReceiverATS,
        flowedAmountSinceUpdatedAt,
        expectedReceiverATS
    );

    const updatedATS: { [id: string]: IAccountTokenSnapshot } = {
        ...atsData,
        [senderATSId]: updatedSenderATS,
        [receiverATSId]: updatedReceiverATS,
    };

    const { graphTokenStats, expectedTokenStats, updatedTokenStats } =
        await getTokenStatsData(
            hexToken,
            tokenStats,
            actionType,
            flowRateDelta,
            flowedAmountSinceUpdatedAt
        );
    validateTokenStatsEntityForFlowUpdated(
        graphTokenStats,
        flowedAmountSinceUpdatedAt,
        expectedTokenStats
    );

    return {
        updatedATS,
        updatedTokenStats,
        updatedStreamHistory,
    };
};
