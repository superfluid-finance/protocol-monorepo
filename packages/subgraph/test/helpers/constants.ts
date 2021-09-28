import { ethers } from "ethers";
import { IAccountTokenSnapshot, IStreamHistory, ITokenStatistic } from "../interfaces";

export const enum FlowActionType {
    Create,
    Update,
    Delete,
}

export const INITIAL_ATS: IAccountTokenSnapshot = {
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

export const INITIAL_TOKEN_STATS: ITokenStatistic = {
    id: ethers.constants.AddressZero,
    updatedAtBlock: "0",
    updatedAtTimestamp: "0",
    totalNumberOfActiveStreams: 0,
    totalNumberOfClosedStreams: 0,
    totalNumberOfIndexes: 0,
    totalNumberOfActiveIndexes: 0,
    totalSubscriptions: 0,
    totalApprovedSubscriptions: 0,
    totalOutflowRate: "0",
    totalAmountStreamedUntilUpdatedAt: "0",
    totalAmountTransferredUntilUpdatedAt: "0",
    totalAmountDistributedUntilUpdatedAt: "0",
    token: { id: ethers.constants.AddressZero },
};

export const INITIAL_STREAM_HISTORY: IStreamHistory = {
	revisionIndex: "0",
	oldFlowRate: "0",
	streamedUntilUpdatedAt: "0"
}