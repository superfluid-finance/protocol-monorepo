import { FlowActionType } from "./index";

export type NetworkName =
    | "ropsten"
    | "rinkeby"
    | "goerli"
    | "kovan"
    | "xdai"
    | "matic"
    | "mumbai";

// request interfaces
export interface IIndexRequestFilter {
    readonly indexId?: string;
    readonly publisher?: string;
    readonly token?: string;
}
export interface IStreamRequestFilter {
    readonly sender?: string;
    readonly receiver?: string;
    readonly token?: string;
}
export interface IIndexSubscriptionRequestFilter {
    readonly subscriber?: string;
    readonly approved?: boolean;
}
export interface IPaginateRequest {
    readonly first?: number;
    readonly skip?: number;
}
export interface IPaginateResponse {
    readonly first: number;
    readonly skip: number;
}

// response interfaces
export interface IPaginatedResponse<T> {
    readonly hasNextPage: boolean;
    readonly response: T;
    readonly first: number;
    readonly skip: number;
}
export interface ISubgraphResponse<T> {
    readonly response: T;
}

export interface ILightEntity {
    readonly id: string;
}

export interface IEventEntityBase {
    readonly id: string;
    readonly blockNumber: string;
    readonly timestamp: string;
    readonly transactionHash: string;
}

export interface IFlowUpdatedEvent extends IEventEntityBase {
    readonly token: string;
    readonly sender: string;
    readonly receiver: string;
    readonly flowRate: string;
    readonly totalSenderFlowRate: string;
    readonly totalReceiverFlowRate: string;
    readonly userData: string;
    readonly oldFlowRate: string;
    readonly type: FlowActionType;
    readonly totalAmountStreamedUntilTimestamp: string;
}

export interface IHOLEntityBase {
    readonly id: string;
    readonly createdAtTimestamp: string;
    readonly createdAtBlockNumber: string;
}

export interface IHOLUpdateable extends IHOLEntityBase {
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlockNumber: string;
}

export interface IIndex extends IHOLUpdateable {
    readonly indexId: string;
    readonly indexValue: string;
    readonly totalSubscriptionsWithUnits: number;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly totalUnits: string;
    readonly totalAmountDistributedUntilUpdatedAt: string;
    readonly token: ISuperToken;
    readonly publisher: ILightEntity;
}

export interface IIndexSubscription extends IHOLUpdateable {
    readonly subscriber: ILightEntity;
    readonly approved: boolean;
    readonly units: string;
    readonly totalAmountReceivedUntilUpdatedAt: string;
    readonly indexValueUntilUpdatedAt: string;
    readonly index: IIndexSubscriptionIndex;
}

interface IIndexSubscriptionIndex {
    readonly id: string;
    readonly indexId: string;
    readonly indexValue: string;
    readonly token: ISuperToken;
}

export interface IStream extends IHOLUpdateable {
    readonly currentFlowRate: string;
    readonly streamedUntilUpdatedAt: string;
    readonly token: ISuperToken;
    readonly sender: ILightEntity;
    readonly receiver: ILightEntity;
    readonly flowUpdatedEvents: IStreamFlowUpdatedEvent[];
}
export interface IStreamFlowUpdatedEvent extends IFlowUpdatedEvent {}

export interface ISuperToken extends IHOLEntityBase {
    readonly name: string;
    readonly symbol: string;
    readonly isListed: boolean;
    readonly underlyingAddress: string;
}

export interface IAggregateEntityBase {
    readonly id: string;
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlockNumber: string;
}

export interface ILightAccountTokenSnapshot extends IAggregateEntityBase {
    readonly totalNumberOfActiveStreams: number;
    readonly totalNumberOfClosedStreams: number;
    readonly totalSubscriptionsWithUnits: number;
    readonly totalApprovedSubscriptions: number;
    readonly balanceUntilUpdatedAt: string;
    readonly totalNetFlowRate: string;
    readonly totalInflowRate: string;
    readonly totalOutflowRate: string;
    readonly totalAmountStreamedUntilUpdatedAt: string;
    readonly totalAmountTransferredUntilUpdatedAt: string;
    readonly account: ILightEntity;
    readonly token: ISuperToken;
}
