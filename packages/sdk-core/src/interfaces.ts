import { ethers } from "ethers";
import { NetworkName } from "./types";
// TODO (0xdavinchee): reorganize this
// Maybe moving these into categorical files
// makes more sense than stuffing them all here

// read request interfaces
export interface IAccountTokenSnapshotFilter {
    readonly account?: string;
    readonly token?: string;
}

export interface IAccountEventsFilter {
    readonly account: string;
    readonly timestamp_gte: string;
}
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

// write request interfaces
export interface ISuperTokenModifyFlowParams {
    readonly flowRate?: string;
    readonly receiver: string;
    readonly sender?: string;
    readonly userData?: string;
}
export interface ISuperTokenCreateFlowParams
    extends ISuperTokenModifyFlowParams {
    readonly flowRate: string;
    readonly sender: string;
}
export interface ISuperTokenUpdateFlowParams
    extends ISuperTokenCreateFlowParams {}
export interface ISuperTokenDeleteFlowParams
    extends ISuperTokenModifyFlowParams {
    readonly sender: string;
}

export interface ISuperTokenBaseIDAParams {
    readonly indexId: string;
    readonly publisher?: string;
    readonly userData?: string;
}
export interface ISuperTokenBaseSubscriptionParams
    extends ISuperTokenBaseIDAParams {
    readonly subscriber: string;
}
export interface ISuperTokenDistributeParams extends ISuperTokenBaseIDAParams {
    readonly amount: string;
}
export interface ISuperTokenUpdateIndexValueParams
    extends ISuperTokenBaseIDAParams {
    readonly indexValue: string;
}
export interface ISuperTokenUpdateSubscriptionUnitsParams
    extends ISuperTokenBaseSubscriptionParams {
    readonly units: string;
}
export interface IModifyFlowParams {
    readonly receiver: string;
    readonly superToken: string;
    readonly flowRate?: string;
    readonly sender?: string;
    readonly userData?: string;
}
export interface ICreateFlowParams extends IModifyFlowParams {
    readonly flowRate: string;
}

export interface IUpdateFlowParams extends ICreateFlowParams {}
export interface IDeleteFlowParams extends IModifyFlowParams {
    readonly sender: string;
}

export interface IBaseIDAParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher?: string;
    readonly userData?: string;
}
export interface IBaseSubscriptionParams extends IBaseIDAParams {
    readonly subscriber: string;
}

export interface IDistributeParams extends IBaseIDAParams {
    readonly amount: string;
}

export interface IUpdateIndexValueParams extends IBaseIDAParams {
    readonly indexValue: string;
}

export interface IUpdateSubscriptionUnitsParams
    extends IBaseSubscriptionParams {
    readonly units: string;
}

export interface IApproveSubscriptionParams extends IBaseSubscriptionParams {
    readonly publisher: string;
}

export interface IRevokeSubscriptionParams extends IBaseSubscriptionParams {
    readonly publisher: string;
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
    readonly type: number; // TODO(KK): Can't use the union type here because we get number/Int from subgraph.
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

// Internal Interfaces

export interface IResolverData {
    readonly subgraphAPIEndpoint: string;
    readonly networkName: NetworkName;
    readonly resolverAddress: string;
}

export interface ISignerConstructorOptions {
    readonly web3Provider?: ethers.providers.Web3Provider; // Web3Provider (client side - metamask, web3modal)
    readonly provider?: ethers.providers.Provider; // Provider
    readonly privateKey?: string; // private key (best to store a test account PK in .env file)
    readonly signer?: ethers.Signer; // ethers.Wallet
}

export interface IConfig {
    readonly hostAddress: string;
    readonly superTokenFactoryAddress: string;
    readonly cfaV1Address: string;
    readonly idaV1Address: string;
}

export interface IAgreementV1Options {
    readonly config: IConfig;
}
