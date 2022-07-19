import { ethers, Overrides } from "ethers";

import {
    IConstantFlowAgreementV1,
    IInstantDistributionAgreementV1,
    IResolver,
    Superfluid,
} from "./typechain";
import { SuperfluidGovernanceII } from "./typechain/SuperfluidGovernanceII";
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
    readonly timestamp_gte: number;
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
export interface ISuperTokenRequestFilter {
    readonly isListed?: boolean;
}

// TODO: Major Cleanup needed for SuperToken interfaces
// along with IDA/CFA interfaces
// A better thought out inheritance pattern - SuperToken is parent
// CFA/IDA inherits and tacks on superToken property

// write request interfaces
export interface ISuperTokenModifyFlowParams {
    readonly flowRate?: string;
    readonly receiver: string;
    readonly sender?: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}
export interface ISuperTokenCreateFlowParams
    extends ISuperTokenModifyFlowParams {
    readonly flowRate: string;
}

export interface ISuperTokenCreateFlowByOperatorParams
    extends ISuperTokenCreateFlowParams {
    readonly sender: string;
}

export type ISuperTokenUpdateFlowParams = ISuperTokenCreateFlowParams;
export type ISuperTokenUpdateFlowByOperatorParams =
    ISuperTokenCreateFlowByOperatorParams;
export interface ISuperTokenDeleteFlowParams
    extends ISuperTokenModifyFlowParams {
    readonly sender: string;
}

export interface ISuperTokenBaseIDAParams {
    readonly indexId: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}
export interface ISuperTokenGetSubscriptionParams {
    readonly indexId: string;
    readonly publisher: string;
    readonly subscriber: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}
export interface ISuperTokenGetIndexParams {
    readonly indexId: string;
    readonly publisher: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}
export interface ISuperTokenPublisherParams extends ISuperTokenBaseIDAParams {
    readonly publisher: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}
export interface ISuperTokenPubSubParams {
    readonly indexId: string;
    readonly publisher: string;
    readonly subscriber: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}
export interface ISuperTokenPublisherOperationParams {
    readonly indexId: string;
    readonly publisher: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}
export interface ISuperTokenDistributeParams {
    readonly indexId: string;
    readonly amount: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}
export interface ISuperTokenUpdateIndexValueParams {
    readonly indexId: string;
    readonly indexValue: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}
export interface ISuperTokenUpdateSubscriptionUnitsParams {
    readonly indexId: string;
    readonly subscriber: string;
    readonly units: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface IModifyFlowParams extends ISuperTokenModifyFlowParams {
    readonly superToken: string;
}
export interface ICreateFlowParams extends IModifyFlowParams {
    readonly flowRate: string;
}
export interface ICreateFlowByOperatorParams extends ICreateFlowParams {
    readonly sender: string;
}

export type IUpdateFlowParams = ICreateFlowParams;
export type IUpdateFlowByOperatorParams = ICreateFlowByOperatorParams;

export interface IDeleteFlowParams extends IModifyFlowParams {
    readonly sender: string;
}

export interface ISuperTokenUpdateFlowOperatorPermissionsParams {
    readonly flowOperator: string;
    readonly permissions: number;
    readonly flowRateAllowance: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface ISuperTokenFullControlParams {
    readonly flowOperator: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface IUpdateFlowOperatorPermissionsParams
    extends ISuperTokenUpdateFlowOperatorPermissionsParams {
    readonly superToken: string;
}

export interface IFullControlParams extends ISuperTokenFullControlParams {
    readonly superToken: string;
}

export interface IRealtimeBalanceOfParams {
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
    readonly account: string;
    readonly timestamp?: number;
}

export interface IBaseSuperTokenParams {
    readonly receiver: string;
    readonly amount: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface ITransferFromParams {
    readonly sender: string;
    readonly receiver: string;
    readonly amount: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface ISuperTokenGetFlowParams {
    readonly sender: string;
    readonly receiver: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface ISuperTokenGetFlowInfoParams {
    readonly account: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface IGetFlowParams {
    readonly superToken: string;
    readonly sender: string;
    readonly receiver: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface IGetAccountFlowInfoParams {
    readonly superToken: string;
    readonly account: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface IGetFlowOperatorDataParams {
    readonly superToken: string;
    readonly sender: string;
    readonly flowOperator: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface IGetFlowOperatorDataByIDParams {
    readonly superToken: string;
    readonly flowOperatorId: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface IGetGovernanceParametersParams {
    providerOrSigner: ethers.providers.Provider | ethers.Signer;
    token?: string;
}
export interface ISuperTokenFlowOperatorDataParams {
    readonly sender: string;
    readonly flowOperator: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface ISuperTokenFlowOperatorDataByIDParams {
    readonly flowOperatorId: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface IBaseIDAParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly userData?: string;
    readonly publisher?: string;
}

export interface ICreateIndexParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}
export interface IBaseSubscriptionParams extends IBaseIDAParams {
    readonly subscriber: string;
}

export interface IGetSubscriptionParams extends IBaseIDAParams {
    readonly publisher: string;
    readonly subscriber: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}
export interface IGetIndexParams extends IBaseIDAParams {
    readonly publisher: string;
    readonly providerOrSigner: ethers.providers.Provider | ethers.Signer;
}

export interface IDistributeParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly amount: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface IUpdateIndexValueParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly indexValue: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface IUpdateSubscriptionUnitsParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly subscriber: string;
    readonly units: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface IApproveSubscriptionParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface IRevokeSubscriptionParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface IDeleteSubscriptionParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher: string;
    readonly subscriber: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

export interface IClaimParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher: string;
    readonly subscriber: string;
    readonly userData?: string;
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

// Subgraph Return Data

export interface ILightEntity {
    readonly id: string;
}

export interface IEventEntityBase extends ILightEntity {
    readonly blockNumber: number;
    readonly timestamp: number;
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

export interface IHOLEntityBase extends ILightEntity {
    readonly createdAtTimestamp: number;
    readonly createdAtBlockNumber: number;
}

export interface IHOLUpdateable extends IHOLEntityBase {
    readonly updatedAtTimestamp: number;
    readonly updatedAtBlockNumber: number;
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
    readonly publisher: string;
}

export interface IIndexSubscription extends IHOLUpdateable {
    readonly subscriber: string;
    readonly approved: boolean;
    readonly units: string;
    readonly totalAmountReceivedUntilUpdatedAt: string;
    readonly indexValueUntilUpdatedAt: string;
    readonly index: IIndexSubscriptionIndex;
}

export interface IIndexSubscriptionIndex {
    readonly id: string;
    readonly indexId: string;
    readonly indexValue: string;
    readonly token: ISuperToken;
}

export interface IStream extends IHOLUpdateable {
    readonly currentFlowRate: string;
    readonly streamedUntilUpdatedAt: string;
    readonly token: ISuperToken;
    readonly sender: string;
    readonly receiver: string;
    readonly flowUpdatedEvents: IStreamFlowUpdatedEvent[];
}
export type IStreamFlowUpdatedEvent = IFlowUpdatedEvent;

export interface ISuperToken extends IHOLEntityBase {
    readonly name: string;
    readonly symbol: string;
    readonly isListed: boolean;
    readonly isNativeAssetSuperToken: boolean;
    readonly underlyingAddress: string;
}

export interface IAggregateEntityBase {
    readonly id: string;
    readonly updatedAtTimestamp: number;
    readonly updatedAtBlockNumber: number;
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
    readonly account: string;
    readonly token: ISuperToken;
}

// Internal Interfaces

export interface IResolverData {
    readonly subgraphAPIEndpoint: string;
    readonly networkName: string;
    readonly resolverAddress: string;
    readonly nativeTokenSymbol: string;
}

export interface ISignerConstructorOptions {
    readonly web3Provider?: ethers.providers.Web3Provider; // Web3Provider (client side - metamask, web3modal)
    readonly provider?: ethers.providers.Provider; // Provider
    readonly privateKey?: string; // private key (best to store a test account PK in .env file)
    readonly signer?: ethers.Signer; // ethers.Wallet
}

export interface IConfig {
    readonly resolverAddress: string;
    readonly hostAddress: string;
    readonly cfaV1Address: string;
    readonly idaV1Address: string;
    readonly governanceAddress: string;
}

export interface IContracts {
    readonly cfaV1: IConstantFlowAgreementV1;
    readonly governance: SuperfluidGovernanceII;
    readonly host: Superfluid;
    readonly idaV1: IInstantDistributionAgreementV1;
    readonly resolver: IResolver;
}

export interface IAgreementV1Options {
    readonly config: IConfig;
}

// Web3 Return Data

export interface IWeb3RealTimeBalanceOf {
    readonly availableBalance: string;
    readonly deposit: string;
    readonly owedDeposit: string;
    readonly timestamp: Date;
}

export interface IWeb3Subscription {
    readonly exist: boolean;
    readonly approved: boolean;
    readonly units: string;
    readonly pendingDistribution: string;
}

export interface IWeb3Index {
    readonly exist: boolean;
    readonly indexValue: string;
    readonly totalUnitsApproved: string;
    readonly totalUnitsPending: string;
}

export interface IWeb3FlowInfoParams {
    readonly timestamp: ethers.BigNumber;
    readonly flowRate: ethers.BigNumber;
    readonly deposit: ethers.BigNumber;
    readonly owedDeposit: ethers.BigNumber;
}

export interface IWeb3FlowInfo {
    readonly timestamp: Date;
    readonly flowRate: string;
    readonly deposit: string;
    readonly owedDeposit: string;
}

export interface IWeb3FlowOperatorDataParams {
    readonly flowOperatorId: string;
    readonly permissions: number;
    readonly flowRateAllowance: ethers.BigNumber;
}
export interface IWeb3FlowOperatorData {
    readonly flowOperatorId: string;
    readonly permissions: string;
    readonly flowRateAllowance: string;
}

export interface IWeb3GovernanceParams {
    readonly liquidationPeriod: string;
    readonly patricianPeriod: string;
    readonly rewardAddress: string;
    readonly minimumDeposit: string;
}
