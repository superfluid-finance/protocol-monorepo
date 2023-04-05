import {
    IConstantFlowAgreementV1,
    IInstantDistributionAgreementV1,
    IResolver,
    Superfluid,
    SuperfluidGovernanceII,
} from "@superfluid-finance/ethereum-contracts/build/typechain";
import { ethers, Overrides } from "ethers";

// TODO (0xdavinchee): reorganize this
// Maybe moving these into categorical files
// makes more sense than stuffing them all here

export type ProviderOrSigner = ethers.providers.Provider | ethers.Signer;

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

export interface IShouldUseCallAgreement {
    readonly shouldUseCallAgreement?: boolean;
}

interface EthersParams {
    readonly overrides?: Overrides & { from?: string | Promise<string> };
}

// write request interfaces
export interface ISuperTokenModifyFlowParams
    extends IShouldUseCallAgreement,
        EthersParams {
    readonly flowRate?: string;
    readonly receiver: string;
    readonly sender?: string;
    readonly userData?: string;
}
export interface ISuperTokenCreateFlowParams
    extends ISuperTokenModifyFlowParams {
    readonly flowRate: string;
}
export type ISuperTokenUpdateFlowParams = ISuperTokenCreateFlowParams;
export interface ISuperTokenDeleteFlowParams
    extends ISuperTokenModifyFlowParams {
    readonly sender: string;
}

export interface ISuperTokenCreateFlowByOperatorParams
    extends ISuperTokenCreateFlowParams {
    readonly sender: string;
}
export type ISuperTokenUpdateFlowByOperatorParams =
    ISuperTokenCreateFlowByOperatorParams;

export interface ISuperTokenBaseIDAParams extends EthersParams {
    readonly indexId: string;
    readonly userData?: string;
}
export interface ISuperTokenGetSubscriptionParams {
    readonly indexId: string;
    readonly publisher: string;
    readonly subscriber: string;
    readonly providerOrSigner: ProviderOrSigner;
}
export interface ISuperTokenGetIndexParams {
    readonly indexId: string;
    readonly publisher: string;
    readonly providerOrSigner: ProviderOrSigner;
}
export interface ISuperTokenPublisherParams extends ISuperTokenBaseIDAParams {
    readonly publisher: string;
    readonly providerOrSigner: ProviderOrSigner;
}
export interface ISuperTokenPubSubParams extends EthersParams {
    readonly indexId: string;
    readonly publisher: string;
    readonly subscriber: string;
    readonly userData?: string;
}
export interface ISuperTokenPublisherOperationParams extends EthersParams {
    readonly indexId: string;
    readonly publisher: string;
    readonly userData?: string;
}
export interface ISuperTokenDistributeParams extends EthersParams {
    readonly indexId: string;
    readonly amount: string;
    readonly userData?: string;
}
export interface ISuperTokenUpdateIndexValueParams extends EthersParams {
    readonly indexId: string;
    readonly indexValue: string;
    readonly userData?: string;
}
export interface ISuperTokenUpdateSubscriptionUnitsParams extends EthersParams {
    readonly indexId: string;
    readonly subscriber: string;
    readonly units: string;
    readonly userData?: string;
}

export interface IModifyFlowParams
    extends IShouldUseCallAgreement,
        EthersParams {
    readonly flowRate?: string;
    readonly receiver: string;
    readonly sender?: string;
    readonly userData?: string;
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

export interface ISuperTokenUpdateFlowOperatorPermissionsParams
    extends EthersParams {
    readonly flowOperator: string;
    readonly permissions: number;
    readonly flowRateAllowance: string;
    readonly shouldUseCallAgreement?: boolean;
    readonly userData?: string;
}

export interface ISuperTokenFullControlParams extends EthersParams {
    readonly flowOperator: string;
    readonly shouldUseCallAgreement?: boolean;
    readonly userData?: string;
}

export interface IUpdateFlowOperatorPermissionsParams
    extends ISuperTokenUpdateFlowOperatorPermissionsParams,
        IShouldUseCallAgreement {
    readonly superToken: string;
}

export interface IFullControlParams
    extends ISuperTokenFullControlParams,
        IShouldUseCallAgreement {
    readonly superToken: string;
}

export interface IRealtimeBalanceOfParams {
    readonly providerOrSigner: ProviderOrSigner;
    readonly account: string;
    readonly timestamp?: number;
}

export interface IBaseSuperTokenParams extends EthersParams {
    readonly receiver: string;
    readonly amount: string;
}

export interface ITransferFromParams extends EthersParams {
    readonly sender: string;
    readonly receiver: string;
    readonly amount: string;
}

export interface ERC777SendParams extends EthersParams {
    readonly recipient: string;
    readonly amount: string;
    readonly userData?: string;
}

export interface ISuperTokenGetFlowParams {
    readonly sender: string;
    readonly receiver: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface ISuperTokenGetFlowInfoParams {
    readonly account: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface IGetFlowParams {
    readonly superToken: string;
    readonly sender: string;
    readonly receiver: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface IGetAccountFlowInfoParams {
    readonly superToken: string;
    readonly account: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface IGetFlowOperatorDataParams {
    readonly superToken: string;
    readonly sender: string;
    readonly flowOperator: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface IGetFlowOperatorDataByIDParams {
    readonly superToken: string;
    readonly flowOperatorId: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface IGetGovernanceParametersParams {
    providerOrSigner: ProviderOrSigner;
    token?: string;
}
export interface ISuperTokenFlowOperatorDataParams {
    readonly sender: string;
    readonly flowOperator: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface ISuperTokenFlowOperatorDataByIDParams {
    readonly flowOperatorId: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface IBaseIDAParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly userData?: string;
    readonly publisher?: string;
}

export interface ICreateIndexParams extends EthersParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly userData?: string;
}
export interface IBaseSubscriptionParams extends IBaseIDAParams {
    readonly subscriber: string;
}

export interface IGetSubscriptionParams extends IBaseIDAParams {
    readonly publisher: string;
    readonly subscriber: string;
    readonly providerOrSigner: ProviderOrSigner;
}
export interface IGetIndexParams extends IBaseIDAParams {
    readonly publisher: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface IDistributeParams extends EthersParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly amount: string;
    readonly userData?: string;
}

export interface IUpdateIndexValueParams extends EthersParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly indexValue: string;
    readonly userData?: string;
}

export interface IUpdateSubscriptionUnitsParams extends EthersParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly subscriber: string;
    readonly units: string;
    readonly userData?: string;
}

export interface IApproveSubscriptionParams extends EthersParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher: string;
    readonly userData?: string;
}

export interface IRevokeSubscriptionParams extends EthersParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher: string;
    readonly userData?: string;
}

export interface IDeleteSubscriptionParams extends EthersParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher: string;
    readonly subscriber: string;
    readonly userData?: string;
}

export interface IClaimParams extends EthersParams {
    readonly indexId: string;
    readonly superToken: string;
    readonly publisher: string;
    readonly subscriber: string;
    readonly userData?: string;
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
    readonly token: SuperTokenType;
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
    readonly token: SuperTokenType;
}

export interface IStream extends IHOLUpdateable {
    readonly currentFlowRate: string;
    readonly streamedUntilUpdatedAt: string;
    readonly token: SuperTokenType;
    readonly sender: string;
    readonly receiver: string;
    readonly flowUpdatedEvents: IStreamFlowUpdatedEvent[];
}
export type IStreamFlowUpdatedEvent = IFlowUpdatedEvent;

export interface SuperTokenType extends IHOLEntityBase {
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
    readonly token: SuperTokenType;
}

// Internal Interfaces

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
    readonly cfaV1ForwarderAddress: string;
}

export interface IContracts {
    readonly cfaV1: IConstantFlowAgreementV1;
    readonly governance: SuperfluidGovernanceII;
    readonly host: Superfluid;
    readonly idaV1: IInstantDistributionAgreementV1;
    readonly resolver: IResolver;
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

export interface ERC20BalanceOfParams {
    readonly account: string;
    readonly providerOrSigner: ProviderOrSigner;
}
export interface ERC20AllowanceParams {
    readonly owner: string;
    readonly spender: string;
    readonly providerOrSigner: ProviderOrSigner;
}
export interface ERC20BalanceOfParams {
    readonly account: string;
    readonly providerOrSigner: ProviderOrSigner;
}

// ERC721

export interface NFTFlowData {
    readonly flowSender: string;
    readonly flowStartDate: Date;
    readonly flowReceiver: string;
}

export interface ERC721TransferFromParams extends EthersParams {
    readonly from: string;
    readonly to: string;
    readonly tokenId: string;
}

export interface ERC721SafeTransferFromParams extends ERC721TransferFromParams {
    readonly data: string;
}

export interface ERC721ApproveParams extends EthersParams {
    readonly approved: string;
    readonly tokenId: string;
}

export interface ERC721SetApprovalForAllParams extends EthersParams {
    readonly operator: string;
    readonly approved: boolean;
}

export interface ERC721BalanceOfParams {
    readonly owner: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export interface ERC721TokenIdQueryParams {
    readonly tokenId: string;
    readonly providerOrSigner: ProviderOrSigner;
}
export interface ERC721IsApprovedForAllParams {
    readonly owner: string;
    readonly operator: string;
    readonly providerOrSigner: ProviderOrSigner;
}

export type ERC721OwnerOfParams = ERC721TokenIdQueryParams;
export type ERC721GetApprovedParams = ERC721TokenIdQueryParams;
export type ERC721TokenURIParams = ERC721TokenIdQueryParams;
export interface ERC20IncreaseAllowanceParams extends EthersParams {
    readonly spender: string;
    readonly amount: string;
}

export type ERC20DecreaseAllowanceParams = ERC20IncreaseAllowanceParams;

export interface SuperTokenFlowRateAllowanceParams extends EthersParams {
    readonly flowOperator: string;
    readonly flowRateAllowanceDelta: string;
    readonly userData?: string;
}
export interface FlowRateAllowanceParams
    extends SuperTokenFlowRateAllowanceParams {
    readonly superToken: string;
}
