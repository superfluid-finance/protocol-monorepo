/**************************************************************************
 * GraphQL Entity Types
 *************************************************************************/
import { BaseProvider } from "@ethersproject/providers";
import { BigNumber } from "@ethersproject/bignumber";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import { ConstantFlowAgreementV1 } from "../typechain/ConstantFlowAgreementV1";
import { SuperToken } from "../typechain/SuperToken";
import { FlowActionType } from "./helpers/constants";

/**
 * Event Entities
 */
export interface IEvent {
    readonly id: string;
    readonly transactionHash: string;
    readonly blockNumber: string;
    readonly timestamp: string;
}

export interface IMeta {
    readonly _meta: {
        readonly block: {
            readonly number: number;
        };
    };
}

// CFAV1
export interface IFlowUpdated extends IEvent {
    readonly token: string;
    readonly sender: string;
    readonly receiver: string;
    readonly flowRate: string;
    readonly totalAmountStreamedUntilTimestamp: string;
    readonly totalReceiverFlowRate: string;
    readonly totalSenderFlowRate: string;
    readonly oldFlowRate: string;
    readonly type: string;
}

// IDAV1
export interface IIndexCreated extends IEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface IIndexUpdated extends IEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly oldIndexValue: string;
    readonly newIndexValue: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly userData: string;
}

export interface ISubscriptionApproved extends IEvent {
    readonly token: string;
    readonly subscriber: ILightEntity;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface ISubscriptionRevoked extends IEvent {
    readonly token: string;
    readonly subscriber: ILightEntity;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface ISubscriptionUnitsUpdated extends IEvent {
    readonly token: string;
    readonly subscriber: ILightEntity;
    readonly publisher: string;
    readonly indexId: string;
    readonly units: string;
    readonly userData: string;
}

// SuperToken
export interface ITokenUpgraded extends IEvent {
    readonly id: string;
    readonly account: string;
    readonly transactionHash: string;
    readonly timestamp: string;
    readonly blockNumber: string;
    readonly token: string;
    readonly amount: string;
}

export interface ITokenDowngraded extends IEvent {
    readonly id: string;
    readonly account: string;
    readonly transactionHash: string;
    readonly timestamp: string;
    readonly blockNumber: string;
    readonly token: string;
    readonly amount: string;
}

export interface ITransfer extends IEvent {
    readonly id: string;
    readonly transactionHash: string;
    readonly timestamp: string;
    readonly blockNumber: string;
    readonly from: string;
    readonly to: string;
    readonly value: string;
    readonly token: string;
}

/**
 * HOL Entities
 */
interface IBaseEntity {
    readonly id: string;
    readonly createdAt: string;
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlock: string;
}

export interface IAccount extends IBaseEntity {
    readonly isSuperApp: boolean;
}

export interface IToken extends IBaseEntity {
    readonly name: string;
    readonly symbol: string;
    readonly underlyingAddress: string;
    readonly isListed: boolean;
}

export interface IStream extends IBaseEntity {
    readonly currentFlowRate: string;
    readonly streamedUntilUpdatedAt: string;
    readonly token: ILightEntity;
    readonly sender: ILightEntity;
    readonly receiver: ILightEntity;
}

export interface ISubscriber extends IBaseEntity {
    readonly token: ILightEntity;
    readonly subscriber: ILightEntity;
    readonly publisher: ILightEntity;
    readonly indexId: string;
    readonly approved: boolean;
    readonly units: string;
    readonly totalAmountReceivedUntilUpdatedAt: string;
    readonly lastIndexValue: string;
    readonly index: ILightEntity;
}

export interface IIndex extends IBaseEntity {
    readonly indexId: string;
    readonly oldIndexValue: string;
    readonly newIndexValue: string;
    readonly totalSubscribers: number;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly totalUnits: string;
    readonly totalAmountDistributedUntilUpdatedAt: string;
    readonly token: ILightEntity;
    readonly publisher: ILightEntity;
}

/**
 * Aggregate Entities
 */

export interface IBaseAggregateEntity {
    readonly id: string;
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlock: string;
}

export interface IAccountTokenSnapshot extends IBaseAggregateEntity {
    readonly totalNumberOfActiveStreams: number;
    readonly totalNumberOfClosedStreams: number;
    readonly totalSubscriptions: number;
    readonly totalApprovedSubscriptions: number;
    readonly balanceUntilUpdatedAt: string;
    readonly totalNetFlowRate: string;
    readonly totalInflowRate: string;
    readonly totalOutflowRate: string;
    readonly totalAmountStreamedUntilUpdatedAt: string;
    readonly totalAmountTransferredUntilUpdatedAt: string;
    readonly account: ILightEntity;
    readonly token: ILightEntity;
}

export interface ITokenStatistic extends IBaseAggregateEntity {
    readonly totalNumberOfActiveStreams: number;
    readonly totalNumberOfClosedStreams: number;
    readonly totalNumberOfIndexes: number;
    readonly totalNumberOfActiveIndexes: number;
    readonly totalSubscriptions: number;
    readonly totalApprovedSubscriptions: number;
    readonly totalOutflowRate: string;
    readonly totalAmountStreamedUntilUpdatedAt: string;
    readonly totalAmountTransferredUntilUpdatedAt: string;
    readonly totalAmountDistributedUntilUpdatedAt: string;
    readonly totalSupply: string;
    readonly token: ILightEntity;
}

/** Sub-entity Entities */
export interface ILightEntity {
    readonly id: string;
}

/**************************************************************************
 * Internal Interfaces
 *************************************************************************/
export interface IStreamData {
    id: string;
    revisionIndex: string;
    oldFlowRate: string;
    streamedUntilUpdatedAt: string;
    lastUpdatedAtTimestamp: string;
}

export interface IStreamTestParams {
    readonly actionType: FlowActionType;
    readonly flowRate: number;
    readonly streamHistory: IStreamData;
}

export interface IExpectedATSData {
    readonly totalNumberOfActiveStreams: number;
    readonly totalNumberOfClosedStreams: number;
    readonly balanceUntilUpdatedAt: string;
    readonly totalInflowRate: string;
    readonly totalOutflowRate: string;
    readonly totalNetFlowRate: string;
    readonly totalAmountStreamedUntilUpdatedAt: string;
}

export interface IExpectedTokenStats {
    readonly totalNumberOfActiveStreams: number;
    readonly totalNumberOfClosedStreams: number;
    readonly totalOutflowRate: string;
    readonly totalAmountStreamedUntilUpdatedAt: string;
}

export interface IUpdateGlobalObjectData {
    readonly updatedStreamData?: IStreamData;
    readonly updatedTokenStats?: ITokenStatistic;
    readonly updatedReceiverATS?: IAccountTokenSnapshot;
    readonly updatedSenderATS?: IAccountTokenSnapshot;
    readonly revisionIndexId?: string;
    readonly updatedIndex?: IIndex;
    readonly updatedSubscriber?: ISubscriber;
}

export interface IAggregateLocalData {
    readonly accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    };
    readonly tokenStatistics: { [id: string]: ITokenStatistic | undefined };
}

export interface IBaseTestData {
    readonly lastUpdatedAtTimestamp: string;
    readonly lastUpdatedBlockNumber: string;
    readonly token: string;
}

export interface IStreamLocalData extends IAggregateLocalData {
    readonly revisionIndexes: { [id: string]: number | undefined };
    readonly streamData: { [id: string]: IStreamData | undefined };
}

export interface IContracts {
    readonly sf: Framework;
    readonly cfaV1: ConstantFlowAgreementV1;
    readonly superToken: SuperToken;
}

export interface IFlowUpdatedInitTestData
    extends IStreamLocalData,
        IBaseTestData {
    readonly sender: string;
    readonly receiver: string;
}

export interface IFlowUpdatedUpdateTestData {
    readonly actionType: FlowActionType;
    readonly lastUpdatedBlockNumber: string;
    readonly lastUpdatedAtTimestamp: string;
    readonly accountTokenSnapshots: IAccountTokenSnapshot[];
    readonly flowRate: BigNumber;
    readonly superToken: SuperToken;
    readonly pastStreamData: IStreamData;
    readonly currentSenderATS: IAccountTokenSnapshot;
    readonly currentReceiverATS: IAccountTokenSnapshot;
    readonly currentTokenStats: ITokenStatistic;
}

export interface ITestModifyFlowData {
	readonly contracts: IContracts,
    readonly localData: IStreamLocalData,
    readonly provider: BaseProvider,
    readonly actionType: FlowActionType,
    readonly atsArray: IAccountTokenSnapshot[],
    readonly newFlowRate: number,
    readonly sender: string,
    readonly receiver: string,
    readonly tokenAddress: string
}

export interface IInstantDistributionLocalData extends IAggregateLocalData {
    readonly indexes: { [id: string]: IIndex | undefined };
    readonly subscribers: { [id: string]: ISubscriber | undefined };
}

export interface IInstantDistributionTestData
    extends IInstantDistributionLocalData,
        IBaseTestData {
    readonly publisher: string;
    readonly subscriber?: string;
    readonly indexId: string;
}

export interface IUpdateIndexData {
    readonly oldIndexValue?: string;
    readonly newIndexValue?: string;
    readonly totalSubscribersDelta?: number;
    readonly totalUnitsPending?: BigNumber;
    readonly totalUnitsApproved?: BigNumber;
}

export interface IUpdateSubscriberData {
    readonly approved?: boolean;
    readonly units?: string;
    readonly totalAmountReceivedUntilUpdatedAt?: string;
    readonly lastIndexValue?: string;
}

export interface IBaseIDAEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
}

export interface IExpectedIndexUpdated extends IBaseIDAEvent {
    readonly oldIndexValue: string;
    readonly newIndexValue: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
}

export interface IExpectedSubscriberEvent extends IBaseIDAEvent {
    readonly subscriber: ILightEntity;
}

export interface IExpectedSubscriptionUnitsUpdated
    extends IExpectedSubscriberEvent {
    readonly units: string;
}

export interface IExpectedFlowUpdateEvent {
    readonly flowRate: string;
    readonly oldFlowRate: string;
    readonly receiver: string;
    readonly sender: string;
    readonly token: string;
    readonly totalAmountStreamedUntilTimestamp: string;
    readonly totalReceiverFlowRate: string;
    readonly totalSenderFlowRate: string;
    readonly type: FlowActionType;
}

export interface IGetExpectedIDADataBase {
    readonly token: SuperToken;
    readonly atsArray: IAccountTokenSnapshot[];
    readonly currentIndex: IIndex;
    readonly currentPublisherATS: IAccountTokenSnapshot;
    readonly currentTokenStats: ITokenStatistic;
    readonly updatedAtBlock: string;
    readonly timestamp: string;
}

export interface IGetExpectedSubscriberIDAData extends IGetExpectedIDADataBase {
    readonly currentSubscriber: ISubscriber;
    readonly currentSubscriberATS: IAccountTokenSnapshot;
}
