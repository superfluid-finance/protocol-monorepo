/**************************************************************************
 * GraphQL Entity Types
 *************************************************************************/
import { BaseProvider } from "@ethersproject/providers";
import { BigNumber } from "@ethersproject/bignumber";
import { Framework, SuperToken } from "@superfluid-finance/sdk-core";
import { FlowActionType, IDAEventType } from "./helpers/constants";

/**
 * Event Entities
 */
export interface IEvent {
    readonly id: string;
    readonly transactionHash: string;
    readonly blockNumber: string;
    readonly name: string;
    readonly addresses: string[];
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
export interface IFlowUpdatedEvent extends IEvent {
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
export interface IIndexCreatedEvent extends IEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface IIndexUpdatedEvent extends IEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly oldIndexValue: string;
    readonly newIndexValue: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly userData: string;
}

export interface IIndexSubscribedEvent extends IEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly subscriber: string;
    readonly userData: string;
}

export interface IIndexUnitsUpdatedEvent extends IEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly subscriber: string;
    readonly units: string;
    readonly userData: string;
}

export interface IIndexUnsubscribedEvent extends IEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly subscriber: string;
    readonly userData: string;
}

export interface ISubscriptionApprovedEvent extends IEvent {
    readonly token: string;
    readonly subscriber: string;
    readonly subscription: ILightEntity;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface ISubscriptionRevokedEvent extends IEvent {
    readonly token: string;
    readonly subscriber: string;
    readonly subscription: ILightEntity;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface ISubscriptionUnitsUpdatedEvent extends IEvent {
    readonly token: string;
    readonly subscriber: string;
    readonly subscription: ILightEntity;
    readonly publisher: string;
    readonly indexId: string;
    readonly units: string;
    readonly userData: string;
}

// SuperToken
export interface ITokenUpgradedEvent extends IEvent {
    readonly id: string;
    readonly account: string;
    readonly transactionHash: string;
    readonly timestamp: string;
    readonly blockNumber: string;
    readonly token: string;
    readonly amount: string;
}

export interface ITokenDowngradedEvent extends IEvent {
    readonly id: string;
    readonly account: string;
    readonly transactionHash: string;
    readonly timestamp: string;
    readonly blockNumber: string;
    readonly token: string;
    readonly amount: string;
}

export interface ITransferEvent extends IEvent {
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
    readonly createdAtTimestamp: string;
    readonly createdAtBlockNumber: string;
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlockNumber: string;
}

export interface IAccount extends IBaseEntity {
    readonly isSuperApp: boolean;
    readonly inflows: ILightEntity[];
    readonly outflows: ILightEntity[];
    readonly subscriptions: ILightEntity[];
    readonly publishedIndexes: ILightEntity[];
}

export interface IToken extends IBaseEntity {
    readonly decimals: number;
    readonly name: string;
    readonly symbol: string;
    readonly underlyingAddress: string;
    readonly isListed: boolean;
    readonly underlyingToken: ILightEntity | null;
}

export interface IStream extends IBaseEntity {
    readonly currentFlowRate: string;
    readonly streamedUntilUpdatedAt: string;
    readonly token: ILightEntity;
    readonly sender: ILightEntity;
    readonly receiver: ILightEntity;
    readonly flowUpdatedEvents: ILightEntity[];
    readonly streamPeriods: ILightEntity[];
}

export interface IStreamPeriod extends IBaseEntity {
    readonly id: string;
    readonly stream: ILightEntity;
    readonly sender: ILightEntity;
    readonly receiver: ILightEntity;
    readonly token: ILightEntity;
    readonly flowRate: string;
    readonly startedAtTimestamp: string;
    readonly startedAtBlockNumber: string;
    readonly startedAtEvent: ILightEntity;
    readonly stoppedAtTimestamp: string;
    readonly stoppedAtBlockNumber: string;
    readonly stoppedAtEvent: ILightEntity;
    readonly totalAmountStreamed: string;
}

export interface IIndexSubscription extends IBaseEntity {
    readonly subscriber: ILightEntity;
    readonly approved: boolean;
    readonly units: string;
    readonly totalAmountReceivedUntilUpdatedAt: string;
    readonly indexValueUntilUpdatedAt: string;
    readonly index: ILightIndex;

    readonly subscriptionApprovedEvents?: ILightEntity[];
    readonly subscriptionDistributionClaimedEvents?: ILightEntity[];
    readonly subscriptionRevokedEvents?: ILightEntity[];
    readonly subscriptionUnitsUpdatedEvents?: ILightEntity[];
}

interface ILightIndex extends ILightEntity {
    readonly indexId: string;
    readonly token: ILightEntity;
    readonly publisher: ILightEntity;
}

export interface IIndex extends IBaseEntity {
    readonly indexId: string;
    readonly indexValue: string;
    readonly totalSubscriptionsWithUnits: number;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly totalUnits: string;
    readonly totalAmountDistributedUntilUpdatedAt: string;
    readonly token: ILightEntity;
    readonly publisher: ILightEntity;

    readonly subscriptions?: ILightEntity[];
    readonly indexCreatedEvent?: ILightEntity;
    readonly indexDistributionClaimedEvents?: ILightEntity[];
    readonly indexUpdatedEvents?: ILightEntity[];
    readonly indexSubscribedEvents?: ILightEntity[];
    readonly indexUnitsUpdatedEvents?: ILightEntity[];
    readonly indexUnsubscribedEvents?: ILightEntity[];
}

/**
 * Aggregate Entities
 */

export interface IBaseAggregateEntity {
    readonly id: string;
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlockNumber: string;
}

export interface IAccountTokenSnapshot extends IBaseAggregateEntity {
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
    readonly token: ILightEntity;
}

export interface ITokenStatistic extends IBaseAggregateEntity {
    readonly totalNumberOfActiveStreams: number;
    readonly totalNumberOfClosedStreams: number;
    readonly totalNumberOfIndexes: number;
    readonly totalNumberOfActiveIndexes: number;
    readonly totalSubscriptionsWithUnits: number;
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
    periodRevisionIndex: string;
    oldFlowRate: string;
    streamedUntilUpdatedAt: string;
    updatedAtTimestamp: string;
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

export interface IUpdateGlobalObjects {
    readonly revisionIndexId?: string;
    readonly updatedStreamData?: IStreamData;
    readonly updatedSenderATS?: IAccountTokenSnapshot;
    readonly updatedReceiverATS?: IAccountTokenSnapshot;
    readonly updatedPublisherATS?: IAccountTokenSnapshot;
    readonly updatedSubscriberATS?: IAccountTokenSnapshot;
    readonly updatedIndex?: IIndex;
    readonly updatedSubscription?: IIndexSubscription;
    readonly updatedTokenStats: ITokenStatistic;
}

export interface IAggregateLocalData {
    readonly accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    };
    readonly tokenStatistics: { [id: string]: ITokenStatistic | undefined };
}

export interface IBaseTestData {
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlockNumber: string;
    readonly token: string;
}

export interface IStreamLocalData extends IAggregateLocalData {
    readonly revisionIndexes: { [id: string]: number | undefined };
    readonly periodRevisionIndexes: { [id: string]: number | undefined };
    readonly streamData: { [id: string]: IStreamData | undefined };
}
export interface IDistributionLocalData extends IAggregateLocalData {
    readonly indexes: { [id: string]: IIndex | undefined };
    readonly subscriptions: { [id: string]: IIndexSubscription | undefined };
}

export interface IFlowUpdatedInitTestData
    extends IStreamLocalData,
        IBaseTestData {
    readonly sender: string;
    readonly receiver: string;
    readonly totalSupply?: string;
}

export interface IBaseDistributionTesterParams {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: number;
    readonly userData: string;
}

export interface ISubscriberDistributionTesterParams
    extends IBaseDistributionTesterParams {
    readonly subscriber: string;
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
    readonly provider: BaseProvider;
}

export interface ITestModifyFlowData {
    readonly framework: Framework;
    readonly superToken: SuperToken;
    readonly localData: IStreamLocalData;
    readonly provider: BaseProvider;
    readonly actionType: FlowActionType;
    readonly atsArray: IAccountTokenSnapshot[];
    readonly newFlowRate: number;
    readonly sender: string;
    readonly receiver: string;
    readonly tokenAddress: string;
    readonly totalSupply?: string;
}

export interface ITestModifyIDAData {
    readonly framework: Framework;
    readonly superToken: SuperToken;
    readonly provider: BaseProvider;
    readonly atsArray: IAccountTokenSnapshot[];
    readonly localData: IDistributionLocalData;
    readonly baseParams: ISubscriberDistributionTesterParams;
    readonly eventType: IDAEventType;
    readonly units?: BigNumber;
    readonly isRevoke?: boolean;
    readonly sender?: string;
    readonly isDistribute?: boolean;
    readonly amountOrIndexValue?: BigNumber;
}

export interface IEventQueryData {
    readonly query: string;
    readonly queryName: string;
}

export interface IExtraEventData {
    readonly units?: BigNumber;
    readonly oldIndexValue?: string;
    readonly newIndexValue?: BigNumber;
    readonly totalUnitsApproved?: BigNumber;
    readonly totalUnitsPending?: BigNumber;
    readonly distributionDelta?: BigNumber;
}

export interface IExtraExpectedData extends IExtraEventData {
    readonly isRevoke?: boolean;
    readonly totalUnits?: BigNumber;
}

export interface IInstantDistributionLocalData extends IAggregateLocalData {
    readonly indexes: { [id: string]: IIndex | undefined };
    readonly subscriptions: { [id: string]: IIndexSubscription | undefined };
}

export interface IInstantDistributionTestData
    extends IInstantDistributionLocalData,
        IBaseTestData {
    readonly publisher: string;
    readonly subscriber?: string;
    readonly indexId: string;
}

export interface IIDAEvents {
    readonly IndexCreatedEvent?: IEvent;
    readonly IndexDistributionClaimedEvent?: IEvent;
    readonly IndexUpdatedEvent?: IEvent;
    readonly IndexSubscribedEvent?: IEvent;
    readonly IndexUnitsUpdatedEvent?: IEvent;
    readonly IndexUnsubscribedEvent?: IEvent;
    readonly SubscriptionApprovedEvent?: IEvent;
    readonly SubscriptionDistributionClaimedEvent?: IEvent;
    readonly SubscriptionRevokedEvent?: IEvent;
    readonly SubscriptionUnitsUpdatedEvent?: IEvent;
}

export interface IBaseIDAEvent {
    readonly token: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface IExpectedIndexUpdatedEvent extends IBaseIDAEvent {
    readonly oldIndexValue: string;
    readonly newIndexValue: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
}

export interface IExpectedSubscriberEvent extends IBaseIDAEvent {
    readonly subscriber: ILightEntity;
}

export interface IExpectedSubscriptionUnitsUpdatedEvent
    extends IExpectedSubscriberEvent {
    readonly units: string;
}

export interface IExpectedFlowUpdateEvent {
    readonly flowRate: string;
    readonly oldFlowRate: string;
    readonly receiver: string;
    readonly sender: string;
    readonly addresses: string[];
    readonly token: string;
    readonly totalAmountStreamedUntilTimestamp: string;
    readonly totalReceiverFlowRate: string;
    readonly totalSenderFlowRate: string;
    readonly type: FlowActionType;
}

export interface IGetExpectedIDADataParams {
    readonly token: SuperToken;
    readonly atsArray: IAccountTokenSnapshot[];
    readonly currentIndex: IIndex;
    readonly currentPublisherATS: IAccountTokenSnapshot;
    readonly currentSubscription: IIndexSubscription;
    readonly currentSubscriberATS: IAccountTokenSnapshot;
    readonly currentTokenStats: ITokenStatistic;
    readonly updatedAtBlockNumber: string;
    readonly timestamp: string;
    readonly provider: BaseProvider;
}
