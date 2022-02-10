import _ from "lodash";
import {ILightEntity} from "../test/interfaces";
import {FlowUpdatedEvent} from "../typechain/ConstantFlowAgreementV1";
import {
    IndexCreatedEvent,
    IndexDistributionClaimedEvent,
    IndexSubscribedEvent,
    IndexUnitsUpdatedEvent,
    IndexUnsubscribedEvent,
    IndexUpdatedEvent,
    SubscriptionApprovedEvent,
    SubscriptionDistributionClaimedEvent,
    SubscriptionRevokedEvent,
    SubscriptionUnitsUpdatedEvent,
} from "../typechain/IInstantDistributionAgreementV1";

export interface IBaseEntity {
    readonly id: string;
    readonly createdAtTimestamp: string;
    readonly timestamp?: string;
    readonly updatedAtTimestamp?: string;
}

export interface IBaseIndexEvent {
    readonly publisher: string;
    readonly token: string;
    readonly indexId: string;
}

export interface IDataIntegrityFlowUpdatedEvent extends IBaseEntity {
    readonly token: string;
    readonly transactionHash: string;
    readonly sender: string;
    readonly receiver: string;
    readonly flowRate: string;
    readonly totalSenderFlowRate: string;
    readonly totalReceiverFlowRate: string;
    readonly userData: string;
}

export interface IDataIntegrityIndexCreatedEvent
    extends IBaseEntity,
        IBaseIndexEvent {
    readonly transactionHash: string;
    readonly userData: string;
}

export interface IDataIntegrityIndexDistributionClaimedEvent
    extends IBaseEntity,
        IBaseIndexEvent {
    readonly transactionHash: string;
    readonly subscriber: string;
    readonly amount: string;
}

export interface IDataIntegrityIndexUpdatedEvent
    extends IBaseEntity,
        IBaseIndexEvent {
    readonly transactionHash: string;
    readonly oldIndexValue: string;
    readonly newIndexValue: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly userData: string;
}

export interface IDataIntegrityIndexSubscribedEvent
    extends IBaseEntity,
        IBaseIndexEvent {
    readonly transactionHash: string;
    readonly subscriber: string;
    readonly userData: string;
}

export interface IDataIntegrityIndexUnitsUpdatedEvent
    extends IBaseEntity,
        IBaseIndexEvent {
    readonly transactionHash: string;
    readonly subscriber: string;
    readonly units: string;
    readonly userData: string;
}

export interface IDataIntegrityStream extends IBaseEntity {
    readonly updatedAtTimestamp: string;
    readonly currentFlowRate: string;
    readonly token: ILightEntity;
    readonly sender: ILightEntity;
    readonly receiver: ILightEntity;
}

export interface IDataIntegrityIndex extends IBaseEntity {
    readonly indexId: string;
    readonly indexValue: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly totalUnits: string;
    readonly token: ILightEntity;
    readonly publisher: ILightEntity;
}

export interface IDataIntegritySubscription extends IBaseEntity {
    readonly approved: boolean;
    readonly units: string;
    readonly indexValueUntilUpdatedAt: string;
    readonly subscriber: ILightEntity;
    readonly index: {
        readonly id: string;
        readonly indexId: string;
        readonly indexValue: string;
        readonly token: ILightEntity;
        readonly publisher: ILightEntity;
    };
}

// NOTE: IDataIntegrityAccountTokenSnapshot and
// IDataIntegrityTokenStatistic only have updatedAtTimestamp
// not createdAtTimestamp
export interface IDataIntegrityAccountTokenSnapshot extends IBaseEntity {
    readonly balanceUntilUpdatedAt: string;
    readonly totalNetFlowRate: string;
    readonly token: {id: string; underlyingAddress: string};
    readonly account: ILightEntity;
}

export interface IDataIntegrityTokenStatistic extends IBaseEntity {
    readonly totalSupply: string;
    readonly token: {id: string; underlyingAddress: string};
}

export const enum IDAEvent {
    IndexCreated = "IndexCreated",
    IndexUpdated = "IndexUpdated",
    IndexDistributionClaimed = "IndexDistributionClaimed",
    IndexSubscribed = "IndexSubscribed",
    IndexUnitsUpdated = "IndexUnitsUpdated",
    IndexUnsubscribed = "IndexUnsubscribed",
    SubscriptionApproved = "SubscriptionApproved",
    SubscriptionDistributionClaimed = "SubscriptionDistributionClaimed",
    SubscriptionRevoked = "SubscriptionRevoked",
    SubscriptionUnitsUpdated = "SubscriptionUnitsUpdated",
}

export interface IOnChainCFAEvents {
    ["FlowUpdated"]: {
        events: FlowUpdatedEvent[];
        groupedEvents: _.Dictionary<FlowUpdatedEvent[]>;
    };
}
export interface IOnChainIDAEvents {
    [IDAEvent.IndexCreated]: {
        events: IndexCreatedEvent[];
        groupedEvents: _.Dictionary<IndexCreatedEvent[]>;
    };
    [IDAEvent.IndexUpdated]: {
        events: IndexUpdatedEvent[];
        groupedEvents: _.Dictionary<IndexUpdatedEvent[]>;
    };
    [IDAEvent.IndexDistributionClaimed]: {
        events: IndexDistributionClaimedEvent[];
        groupedEvents: _.Dictionary<IndexDistributionClaimedEvent[]>;
    };
    [IDAEvent.IndexSubscribed]: {
        events: IndexSubscribedEvent[];
        groupedEvents: _.Dictionary<IndexSubscribedEvent[]>;
    };
    [IDAEvent.IndexUnitsUpdated]: {
        events: IndexUnitsUpdatedEvent[];
        groupedEvents: _.Dictionary<IndexUnitsUpdatedEvent[]>;
    };
    [IDAEvent.IndexUnsubscribed]: {
        events: IndexUnsubscribedEvent[];
        groupedEvents: _.Dictionary<IndexUnsubscribedEvent[]>;
    };
    [IDAEvent.SubscriptionApproved]: {
        events: SubscriptionApprovedEvent[];
        groupedEvents: _.Dictionary<SubscriptionApprovedEvent[]>;
    };
    [IDAEvent.SubscriptionDistributionClaimed]: {
        events: SubscriptionDistributionClaimedEvent[];
        groupedEvents: _.Dictionary<SubscriptionDistributionClaimedEvent[]>;
    };
    [IDAEvent.SubscriptionRevoked]: {
        events: SubscriptionRevokedEvent[];
        groupedEvents: _.Dictionary<SubscriptionRevokedEvent[]>;
    };
    [IDAEvent.SubscriptionUnitsUpdated]: {
        events: SubscriptionUnitsUpdatedEvent[];
        groupedEvents: _.Dictionary<SubscriptionUnitsUpdatedEvent[]>;
    };
}
