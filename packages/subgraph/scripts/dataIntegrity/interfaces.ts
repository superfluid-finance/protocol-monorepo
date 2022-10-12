import _ from "lodash";
import { ILightEntity } from "../../test/interfaces";
import {
    FlowOperatorUpdatedEvent,
    FlowUpdatedEvent,
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
} from "@superfluid-finance/sdk-core";

export interface BaseEntity {
    readonly id: string;
    readonly createdAtTimestamp: string;
    readonly timestamp?: string;
    readonly updatedAtTimestamp?: string;
}

export interface BaseIndexEvent {
    readonly publisher: string;
    readonly token: string;
    readonly indexId: string;
}

export interface DataIntegrityFlowUpdatedEvent extends BaseEntity {
    readonly token: string;
    readonly transactionHash: string;
    readonly sender: string;
    readonly receiver: string;
    readonly flowRate: string;
    readonly totalSenderFlowRate: string;
    readonly totalReceiverFlowRate: string;
    readonly userData: string;
}

export interface DataIntegrityIndexCreatedEvent
    extends BaseEntity,
        BaseIndexEvent {
    readonly transactionHash: string;
    readonly userData: string;
}

export interface DataIntegrityIndexDistributionClaimedEvent
    extends BaseEntity,
        BaseIndexEvent {
    readonly transactionHash: string;
    readonly subscriber: string;
    readonly amount: string;
}

export interface DataIntegrityIndexUpdatedEvent
    extends BaseEntity,
        BaseIndexEvent {
    readonly transactionHash: string;
    readonly oldIndexValue: string;
    readonly newIndexValue: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly userData: string;
}

export interface DataIntegrityIndexSubscribedEvent
    extends BaseEntity,
        BaseIndexEvent {
    readonly transactionHash: string;
    readonly subscriber: string;
    readonly userData: string;
}

export interface DataIntegrityIndexUnitsUpdatedEvent
    extends BaseEntity,
        BaseIndexEvent {
    readonly transactionHash: string;
    readonly subscriber: string;
    readonly units: string;
    readonly userData: string;
}

export interface DataIntegrityStream extends BaseEntity {
    readonly updatedAtTimestamp: string;
    readonly currentFlowRate: string;
    readonly deposit: string;
    readonly token: ILightEntity;
    readonly sender: ILightEntity;
    readonly receiver: ILightEntity;
}

export interface DataIntegrityIndex extends BaseEntity {
    readonly indexId: string;
    readonly indexValue: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly totalUnits: string;
    readonly token: ILightEntity;
    readonly publisher: ILightEntity;
}

export interface DataIntegritySubscription extends BaseEntity {
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
export interface DataIntegrityAccountTokenSnapshot extends BaseEntity {
    readonly balanceUntilUpdatedAt: string;
    readonly totalNetFlowRate: string;
    readonly token: { id: string; underlyingAddress: string };
    readonly account: ILightEntity;
}

export interface DataIntegrityTokenStatistic extends BaseEntity {
    readonly totalSupply: string;
    readonly token: { id: string; underlyingAddress: string };
}

export const enum CFAEvent {
    FlowOperatorUpdated = "FlowOperatorUpdated",
    FlowUpdated = "FlowUpdated",
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

export interface OnChainCFAEvents {
    [CFAEvent.FlowOperatorUpdated]: {
        events: FlowOperatorUpdatedEvent[];
        groupedEvents: _.Dictionary<FlowOperatorUpdatedEvent[]>;
    };
    [CFAEvent.FlowUpdated]: {
        events: FlowUpdatedEvent[];
        groupedEvents: _.Dictionary<FlowUpdatedEvent[]>;
    };
}
export interface OnChainIDAEvents {
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
