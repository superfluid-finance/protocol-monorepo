import {ILightEntity} from "../test/interfaces";

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
