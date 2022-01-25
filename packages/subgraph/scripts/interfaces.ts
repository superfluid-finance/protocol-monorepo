import { ILightEntity } from "../test/interfaces";

export interface IBaseEntity {
    readonly id: string;
    readonly createdAtTimestamp: string;
    readonly updatedAtTimestamp?: string;
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
    readonly token: { id: string, underlyingAddress: string };
    readonly account: ILightEntity;
}

export interface IDataIntegrityTokenStatistic extends IBaseEntity {
    readonly totalSupply: string;
    readonly token: { id: string, underlyingAddress: string };
}