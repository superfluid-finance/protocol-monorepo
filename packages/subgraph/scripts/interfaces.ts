import { ILightEntity } from "../test/interfaces";

export interface IBaseEntity {
    readonly id: string;
    readonly createdAtTimestamp: string;
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

export interface IDataIntegrityAccountTokenSnapshot extends IBaseEntity {
    readonly totalNetFlowRate: string;
    readonly token: ILightEntity;
    readonly account: ILightEntity;
}
