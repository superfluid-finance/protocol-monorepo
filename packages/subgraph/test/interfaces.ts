/**************************************************************************
 * Param interfaces
 *************************************************************************/
export interface IQueryOptions {
    readonly orderByProperty?: string;
    readonly direction?: "asc" | "desc";
    readonly limit?: number;
}

/**************************************************************************
 * GraphQL Entity Types
 *************************************************************************/

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
    readonly totalSenderFlowRate: string;
    readonly totalReceiverFlowRate: string;
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
    readonly subscriber: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface ISubscriptionRevoked extends IEvent {
    readonly token: string;
    readonly subscriber: string;
    readonly publisher: string;
    readonly indexId: string;
    readonly userData: string;
}

export interface ISubscriptionUnitsUpdated extends IEvent {
    readonly token: string;
    readonly subscriber: string;
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
    readonly updatedAt: string;
}

export interface IAccount extends IBaseEntity {}

export interface IToken extends IBaseEntity {
    readonly name: string;
    readonly symbol: string;
    readonly underlyingAddress: string;
}

export interface IStream extends IBaseEntity {
    readonly currentFlowRate: string;
    readonly streamedUntilUpdatedAt: string;
    readonly token: IToken;
    readonly sender: IAccount;
    readonly receiver: IAccount;
}

export interface ISubscriber extends IBaseEntity {
    readonly token: IToken;
    readonly subscriber: IAccount;
    readonly publisher: IAccount;
    readonly indexId: string;
    readonly userData: string;
    readonly approved: boolean;
    readonly units: string;
    readonly totalUnitsReceivedUntilUpdatedAt: string;
    readonly lastIndexValue: string;
    readonly index: IIndex;
}

export interface IIndex extends IBaseEntity {
    readonly indexId: string;
    readonly userData: string;
    readonly oldIndexValue: string;
    readonly newIndexValue: string;
    readonly totalSubscribers: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsApproved: string;
    readonly totalUnits: string;
    readonly totalUnitsDistributed: string;
    readonly token: IToken;
    readonly publisher: IAccount;
}

/**
 * Aggregate Entities
 */
export interface IAccountTokenSnapshot {
    readonly id: string;
    readonly totalNumberOfStreams: number;
    readonly totalSubscriptions: number;
    readonly totalApprovedSubscriptions: number;
    readonly balance: string;
    readonly totalNetFlowRate: string;
    readonly account: IAccount;
    readonly token: IToken;
}

export interface ITokenStatistic {
    readonly id: string;
    readonly totalNumberOfStreams: number;
    readonly totalNumberOfIndexes: number;
    readonly totalSubscribers: number;
    readonly totalApprovedSubscribers: number;
    readonly totalOutflowRate: string;
    readonly totalUnitsApproved: string;
    readonly totalUnitsPending: string;
    readonly totalUnitsDistributed: string;
    readonly token: IToken;
}
