/**************************************************************************
 * Param interfaces
 *************************************************************************/
export interface IQueryOptions {
    orderByProperty?: string;
    direction?: "asc" | "desc";
    limit?: number;
}

/**************************************************************************
 * GraphQL Entity Types
 *************************************************************************/

// Event Entities
interface IEvent {
    id: string;
    transactionHash: string;
    blockNumber: string;
}

export interface IMeta {
    _meta: {
        block: {
            number: number;
        };
    };
}

export interface IFlowUpdated extends IEvent {
    token: string;
    sender: string;
    receiver: string;
    flowRate: string;
    totalSenderFlowRate: string;
    totalReceiverFlowRate: string;
    oldFlowRate: string;
    type: string;
}

// HOL Entities
interface IBaseEntity {
	id: string;
	createdAt: string;
	updatedAt: string;
}

export interface IAccount extends IBaseEntity {}

export interface IToken extends IBaseEntity {
    name: string;
    symbol: string;
    underlyingAddress: string;
}

export interface IStream extends IBaseEntity {
    currentFlowRate: string;
    streamedUntilUpdatedAt: string;
    token: IToken;
    sender: IAccount;
    receiver: IAccount;
}
