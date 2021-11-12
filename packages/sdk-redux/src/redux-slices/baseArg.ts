export interface QueryArg {
    chainId: number;
}

export interface PaginatedQueryArg extends QueryArg {
    skip: number;
    take: number;
}

export interface PaginatedQueryResult {
    skip: number;
    take: number;
    hasNextPage: boolean;
}

export interface MutationArg {
    chainId: number;
    waitForConfirmation?: boolean;
}

export interface TransactionInfo {
    chainId: number;
    hash: string;
}
