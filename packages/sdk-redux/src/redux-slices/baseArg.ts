export interface QueryArg extends Record<string, unknown> {
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

export interface MutationArg extends Record<string, unknown> {
    chainId: number;
    waitForConfirmation?: boolean;
}

export interface TransactionInfo {
    chainId: number;
    hash: string;
}
