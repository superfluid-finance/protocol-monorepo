export type QueryArg = Record<string, unknown> & {
    chainId: number;
};

export type PaginatedQueryArg = QueryArg & {
    skip: number;
    take: number;
};

export type PaginatedQueryResult = {
    skip: number;
    take: number;
    hasNextPage: boolean;
};

export type SuperTokenMutationArg = Record<string, unknown> & {
    chainId: number;
    superTokenAddress: string;
    waitForConfirmation?: boolean;
};

export type TransactionInfo = {
    chainId: number;
    hash: string;
};
