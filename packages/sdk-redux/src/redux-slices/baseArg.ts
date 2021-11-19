export type QueryArg = Record<string, unknown> & {
    chainId: number;
};

export type PaginatedQueryArg = QueryArg & {
    skip: number | NothingNumber;
    take: number | NothingNumber;
};

export type PaginatedQueryResult = {
    skip: number;
    take: number;
    hasNextPage: boolean;
};

export type SuperTokenMutationArg = Record<string, unknown> & {
    chainId: number;
    superTokenAddress: string;
    waitForConfirmation: boolean | NothingBoolean;
};

export type TransactionInfo = {
    chainId: number;
    hash: string;
};

export type NothingString = "" | undefined;
export type NothingNumber = undefined;
export type NothingBoolean = undefined;
