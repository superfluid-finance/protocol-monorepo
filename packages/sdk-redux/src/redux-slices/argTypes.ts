export type NothingString = "" | undefined;
export type NothingNumber = undefined;
export type NothingBoolean = undefined;

export type TransactionInfo = {
    chainId: number;
    hash: string;
};

export type QueryArg = {
    chainId: number;
};

export type PaginatedQueryArg = QueryArg & {
    skip: number;
    take: number;
};

export type SuperTokenMutationArg = {
    chainId: number;
    superTokenAddress: string;
    waitForConfirmation: boolean | NothingBoolean;
};
