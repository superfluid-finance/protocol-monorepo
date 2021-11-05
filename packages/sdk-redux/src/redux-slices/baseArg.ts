export interface QueryArg {
    chainId: number;
}

export interface MutationArg {
    chainId: number;
    waitForConfirmation?: boolean;
}

export interface TransactionInfo {
    chainId: number;
    hash: string;
}
