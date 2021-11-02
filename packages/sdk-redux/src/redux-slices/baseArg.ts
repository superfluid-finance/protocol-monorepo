export interface QueryArg {
    chainId: number;
}

export interface MutationArg {
    chainId: number;
    confirmations?: number;
}

export interface TransactionInfo {
    chainId: number;
    hash: string;
}
