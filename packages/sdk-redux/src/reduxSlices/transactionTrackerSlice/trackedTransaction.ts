import {createEntityAdapter} from '@reduxjs/toolkit';
import {ethers} from 'ethers';

export type TransactionStatus = 'Pending' | 'Succeeded' | 'Failed' | 'Unknown';

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface TrackedTransaction {
    chainId: number;
    hash: string;
    /**
     * Milliseconds since epoch when started tracking the transaction.
     */
    timestampMs: number;
    status: TransactionStatus;
    transactionResponse: string;
    transactionReceipt?: string;
    ethersErrorCode?: ethers.errors;
    ethersErrorMessage?: string;
    key: string;
    extra?: unknown;
}

export const transactionsAdapter = createEntityAdapter<TrackedTransaction>({
    selectId: (transaction) => transaction.hash,
    sortComparer: (a) => a.timestampMs,
});

export const transactionSelectors = transactionsAdapter.getSelectors();
