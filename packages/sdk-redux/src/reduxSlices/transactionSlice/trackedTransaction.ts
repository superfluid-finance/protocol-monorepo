import {createEntityAdapter} from '@reduxjs/toolkit';
import {ethers} from 'ethers';

export type TransactionStatus = 'Pending' | 'Succeeded' | 'Failed' | 'Unknown';

// TODO(KK): This is temporary solution.
export interface ExecutedMutation {
    endpoint: string;
    arg: unknown;
}

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface TrackedTransaction {
    chainId: number;
    hash: string;
    status: TransactionStatus;
    ethersErrorCode?: ethers.errors;
    ethersErrorMessage?: string;
    executedMutation?: ExecutedMutation;
}

export const transactionsAdapter = createEntityAdapter<TrackedTransaction>({
    selectId: (transaction) => transaction.hash,
    sortComparer: (a, b) => a.hash.localeCompare(b.hash),
});

export const transactionSelectors = transactionsAdapter.getSelectors();
