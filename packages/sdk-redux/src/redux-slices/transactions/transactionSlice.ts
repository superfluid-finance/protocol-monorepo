import { createEntityAdapter, createSlice } from '@reduxjs/toolkit';
import { ethers } from 'ethers';

export type TransactionStatus = 'Pending' | 'Succeeded' | 'Failed' | 'Unknown';

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface TrackedTransaction {
    chainId: number;
    hash: string;
    status: TransactionStatus;
    ethersErrorCode?: ethers.errors;
    ethersErrorMessage?: string;
}

const getTransactionId = (transaction: TrackedTransaction) =>
    `${transaction.chainId}_${transaction.hash}`;

export const transactionsAdapter = createEntityAdapter<TrackedTransaction>({
    selectId: (transaction) => getTransactionId(transaction),
    sortComparer: (a, b) =>
        getTransactionId(a).localeCompare(getTransactionId(b)),
});

export const transactionSelectors = transactionsAdapter.getSelectors();

export const transactionSlicePrefix = 'sfTransactions' as const;

export const transactionSlice = {
    reducerPath: transactionSlicePrefix,
    ...createSlice({
        name: transactionSlicePrefix,
        initialState: transactionsAdapter.getInitialState(),
        reducers: {
            upsertTransaction: transactionsAdapter.upsertOne,
        },
    }),
};

export type SuperfluidTransactionReduxSlice = typeof transactionSlice;
