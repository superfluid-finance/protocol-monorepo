import {createEntityAdapter, createSlice} from '@reduxjs/toolkit';

import {TrackedTransaction} from './trackedTransaction';

export const transactionTrackerSlicePrefix = 'superfluid_transactions' as const;

export const createTransactionTrackerSlice = () => ({
    reducerPath: transactionTrackerSlicePrefix,
    ...createSlice({
        name: transactionTrackerSlicePrefix,
        initialState: transactionTrackerAdapter.getInitialState(),
        reducers: {
            addTransaction: transactionTrackerAdapter.addOne,
            updateTransaction: transactionTrackerAdapter.updateOne,
        },
    }),
});

export type TransactionTrackerSlice = ReturnType<typeof createTransactionTrackerSlice>;
export type TransactionTrackerReducer = ReturnType<TransactionTrackerSlice['getInitialState']>;

export const transactionTrackerAdapter = createEntityAdapter<TrackedTransaction>({
    selectId: (transaction) => transaction.hash,
    sortComparer: (a) => a.timestampMs,
});

export const transactionTrackerSelectors = transactionTrackerAdapter.getSelectors<{
    [transactionTrackerSlicePrefix]: TransactionTrackerReducer;
}>((state) => state[transactionTrackerSlicePrefix]);
