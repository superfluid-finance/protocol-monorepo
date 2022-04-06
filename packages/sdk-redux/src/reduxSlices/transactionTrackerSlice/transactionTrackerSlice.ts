import {createSlice} from '@reduxjs/toolkit';

import {transactionsAdapter} from './trackedTransaction';

export const transactionTrackerSlicePrefix = 'superfluid_transactions' as const;

export const createTransactionTrackerSlice = () => ({
    reducerPath: transactionTrackerSlicePrefix,
    ...createSlice({
        name: transactionTrackerSlicePrefix,
        initialState: transactionsAdapter.getInitialState(),
        reducers: {
            addTransaction: transactionsAdapter.addOne,
            updateTransaction: transactionsAdapter.updateOne,
        },
    }),
});

export type TransactionTrackerSlice = ReturnType<typeof createTransactionTrackerSlice>;
export type TransactionTrackerReducer = ReturnType<TransactionTrackerSlice['getInitialState']>;
