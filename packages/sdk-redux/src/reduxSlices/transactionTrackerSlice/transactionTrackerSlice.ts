import {createSlice} from '@reduxjs/toolkit';

import {transactionTrackerAdapter} from './transactionTrackerAdapter';

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
