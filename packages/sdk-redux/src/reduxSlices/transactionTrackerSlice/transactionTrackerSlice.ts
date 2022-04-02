import {createSlice} from '@reduxjs/toolkit';

import {transactionsAdapter} from './trackedTransaction';

export const transactionTrackerSlicePrefix = 'superfluid/transactions' as const;

export const createTransactionTrackerSlice = () =>
    createSlice({
        name: transactionTrackerSlicePrefix,
        initialState: transactionsAdapter.getInitialState(),
        reducers: {
            addTransaction: transactionsAdapter.addOne,
            updateTransaction: transactionsAdapter.updateOne,
        },
    });

export type TransactionTrackerSlice = ReturnType<typeof createTransactionTrackerSlice>;
