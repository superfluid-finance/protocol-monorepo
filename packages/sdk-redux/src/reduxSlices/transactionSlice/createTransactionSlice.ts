import {createSlice} from '@reduxjs/toolkit';

import {transactionsAdapter} from './trackedTransaction';

export const transactionSlicePrefix = 'superfluid/transactions' as const;

export const createTransactionSlice = () =>
    createSlice({
        name: transactionSlicePrefix,
        initialState: transactionsAdapter.getInitialState(),
        reducers: {
            addTransaction: transactionsAdapter.addOne,
            updateTransaction: transactionsAdapter.updateOne,
        },
    });

export type TransactionSlice = ReturnType<typeof createTransactionSlice>;
