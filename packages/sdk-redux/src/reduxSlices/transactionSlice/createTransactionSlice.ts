import {createSlice} from '@reduxjs/toolkit';

import {transactionsAdapter} from './trackedTransaction';

export const transactionSlicePrefix = 'superfluid/transactions' as const;

export const createTransactionSlice = () =>
    createSlice({
        name: transactionSlicePrefix,
        initialState: transactionsAdapter.getInitialState(),
        reducers: {
            upsertTransaction: transactionsAdapter.upsertOne,
        },
    });

export type TransactionSlice = ReturnType<typeof createTransactionSlice>;
