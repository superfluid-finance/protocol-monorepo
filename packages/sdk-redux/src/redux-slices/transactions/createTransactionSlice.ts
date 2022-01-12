import {createSlice} from '@reduxjs/toolkit';

import {transactionsAdapter} from './trackedTransaction';

export const transactionSlicePrefix = 'sfTransactions' as const;

export const createTransactionSlice = () =>
    createSlice({
        name: transactionSlicePrefix,
        initialState: transactionsAdapter.getInitialState(),
        reducers: {
            upsertTransaction: transactionsAdapter.upsertOne,
        },
    });

export type SfTransactionSliceType = ReturnType<typeof createTransactionSlice>;
