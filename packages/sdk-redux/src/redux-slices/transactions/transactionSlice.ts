import {createSlice} from '@reduxjs/toolkit';

import {getConfig} from '../../sdkReduxConfig';

import {transactionsAdapter} from './trackedTransaction';

export const transactionSlicePrefix = 'sfTransactions' as const;

/**
 * For initializing "sfTransaction" Redux slice.
 */
export const initializeSfTransactionSlice = () => {
    const slice = createSlice({
        name: transactionSlicePrefix,
        initialState: transactionsAdapter.getInitialState(),
        reducers: {
            upsertTransaction: transactionsAdapter.upsertOne,
        },
    });
    getConfig().setTransactionSlice(slice as any);
    return slice;
};

export type SfTransactionSliceType = ReturnType<typeof initializeSfTransactionSlice>;
