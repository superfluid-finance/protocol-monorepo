import {
    createAsyncThunk,
    createEntityAdapter,
    createSlice,
} from '@reduxjs/toolkit';

import { superfluidSource } from '../../superfluidSource';

export type TransactionId = {
    chainId: number; // TODO(KK): Can I use "extends" here?
    transactionHash: string;
};

export enum TransactionStatus {
    Pending = 'Pending',
    Succeeded = 'Succeeded',
    Failed = 'Failed',
}

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface TransactionTracking {
    chainId: number;
    hash: string;
    status: TransactionStatus;
    error?: string;
}

// Not strictly necessary to use: https://redux-toolkit.js.org/api/createEntityAdapter
export const transactionsAdapter = createEntityAdapter<TransactionTracking>({
    selectId: (transaction) => transaction.hash,
    sortComparer: (a, b) => a.hash.localeCompare(b.hash),
});

export const superfluidTransactionSelectors =
    transactionsAdapter.getSelectors();

export interface TrackTransactionArg {
    chainId: number;
    hash: string;
}

// Having a single "track" action makes it easy to use transaction tracking logic.
export const trackTransaction = createAsyncThunk<
    TransactionTracking,
    TrackTransactionArg,
    { rejectValue: TransactionTracking }
>('trackTransaction', async (arg, thunkAPI) => {
    try {
        console.log({
            chainId: arg.chainId,
            transactionHash: arg.hash
        })
        const framework = await superfluidSource.getFramework(arg.chainId);
        // TODO: What's the best confirmation amount and timeout?
        const transactionReceipt = await framework.settings.provider.waitForTransaction(
            arg.hash,
            1,
            60000
        );
        if (transactionReceipt.status === 1) {
            return {
                chainId: arg.chainId,
                hash: arg.hash,
                status: TransactionStatus.Succeeded,
            };
        } else {
            return thunkAPI.rejectWithValue({
                chainId: arg.chainId,
                hash: arg.hash,
                status: TransactionStatus.Failed,
                error: 'Whatever error...',
            });
        }
    } catch (e) {
        console.error(e);

        return thunkAPI.rejectWithValue({
            chainId: arg.chainId,
            hash: arg.hash,
            status: TransactionStatus.Failed,
            error: 'Whatever error...',
        });
    }
});

export const transactionSlice = createSlice({
    name: 'transactions',
    initialState: transactionsAdapter.getInitialState(),
    reducers: {
        upsertTransaction: transactionsAdapter.upsertOne,
    },
    extraReducers: (builder) => {
        builder
            .addCase(trackTransaction.pending, (state, action) => {
                transactionsAdapter.upsertOne(state, {
                    chainId: action.meta.arg.chainId,
                    hash: action.meta.arg.hash,
                    status: TransactionStatus.Pending,
                });
            })
            .addCase(trackTransaction.fulfilled, (state, action) => {
                transactionsAdapter.upsertOne(state, action.payload);
            })
            .addCase(trackTransaction.rejected, (state, action) => {
                if (action.payload) {
                    transactionsAdapter.upsertOne(state, action.payload);
                } else {
                    throw Error("Haven't handled this use-case yet.");
                }
            });
    },
});

export type SuperfluidReduxTransactionSliceType = typeof transactionSlice;
