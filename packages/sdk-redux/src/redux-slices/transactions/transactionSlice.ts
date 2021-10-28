import {
    createAsyncThunk,
    createEntityAdapter,
    createSlice,
} from '@reduxjs/toolkit';
import { NetworkName } from '@superfluid-finance/sdk-core';

import { superfluidFrameworkSource } from '../../superfluidFrameworkSource';

export type TransactionId = {
    networkName: NetworkName;
    transactionHash: string;
};

export enum TransactionStatus {
    Pending = 'Pending',
    Succeeded = 'Succeeded',
    Failed = 'Failed',
}

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface SuperfluidTransaction {
    networkName: string;
    hash: string;
    status: TransactionStatus;
    error?: string;
}

// Not strictly necessary to use: https://redux-toolkit.js.org/api/createEntityAdapter
export const transactionsAdapter = createEntityAdapter<SuperfluidTransaction>({
    selectId: (transaction) => transaction.hash,
    sortComparer: (a, b) => a.hash.localeCompare(b.hash),
});

export const superfluidTransactionSelectors =
    transactionsAdapter.getSelectors();

// Having a single "track" action makes it easy to use transaction tracking logic.
export const trackTransaction = createAsyncThunk<
    SuperfluidTransaction,
    TransactionId,
    { rejectValue: SuperfluidTransaction }
>('trackTransaction', async (arg, thunkAPI) => {
    const framework = await superfluidFrameworkSource.getForRead(
        arg.networkName
    );
    try {
        // TODO: What's the best confirmation amount and timeout?
        const transactionReceipt = await framework.ethers.waitForTransaction(
            arg.transactionHash,
            1,
            60000
        );
        if (transactionReceipt.status === 1) {
            return {
                networkName: arg.networkName,
                hash: arg.transactionHash,
                status: TransactionStatus.Succeeded,
            };
        } else {
            return thunkAPI.rejectWithValue({
                networkName: arg.networkName,
                hash: arg.transactionHash,
                status: TransactionStatus.Failed,
                error: 'Whatever error...',
            });
        }
    } catch (e) {
        return thunkAPI.rejectWithValue({
            networkName: arg.networkName,
            hash: arg.transactionHash,
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
                    networkName: action.meta.arg.networkName,
                    hash: action.meta.arg.transactionHash,
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
