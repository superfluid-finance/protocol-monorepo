import {
    createAsyncThunk,
    createEntityAdapter,
    createSlice,
} from '@reduxjs/toolkit';

import { superfluidSource } from '../../superfluidSource';
import { rtkQuerySlice } from '../rtk-query/rtkQuerySlice';
import ethers from 'ethers';
import { AnyAction, ThunkDispatch } from '@reduxjs/toolkit';
import {MsTimes} from "../../utils";

export type TransactionId = {
    chainId: number; // TODO(KK): Can I use "extends" here?
    transactionHash: string;
};

export enum TransactionStatus {
    Pending = 'Pending',
    Succeeded = 'Succeeded',
    Failed = 'Failed',
    ReOrg = 'Re-org',
}

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface TransactionTracking {
    chainId: number;
    hash: string;
    status: TransactionStatus;
    error?: string;
}

// Not strictly necessary to use: https://docs.ethers.io/v5/api/providers/types/#providers-TransactionResponse
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

// TODO(KK): https://docs.ethers.io/v5/api/providers/types/#providers-TransactionResponse

// Having a single "track" action makes it easy to use transaction tracking logic.
export const trackTransaction = createAsyncThunk<void, TrackTransactionArg>(
    'trackTransaction',
    async (arg, thunkAPI) => {
        thunkAPI.dispatch(
            transactionSlice.actions.upsertTransaction({
                chainId: arg.chainId,
                hash: arg.hash,
                status: TransactionStatus.Pending,
            })
        );

        const framework = await superfluidSource.getFramework(arg.chainId);

        framework.settings.provider
            .waitForTransaction(arg.hash, 1, MsTimes.OneMinute)
            .then((receipt) => {
                if (receipt.status === 1) {
                    thunkAPI.dispatch(
                        transactionSlice.actions.upsertTransaction({
                            chainId: arg.chainId,
                            hash: arg.hash,
                            status: TransactionStatus.Succeeded,
                        })
                    );
                    listenForReOrg(
                        framework.settings.provider,
                        arg,
                        thunkAPI.dispatch
                    );
                } else {
                    notifyOfError()
                }
            })
            .catch(() => {
                // TODO(KK): Could be timeout as well...
                notifyOfError()
            });

        const notifyOfError = () => {
            thunkAPI.dispatch(
                transactionSlice.actions.upsertTransaction({
                    chainId: arg.chainId,
                    hash: arg.hash,
                    status: TransactionStatus.Failed,
                    error: 'Whatever error...',
                })
            );
        };
    }
);

const listenForReOrg = (
    provider: ethers.providers.Provider,
    { chainId, hash }: TrackTransactionArg,
    dispatch: ThunkDispatch<any, any, AnyAction>
) => {
    provider
        .waitForTransaction(hash, 10, MsTimes.TenMinutes)
        .then((receipt: ethers.providers.TransactionReceipt) => {
            // TODO(KK): Investigate how re-orgs exactly look like from Ethers perspective...
            if (receipt.status !== 1) {
                notifyOfReOrg();
            } else {
                console.log("re-org didn't happen")
            }
        })
        .catch(() => {
            // TODO(KK): Investigate how re-orgs exactly look like from Ethers perspective...
            // TODO(KK): Could be timeout as well...
            notifyOfReOrg();
        });

    const notifyOfReOrg = () => {
        console.log("re-org happened")

        dispatch(
            transactionSlice.actions.upsertTransaction({
                chainId: chainId,
                hash: hash,
                status: TransactionStatus.ReOrg,
            })
        );

        // Completely reset API cache.
        dispatch(rtkQuerySlice.util.resetApiState());
    };
};

export const transactionSlice = createSlice({
    name: 'transactions',
    initialState: transactionsAdapter.getInitialState(),
    reducers: {
        upsertTransaction: transactionsAdapter.upsertOne,
    },
});

export type SuperfluidReduxTransactionSliceType = typeof transactionSlice;
