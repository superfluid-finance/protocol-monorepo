import {createAsyncThunk} from '@reduxjs/toolkit';
import {ethers} from 'ethers';

import {getTransactionTrackerSlice} from '../../../sdkReduxConfig';
import {TransactionKey} from '../transactionKey';
import {transactionTrackerSlicePrefix} from '../transactionTrackerSlice';

import {trackPendingTransactionThunk} from './trackPendingTransactionThunk';

/**
 *
 */
export const initiateNewTransactionTrackingThunk = createAsyncThunk<
    void,
    {
        chainId: number;
        transactionResponse: ethers.providers.TransactionResponse;
        from: string;
        key: TransactionKey;
        extra?: unknown;
    }
>(`${transactionTrackerSlicePrefix}/initiateNewTransactionTracking`, async (arg, {dispatch}) => {
    arg.transactionResponse.chainId = arg.chainId; // Recommended by Ethers to specify Chain ID when doing serialization.

    const transactionHash = arg.transactionResponse.hash;

    dispatch(
        getTransactionTrackerSlice().actions.addTransaction({
            chainId: arg.chainId,
            hash: transactionHash,
            from: ethers.utils.getAddress(arg.from),
            timestampMs: new Date().getTime(),
            status: 'Pending',
            transactionResponse: ethers.utils.serializeTransaction(arg.transactionResponse),
            key: arg.key,
            ...(arg.extra ? {extra: arg.extra} : {}),
        })
    );

    dispatch(trackPendingTransactionThunk({transactionHash, chainId: arg.chainId}));
});
