import {createAsyncThunk} from '@reduxjs/toolkit';
import {ethers} from 'ethers';

import {getTransactionTrackerSlice} from '../../../sdkReduxConfig';
import {TransactionTitle} from '../transactionTitle';
import {transactionTrackerSlicePrefix} from '../transactionTrackerSlice';
import {trySerializeTransaction} from '../trySerializeTransaction';

import {trackPendingTransactionThunk} from './trackPendingTransactionThunk';

/**
 * Used for tracking _brand new_ transactions.
 */
export const initiateNewTransactionTrackingThunk = createAsyncThunk<
    void,
    {
        chainId: number;
        transactionResponse: ethers.providers.TransactionResponse;
        signer: string;
        title: TransactionTitle;
        extraData: Record<string, unknown>;
    }
>(`${transactionTrackerSlicePrefix}/initiateNewTransactionTracking`, async (arg, {dispatch}) => {
    arg.transactionResponse.chainId = arg.chainId; // Recommended by Ethers to specify Chain ID when doing serialization.

    const transactionHash = arg.transactionResponse.hash;

    dispatch(
        getTransactionTrackerSlice().actions.addTransaction({
            chainId: arg.chainId,
            hash: transactionHash,
            signer: ethers.utils.getAddress(arg.signer),
            timestampMs: new Date().getTime(),
            status: 'Pending',
            transactionResponse: trySerializeTransaction(arg.transactionResponse),
            title: arg.title,
            extraData: arg.extraData,
        })
    );

    dispatch(trackPendingTransactionThunk({transactionHash, chainId: arg.chainId}));
});
