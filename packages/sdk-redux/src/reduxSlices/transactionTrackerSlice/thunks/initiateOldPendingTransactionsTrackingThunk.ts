import {createAsyncThunk} from '@reduxjs/toolkit';
import {ethers} from 'ethers';

import {transactionSelectors} from '../trackedTransaction';
import {TransactionTrackerReducer, transactionTrackerSlicePrefix} from '../transactionTrackerSlice';

import {trackPendingTransactionThunk} from './trackPendingTransactionThunk';

/**
 *
 */
export const initiateOldPendingTransactionsTrackingThunk = createAsyncThunk<
    void,
    {
        signerAddress: string;
        chainIds: number[];
    }
>(`${transactionTrackerSlicePrefix}/initiateOldPendingTransactionsTracking`, async (arg, {getState, dispatch}) => {
    const state = getState() as {[transactionTrackerSlicePrefix]: TransactionTrackerReducer};
    const signerAddress = ethers.utils.getAddress(arg.signerAddress);

    const transactions = transactionSelectors
        .selectAll(state[transactionTrackerSlicePrefix])
        .filter((x) => x.signer === signerAddress && arg.chainIds.includes(x.chainId));

    transactions.forEach((x) => dispatch(trackPendingTransactionThunk({chainId: x.chainId, transactionHash: x.hash})));
});
