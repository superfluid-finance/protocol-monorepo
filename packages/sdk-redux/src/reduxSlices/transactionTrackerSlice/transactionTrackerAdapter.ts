import {createEntityAdapter} from '@reduxjs/toolkit';

import {TrackedTransaction} from './trackedTransaction';
import {TransactionTrackerReducer, transactionTrackerSlicePrefix} from './transactionTrackerSlice';

export const transactionTrackerAdapter = createEntityAdapter<TrackedTransaction>({
    selectId: (transaction) => transaction.hash,
    sortComparer: (a) => a.timestampMs,
});

export const transactionTrackerSelectors = transactionTrackerAdapter.getSelectors<{
    [transactionTrackerSlicePrefix]: TransactionTrackerReducer;
}>((state) => state[transactionTrackerSlicePrefix]);
