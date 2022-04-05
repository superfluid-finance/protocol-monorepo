import {createEntityAdapter} from '@reduxjs/toolkit';
import {ethers} from 'ethers';

import {TransactionKey} from './transactionKey';

export type TransactionStatus = 'Pending' | 'Succeeded' | 'Failed' | 'Unknown';

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface TrackedTransaction {
    chainId: number;
    hash: string;
    /**
     * The address this transaction is from.
     */
    from: string;
    /**
     * Milliseconds since epoch when started tracking the transaction.
     */
    timestampMs: number;
    status: TransactionStatus;
    transactionResponse: string;
    transactionReceipt?: string;
    ethersErrorCode?: ethers.errors;
    ethersErrorMessage?: string;
    key: TransactionKey;
    extra?: unknown;
}

export const transactionsAdapter = createEntityAdapter<TrackedTransaction>({
    selectId: (transaction) => transaction.hash,
    sortComparer: (a) => a.timestampMs,
});

export const transactionSelectors = transactionsAdapter.getSelectors();
