import {ethers} from 'ethers';

import {TransactionTitle} from './transactionTitle';

export type TransactionStatus = 'Pending' | 'Succeeded' | 'Failed' | 'Unknown';

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface TrackedTransaction {
    chainId: number;
    hash: string;
    /**
     * The address this transaction is from.
     */
    signer: string;
    /**
     * Milliseconds since epoch when started tracking the transaction.
     */
    timestampMs: number;
    status: TransactionStatus;
    transactionResponse: string;
    transactionReceipt?: string;
    ethersErrorCode?: ethers.errors;
    ethersErrorMessage?: string;
    title: TransactionTitle;
    extraData: Record<string, unknown>;
}
