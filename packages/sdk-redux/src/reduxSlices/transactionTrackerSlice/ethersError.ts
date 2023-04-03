import {ethers} from 'ethers';

import {NewTransactionResponse} from './registerNewTransaction';

export type EthersError = Error & {
    code: ethers.errors;
    reason?: 'reverted' | 'replaced' | 'repriced' | 'cancelled';
    cancelled?: boolean;
    replacement?: NewTransactionResponse;
};
