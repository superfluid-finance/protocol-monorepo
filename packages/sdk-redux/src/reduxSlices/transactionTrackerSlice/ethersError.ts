import {ethers} from 'ethers';

export type EthersError = Error & {
    code: ethers.errors;
    reason?: 'reverted' | 'replaced' | 'repriced' | 'cancelled';
    cancelled?: boolean;
};
