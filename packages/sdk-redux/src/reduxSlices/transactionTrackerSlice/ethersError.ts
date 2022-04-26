import {ethers} from 'ethers';

export type EthersError = Error & {
    code: ethers.errors;
    reason?: 'replaced' | 'repriced' | 'cancelled';
    cancelled?: boolean;
};
