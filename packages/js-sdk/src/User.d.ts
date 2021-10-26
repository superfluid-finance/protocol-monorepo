import type { Framework } from './Framework';
import type BN from 'bn.js';
import type { Transaction } from 'web3';
import type { Flow } from './ConstantFlowAgreementV1Helper';
import type { Subscription } from './InstantDistributionAgreementV1Helper';

// params options types
export interface UserOptions {
    sf: Framework;
    address: string;
    token: string;
}

// specified User to avoid confusion with CFA CreateFlowOptions
export interface UserFlowOptions {
    recipient: string;
    flowRate: string;
    userData?: string;
    onTransaction?: ()=>any;
    by?: string;
}

export interface CreatePoolOptions {
    poolId: number;
}

export interface GiveSharesOptions {
    recipient: string;
    shares: number | BN;
    poolId: number;
}

export interface DistributeToPoolOptions {
    poolId: number;
    amount: number | BN;
}

export interface UserDetails {
    cfa: {
        flows: FlowList
        netFlow: string // numeric string
    }
    ida: {
        subscriptions: Array<Subscription>;
    }
}

export declare class User {
    constructor({ sf, address, token }: UserOptions);
    sf: Framework;
    address: string;
    token: string;
    details(): Promise<UserDetails>;
    flow({
        recipient,
        flowRate,
        ...options
    }: UserFlowOptions): Promise<Transaction | undefined>;
    createPool({ poolId: indexId }: CreatePoolOptions): Promise<Transaction | undefined>;
    giveShares({ recipient, shares, poolId: indexId }: GiveSharesOptions): Promise<Transaction | undefined>;
    distributeToPool({ poolId: indexId, amount }: DistributeToPoolOptions): Promise<void>;
}
