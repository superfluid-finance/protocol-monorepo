import type Framework from './Framework';
import type BN from 'bn';
import Transaction from 'web3';

type Flow = {
    sender: string
    receiver: string
    flowRate: string
}

type DetailsType = Promise<{
    cfa: {
        flows: {
            inFlows: Array<Flow>;
            outFlows: Array<Flow>;
        }
        netFlow: string // numeric string
    };
    ida: {
        subscriptions: Array<Subscription>;
    };
}>;

export = User;
declare class User {
    constructor({ sf, address, token, options }: {
        sf: Framework;
        address: string;
        token: string;
        options: any;
    });
    sf: Framework;
    address: string;
    token: string;
    options: any;
    details(): DetailsType;
    flow({ recipient, flowRate, ...options }: {
        recipient: string;
        flowRate: BN;
        [x: string]: any;
    }): Promise<Transaction>;
    createPool({ poolId: indexId }: {
        poolId: number;
    }): Promise<Transaction>;
    giveShares({ recipient, shares, poolId: indexId }: {
        recipient: string;
        shares: BN;
        poolId: number;
    }): Promise<Transaction>;
    distributeToPool({ poolId: indexId, amount }: {
        poolId: number;
        amount: BN;
    }): Promise<void>;
}
