export = User;
declare class User {
    constructor({ sf, address, token, options }: {
        sf: any;
        address: any;
        token: any;
        options: any;
    });
    sf: any;
    address: any;
    token: any;
    options: any;
    details(): Promise<{
        cfa: {
            flows: any;
            netFlow: any;
        };
        ida: {
            subscriptions: any;
        };
    }>;
    flow({ recipient, flowRate, ...options }: {
        [x: string]: any;
        recipient: any;
        flowRate: any;
    }): Promise<any>;
    createPool({ poolId: indexId }: {
        poolId: any;
    }): Promise<any>;
    giveShares({ recipient, shares, poolId: indexId }: {
        recipient: any;
        shares: any;
        poolId: any;
    }): Promise<any>;
    distributeToPool({ poolId: indexId, amount }: {
        poolId: any;
        amount: any;
    }): Promise<void>;
}
