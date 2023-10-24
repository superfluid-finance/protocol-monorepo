// Types
export type FlowActionType =
    | 0 // CREATE
    | 1 // UPDATE
    | 2; // TERMINATE

export type NetworkData = {
    subgraphAPIEndpoint: string;
    networkName: string;
    nativeTokenSymbol: string;
    addresses: {
        resolver: string;
        host: string;
        governance?: string;
        cfaV1: string;
        cfaV1Forwarder: string;
        idaV1: string;
        superTokenFactory: string;
        superfluidLoader: string;
        toga?: string;
    };
};
