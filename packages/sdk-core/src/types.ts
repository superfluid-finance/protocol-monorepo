// Types
export type NetworkName =
    | "ropsten"
    | "rinkeby"
    | "goerli"
    | "kovan"
    | "xdai"
    | "matic"
    | "mumbai"
    | "custom";

export type ChainId =
    | 3 // ROPSTEN
    | 4 // RINKEBY
    | 5 // GOERLI
    | 42 // KOVAN
    | 100 // XDAI
    | 137 // MATIC
    | 80001; // MUMBAI

export type DataMode = "SUBGRAPH_ONLY" | "SUBGRAPH_WEB3" | "WEB3_ONLY";

export type FlowActionType =
    | 0 // CREATE
    | 1 // UPDATE
    | 2; // TERMINATE
