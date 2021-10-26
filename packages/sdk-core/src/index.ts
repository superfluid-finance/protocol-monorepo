import Framework from "./Framework";
import Query from "./Query";

export const enum ChainId {
    ROPSTEN = 3,
    RINKEBY = 4,
    GOERLI = 5,
    KOVAN = 42,
    XDAI = 100,
    MATIC = 137,
    MUMBAI = 80001,
}

export const enum DataMode {
    SUBGRAPH_WEB3 = "SUBGRAPH_WEB3",
    WEB3_ONLY = "WEB3_ONLY",
}

export const enum NetworkName {
    ROPSTEN = "ropsten",
    RINKEBY = "rinkeby",
    GOERLI = "goerli",
    KOVAN = "kovan",
    XDAI = "xdai",
    MATIC = "matic",
    MUMBAI = "mumbai",
}

export const enum FlowActionType {
    CREATE,
    UPDATE,
    TERMINATE,
}
export default {
    Framework,
    Query,
};
