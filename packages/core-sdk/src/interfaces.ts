import { ChainId, DataMode, NetworkName } from ".";

type ChainIdType =
    | ChainId.ROPSTEN
    | ChainId.RINKEBY
    | ChainId.GOERLI
    | ChainId.KOVAN
    | ChainId.XDAI
    | ChainId.MATIC
    | ChainId.MUMBAI;

type DataModeType = DataMode.SUBGRAPH_WEB3 | DataMode.WEB3_ONLY;

type NetworkNameType =
    | NetworkName.ROPSTEN
    | NetworkName.RINKEBY
    | NetworkName.GOERLI
    | NetworkName.KOVAN
    | NetworkName.XDAI
    | NetworkName.MATIC
    | NetworkName.MUMBAI;

export interface IFrameworkOptions {
    chainId?: ChainIdType;
    customSubgraphQueriesEndpoint?: string;
    dataMode?: DataModeType;
    networkName?: NetworkNameType;
    protocolReleaseVersion?: string;
}
