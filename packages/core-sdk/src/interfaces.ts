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

export interface ISubgraphResponse<T> {
    readonly response: T;
}

export interface ILightEntity {
    readonly id: string;
}

export interface IEventEntityBase {
    readonly id: string;
    readonly blockNumber: string;
    readonly timestamp: string;
    readonly transactionHash: string;
}

export interface IHOLEntityBase {
    readonly id: string;
    readonly createdAtTimestamp: string;
    readonly createdAtBlockNumber: string;
}

export interface IHOLUpdateable extends IHOLEntityBase {
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlockNumber: string;
}

export interface IStream extends IHOLUpdateable {
    readonly currentFlowRate: string;
    readonly streamedUntilUpdatedAt: string;
}

export interface ISuperToken extends IHOLEntityBase {
    readonly name: string;
    readonly symbol: string;
    readonly isListed: boolean;
    readonly underlyingAddress: string;
}

export interface IAggregateEntityBase {
    readonly id: string;
    readonly updatedAtTimestamp: string;
    readonly updatedAtBlockNumber: string;
}

export interface ILightAccountTokenSnapshot extends IAggregateEntityBase {
    readonly totalNumberOfActiveStreams: number;
    readonly totalNumberOfClosedStreams: number;
    readonly totalSubscriptionsWithUnits: number;
    readonly totalApprovedSubscriptions: number;
    readonly balanceUntilUpdatedAt: string;
    readonly totalNetFlowRate: string;
    readonly totalInflowRate: string;
    readonly totalOutflowRate: string;
    readonly totalAmountStreamedUntilUpdatedAt: string;
    readonly totalAmountTransferredUntilUpdatedAt: string;
    readonly account: ILightEntity;
    readonly token: ISuperToken;
}
