interface ContractAddresses {
    readonly resolver: string;
    readonly host: string;
    readonly governance: string;
    readonly cfaV1: string;
    readonly cfaV1Forwarder: string;
    readonly idaV1: string;
    readonly gdaV1: string;
    readonly superTokenFactory: string;
    readonly superfluidLoader: string;
    readonly toga: string;
}
interface SubgraphData {
    readonly name: string;
    readonly hostedEndpoint: string;
    readonly satsumaEndpoint?: string;
}
export interface NetworkMetaData {
    readonly name: string;
    readonly isTestnet: boolean;
    readonly networkId: number;
    readonly chainId: number;
    readonly shortName: string;
    readonly uppercaseName: string;
    readonly nativeTokenSymbol: string;
    readonly nativeTokenWrapper: string;
    readonly contractsV1: ContractAddresses;
    readonly startBlockV1: number;
    readonly logsQueryRange: number;
    readonly explorer: string;
    readonly subgraphV1: SubgraphData;
    readonly publicRPCs?: string[];
    readonly coinGeckoId?: string;
}
declare const _default: NetworkMetaData[];
export default _default;