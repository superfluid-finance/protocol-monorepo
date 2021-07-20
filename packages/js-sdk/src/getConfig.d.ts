export declare interface NetworkConfig {
    nativeTokenSymbol?: string,
    resolverAddress?: string
}

declare function getConfig(chainId: number): NetworkConfig;

export = getConfig;