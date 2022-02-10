import { HardhatEthersHelpers } from "@nomiclabs/hardhat-ethers/types";
import { ethers } from "ethers";
import Web3 from "web3";

import { IConfig } from "./interfaces";
import { DataMode } from "./types";

type SupportedProvider =
    | ethers.providers.Provider
    | (typeof ethers & HardhatEthersHelpers)
    | Web3;

// TODO: add convenience function of utilizing provider (optional)
// instead of having to pass it in every single time
export interface IFrameworkOptions {
    chainId?: number;
    customSubgraphQueriesEndpoint?: string;
    dataMode?: DataMode;
    networkName?: string;
    resolverAddress?: string;
    protocolReleaseVersion?: string;
    provider: SupportedProvider;
}

export interface IFrameworkSettings {
    chainId: number;
    customSubgraphQueriesEndpoint: string;
    dataMode: DataMode;
    networkName: string;
    protocolReleaseVersion: string;
    provider: ethers.providers.Provider;
    config: IConfig;
}
