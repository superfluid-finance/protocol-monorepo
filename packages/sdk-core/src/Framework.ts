import { ethers } from "ethers";
import { Signer } from "@ethersproject/abstract-signer";
import { ChainId, DataMode, NetworkName } from "./interfaces";
import {
    getNetworkName,
    getSubgraphQueriesEndpoint,
    validateFrameworkConstructorOptions,
} from "./frameworkHelpers";
import Query from "./Query";
import { networkNameToChainIdMap } from "./constants";
import SuperToken from "./SuperToken";

export interface IFrameworkOptions {
    chainId?: ChainId;
    customSubgraphQueriesEndpoint?: string;
    dataMode?: DataMode;
    networkName?: NetworkName;
    protocolReleaseVersion?: string;
}

export interface IFrameworkSettings {
    chainId: ChainId;
    customSubgraphQueriesEndpoint: string;
    dataMode: DataMode;
    networkName: NetworkName;
    protocolReleaseVersion: string;
}

export interface ISignerConstructorOptions {
    readonly web3Provider?: ethers.providers.Web3Provider; // Web3Provider (client side - metamask, web3modal)
    readonly provider?: ethers.providers.Provider; // Provider
    readonly privateKey?: string; // private key (best to store a test account PK in .env file)
    readonly signer?: ethers.Signer; // ethers.Wallet
}

export interface IConfig {
    readonly hostAddress: string;
    readonly cfaV1Address: string;
    readonly idaV1Address: string;
}

/**
 * @dev Framework class which allows you to do a bunch of cool stuff.
 */
export default class Framework {
    readonly userInputOptions: IFrameworkOptions;
    settings: IFrameworkSettings;
    // TODO: userInputOptions is readonly and passed in to settings at constructor
    // TODO: add settings - initialize settings in another step

    query: Query;

    private constructor(options: IFrameworkOptions) {
        this.userInputOptions = options;
        const networkName = getNetworkName(options);

        const customSubgraphQueriesEndpoint =
            getSubgraphQueriesEndpoint(options);

        if (customSubgraphQueriesEndpoint == null) {
            throw new Error("You cannot have a null subgaphQueriesEndpoint.");
        }
        // allow passing in provider
        this.settings = {
            chainId:
                options.chainId || networkNameToChainIdMap.get(networkName)!,
            customSubgraphQueriesEndpoint,
            dataMode: options.dataMode || "SUBGRAPH_WEB3", // this should default to SUBGRAPH, if subgraph_web3, must pass provider
            protocolReleaseVersion: options.protocolReleaseVersion || "v1",
            networkName,
        };

        this.query = new Query(this.settings);
    }

    static create = async (options: IFrameworkOptions) => {
        validateFrameworkConstructorOptions(options);
        // TODO: load resolver and get all the necessary data from that there and pass it into the constructor
        return new Framework(options);
    };

    createSigner = (options: ISignerConstructorOptions): Signer => {
        if (!options.privateKey && !options.provider && !options.signer) {
            throw new Error(
                "You must pass in a private key, provider or signer."
            );
        }
        if (options.privateKey) {
            if (!options.provider) {
                throw new Error(
                    "You must pass in a provider with your private key."
                );
            }
            return new ethers.Wallet(options.privateKey, options.provider);
        }

        if (options.signer) {
            return options.signer;
        }

        if (options.web3Provider) {
            return options.web3Provider.getSigner();
        }

        throw new Error("Something went wrong, this should never occur.");
    };

    // TODO: will be part of create now
    // initializes the framework to query the correct resolver contract
    // which will get the host contract and the agreement contract addresses
    initialize = async () => {};

    // TODO: load up the config file to get all the different addresses from the different networks
    loadConfig = async () => {};

    // TODO: do we only want to take address or should we give users
    // the option to pass in one of a few types of
    loadSuperToken = (address: string): SuperToken => {
        return new SuperToken({ ...this.userInputOptions, address });
    };
}
