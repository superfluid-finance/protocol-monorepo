import { ethers } from "ethers";
import { Signer } from "@ethersproject/abstract-signer";
import { ChainId, DataMode, NetworkName } from "./interfaces";
import { abi as IResolverABI } from "./abi/IResolver.json";
import { abi as SuperfluidLoaderABI } from "./abi/SuperfluidLoader.json";
import {
    getNetworkName,
    getSubgraphQueriesEndpoint,
    validateFrameworkConstructorOptions,
} from "./frameworkHelpers";
import Query from "./Query";
import { chainIdToDataMap, networkNameToChainIdMap } from "./constants";
import SuperToken from "./SuperToken";
import { IResolver, SuperfluidLoader } from "./typechain";

// TODO: do not commit typechain, have the build handle
// generating the types and publishing it with the types
export interface IFrameworkOptions {
    chainId?: ChainId;
    customSubgraphQueriesEndpoint?: string;
    dataMode?: DataMode;
    networkName?: NetworkName;
    resolverAddress?: string;
    protocolReleaseVersion?: string;
    providerOrSigner?: ethers.providers.Provider | ethers.Signer;
}

export interface IFrameworkSettings {
    chainId: ChainId;
    customSubgraphQueriesEndpoint: string;
    dataMode: DataMode;
    networkName: NetworkName;
    protocolReleaseVersion: string;
    providerOrSigner?: ethers.providers.Provider | ethers.Signer;
    config: IConfig;
}

export interface ISignerConstructorOptions {
    readonly web3Provider?: ethers.providers.Web3Provider; // Web3Provider (client side - metamask, web3modal)
    readonly provider?: ethers.providers.Provider; // Provider
    readonly privateKey?: string; // private key (best to store a test account PK in .env file)
    readonly signer?: ethers.Signer; // ethers.Wallet
}

export interface IConfig {
    readonly hostAddress: string;
    readonly superTokenFactoryAddress: string;
    readonly cfaV1Address: string;
    readonly idaV1Address: string;
}

// TODO: figure out how to expose the comments.
/**
 * @dev Framework class which allows you to do a bunch of cool stuff.
 */
export default class Framework {
    readonly userInputOptions: IFrameworkOptions;
    settings: IFrameworkSettings;

    query: Query;

    private constructor(
        options: IFrameworkOptions,
        settings: IFrameworkSettings
    ) {
        this.userInputOptions = options;
        this.settings = settings;

        this.query = new Query(this.settings);
    }

    static create = async (options: IFrameworkOptions) => {
        validateFrameworkConstructorOptions(options);

        const networkName = getNetworkName(options);
        const data = chainIdToDataMap.get(
            options.chainId || networkNameToChainIdMap.get(networkName)!
        );
        const releaseVersion = options.protocolReleaseVersion || "v1";

        const resolverAddress =
            options.resolverAddress || data != null
                ? data!.resolverAddress
                : "";
        const resolver = new ethers.Contract(
            resolverAddress,
            IResolverABI,
            options.providerOrSigner
        ) as IResolver;

        try {
            const superfluidLoaderAddress = await resolver.get(
                "SuperfluidLoader-v1"
            );
            const superfluidLoader = new ethers.Contract(
                superfluidLoaderAddress,
                SuperfluidLoaderABI,
                options.providerOrSigner
            ) as SuperfluidLoader;

            const framework = await superfluidLoader.loadFramework(
                releaseVersion
            );
            const customSubgraphQueriesEndpoint =
                options.customSubgraphQueriesEndpoint ||
                getSubgraphQueriesEndpoint(options);

            if (customSubgraphQueriesEndpoint == null) {
                throw new Error(
                    "You cannot have a null subgaphQueriesEndpoint."
                );
            }

            const settings: IFrameworkSettings = {
                chainId:
                    options.chainId ||
                    networkNameToChainIdMap.get(networkName)!,
                customSubgraphQueriesEndpoint,
                dataMode: options.dataMode || "SUBGRAPH_ONLY",
                protocolReleaseVersion: options.protocolReleaseVersion || "v1",
                providerOrSigner: options.providerOrSigner,
                networkName,
                config: {
                    hostAddress: framework.superfluid,
                    superTokenFactoryAddress: framework.superTokenFactory,
                    cfaV1Address: framework.agreementCFAv1,
                    idaV1Address: framework.agreementIDAv1,
                },
            };
            console.log("settings", settings);

            // TODO: load resolver and get all the necessary data from that there and pass it into the constructor
            return new Framework(options, settings);
        } catch (error) {
            throw new Error(JSON.stringify(error));
        }
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
