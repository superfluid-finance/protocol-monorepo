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
import { IConfig, ISignerConstructorOptions } from "./interfaces";
import { handleError } from "./errorHelper";
import ConstantFlowAgreementV1 from "./ConstantFlowAgreementV1";
import InstantDistributionAgreementV1 from "./InstantDistributionAgreementV1";

export interface IFrameworkOptions {
    chainId?: ChainId;
    customSubgraphQueriesEndpoint?: string;
    dataMode?: DataMode;
    networkName?: NetworkName;
    resolverAddress?: string;
    protocolReleaseVersion?: string;
    provider: ethers.providers.Provider;
}

export interface IFrameworkSettings {
    chainId: ChainId;
    customSubgraphQueriesEndpoint: string;
    dataMode: DataMode;
    networkName: NetworkName;
    protocolReleaseVersion: string;
    provider: ethers.providers.Provider;
    config: IConfig;
}

/**
 * @dev Superfluid Framework class
 */
export default class Framework {
    readonly userInputOptions: IFrameworkOptions;
    settings: IFrameworkSettings;

    query: Query;
    cfaV1: ConstantFlowAgreementV1;
    idaV1: InstantDistributionAgreementV1;

    private constructor(
        options: IFrameworkOptions,
        settings: IFrameworkSettings
    ) {
        this.userInputOptions = options;
        this.settings = settings;

        this.query = new Query(this.settings);
        this.cfaV1 = new ConstantFlowAgreementV1({
            config: this.settings.config,
        });
        this.idaV1 = new InstantDistributionAgreementV1({
            config: this.settings.config,
        });
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
            options.provider
        ) as IResolver;

        try {
            const superfluidLoaderAddress = await resolver.get(
                "SuperfluidLoader-v1"
            );
            const superfluidLoader = new ethers.Contract(
                superfluidLoaderAddress,
                SuperfluidLoaderABI,
                options.provider
            ) as SuperfluidLoader;

            const framework = await superfluidLoader.loadFramework(
                releaseVersion
            );
            const customSubgraphQueriesEndpoint =
                options.customSubgraphQueriesEndpoint ||
                getSubgraphQueriesEndpoint(options);

            if (customSubgraphQueriesEndpoint == null) {
                return handleError(
                    "FRAMEWORK_INITIALIZATION",
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
                provider: options.provider,
                networkName,
                config: {
                    hostAddress: framework.superfluid,
                    superTokenFactoryAddress: framework.superTokenFactory,
                    cfaV1Address: framework.agreementCFAv1,
                    idaV1Address: framework.agreementIDAv1,
                },
            };

            return new Framework(options, settings);
        } catch (err) {
            return handleError(
                "FRAMEWORK_INITIALIZATION",
                "There was an error initializing the framework",
                JSON.stringify(err)
            );
        }
    };

    createSigner = (options: ISignerConstructorOptions): Signer => {
        if (!options.privateKey && !options.provider && !options.signer) {
            return handleError(
                "CREATE_SIGNER",
                "You must pass in a private key, provider or signer."
            );
        }
        if (options.privateKey) {
            if (!options.provider) {
                return handleError(
                    "CREATE_SIGNER",
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

        return handleError(
            "CREATE_SIGNER",
            "Something went wrong, this should never occur."
        );
    };

    loadSuperToken = (address: string): SuperToken => {
        return new SuperToken({ ...this.settings, address });
    };
}
