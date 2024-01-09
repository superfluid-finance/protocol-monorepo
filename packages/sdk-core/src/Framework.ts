import { HardhatEthersHelpers } from "@nomiclabs/hardhat-ethers/types";
import { ethers } from "ethers";
import Web3 from "web3";

import BatchCall from "./BatchCall";
import ConstantFlowAgreementV1 from "./ConstantFlowAgreementV1";
import GeneralDistributionAgreementV1 from "./GeneralDistributionAgreementV1";
import Governance from "./Governance";
import Host from "./Host";
import InstantDistributionAgreementV1 from "./InstantDistributionAgreementV1";
import Operation, { BatchOperationType } from "./Operation";
import Query from "./Query";
import { SFError } from "./SFError";
import SuperToken, {
    NativeAssetSuperToken,
    PureSuperToken,
    WrapperSuperToken,
} from "./SuperToken";
import { chainIdToResolverDataMap, networkNameToChainIdMap } from "./constants";
import {
    getNetworkName,
    getSubgraphQueriesEndpoint,
    validateFrameworkConstructorOptions,
} from "./frameworkHelpers";
import { IConfig, IContracts, ISignerConstructorOptions } from "./interfaces";
import {
    Resolver,
    Resolver__factory,
    Superfluid__factory,
    SuperfluidLoader__factory,
} from "./typechain-types";
import { isEthersProvider, isInjectedWeb3 } from "./utils";

const V1 = "v1";

type SupportedProvider =
    | ethers.providers.Provider
    | HardhatEthersHelpers
    | Web3;

// TODO: add convenience function of utilizing provider (optional)
// instead of having to pass it in every single time
export interface IFrameworkOptions {
    chainId: number;
    customSubgraphQueriesEndpoint?: string;
    resolverAddress?: string;
    protocolReleaseVersion?: string;
    provider: SupportedProvider;
}

export interface IFrameworkSettings {
    chainId: number;
    customSubgraphQueriesEndpoint: string;
    networkName: string;
    protocolReleaseVersion: string;
    provider: ethers.providers.Provider;
    config: IConfig;
}

/**
 * Superfluid Framework Class
 * @description The entrypoint for the SDK-core, `create` an instance of this for full functionality.
 */
export default class Framework {
    readonly userInputOptions: IFrameworkOptions;
    settings: IFrameworkSettings;
    contracts: IContracts;

    cfaV1: ConstantFlowAgreementV1;
    governance: Governance;
    host: Host;
    idaV1: InstantDistributionAgreementV1;
    gdaV1: GeneralDistributionAgreementV1;
    query: Query;

    private constructor(
        options: IFrameworkOptions,
        settings: IFrameworkSettings
    ) {
        this.userInputOptions = options;
        this.settings = settings;

        this.cfaV1 = new ConstantFlowAgreementV1(
            settings.config.hostAddress,
            settings.config.cfaV1Address,
            settings.config.cfaV1ForwarderAddress
        );
        this.governance = new Governance(
            settings.config.hostAddress,
            settings.config.governanceAddress
        );
        this.host = new Host(settings.config.hostAddress);
        this.idaV1 = new InstantDistributionAgreementV1(
            settings.config.hostAddress,
            settings.config.idaV1Address
        );
        this.gdaV1 = new GeneralDistributionAgreementV1(
            settings.config.hostAddress,
            settings.config.gdaV1Address,
            settings.config.gdaV1ForwarderAddress
        );
        this.query = new Query(settings);
        const resolver = new ethers.Contract(
            settings.config.resolverAddress,
            Resolver__factory.abi
        ) as Resolver;

        this.contracts = {
            cfaV1: this.cfaV1.contract,
            governance: this.governance.contract,
            host: this.host.contract,
            idaV1: this.idaV1.contract,
            gdaV1: this.gdaV1.contract,
            resolver,
        };
    }

    /**
     * Creates the Framework object based on user provided `options`.
     * @param options.chainId the chainId of your desired network (e.g. 137 = matic)
     * @param options.customSubgraphQueriesEndpoint your custom subgraph endpoint
     * @param options.resolverAddress a custom resolver address (advanced use for testing)
     * @param options.protocolReleaseVersion a custom release version (advanced use for testing)
     * @param options.provider a provider object (injected web3, injected ethers, ethers provider) necessary for initializing the framework
     * @returns `Framework` class
     */
    static create = async (options: IFrameworkOptions) => {
        validateFrameworkConstructorOptions({
            ...options,
            protocolReleaseVersion: options.protocolReleaseVersion || V1,
        });

        const networkName = getNetworkName(options);
        const chainId =
            options.chainId || networkNameToChainIdMap.get(networkName)!;
        const releaseVersion = options.protocolReleaseVersion || V1;

        const customSubgraphQueriesEndpoint =
            options.customSubgraphQueriesEndpoint ||
            getSubgraphQueriesEndpoint(options);

        const provider = isEthersProvider(options.provider)
            ? options.provider
            : isInjectedWeb3(options.provider)
            ? // must explicitly cast web3 provider type because
              // ethers.providers.Web3Provider doesn't like
              // the type passed.
              new ethers.providers.Web3Provider(
                  options.provider.currentProvider as
                      | ethers.providers.ExternalProvider
                      | ethers.providers.JsonRpcFetchFunc
              )
            : options.provider.provider;

        const network = await provider.getNetwork();
        if (network.chainId !== chainId && chainId != null) {
            throw new SFError({
                type: "NETWORK_MISMATCH",
                message:
                    "Your provider network chainId is: " +
                    network.chainId +
                    " whereas your desired chainId is: " +
                    chainId,
            });
        }

        try {
            const networkData = chainIdToResolverDataMap.get(chainId);
            const resolverAddress = options.resolverAddress
                ? options.resolverAddress
                : networkData
                ? networkData.addresses.resolver
                : ethers.constants.AddressZero;
            const resolver = Resolver__factory.connect(
                resolverAddress,
                provider
            );
            const baseSettings = {
                chainId,
                customSubgraphQueriesEndpoint,
                protocolReleaseVersion: options.protocolReleaseVersion || V1,
                provider,
                networkName,
            };

            // supported networks scenario
            if (networkData && baseSettings.protocolReleaseVersion === V1) {
                const governanceAddress = networkData.addresses.governance
                    ? networkData.addresses.governance
                    : await Superfluid__factory.connect(
                          networkData.addresses.host,
                          provider
                      ).getGovernance();

                const settings: IFrameworkSettings = {
                    ...baseSettings,
                    config: {
                        resolverAddress,
                        hostAddress: networkData.addresses.host,
                        cfaV1Address: networkData.addresses.cfaV1,
                        idaV1Address: networkData.addresses.idaV1,
                        // @note TODO - remove the any once you add gdaV1 and gdaV1Forwarder to metadata
                        // add idaV1Forwarder to metadata as well
                        gdaV1Address:
                            (networkData.addresses as any).gdaV1 ||
                            networkData.addresses.idaV1,
                        gdaV1ForwarderAddress:
                            (networkData.addresses as any).gdaV1Forwarder ||
                            networkData.addresses.idaV1,
                        governanceAddress,
                        cfaV1ForwarderAddress:
                            networkData.addresses.cfaV1Forwarder,
                    },
                };

                return new Framework(options, settings);
            } else {
                // unsupported networks scenario (e.g. local testing)
                const superfluidLoaderAddress = await resolver.get(
                    "SuperfluidLoader-v1"
                );
                const cfaV1ForwarderAddress =
                    await resolver.get("CFAv1Forwarder");
                const gdaV1ForwarderAddress =
                    await resolver.get("GDAv1Forwarder");
                const superfluidLoader = SuperfluidLoader__factory.connect(
                    superfluidLoaderAddress,
                    provider
                );

                const framework =
                    await superfluidLoader.loadFramework(releaseVersion);
                const governanceAddress = await Superfluid__factory.connect(
                    framework.superfluid,
                    provider
                ).getGovernance();

                const settings: IFrameworkSettings = {
                    ...baseSettings,
                    config: {
                        resolverAddress,
                        hostAddress: framework.superfluid,
                        cfaV1Address: framework.agreementCFAv1,
                        idaV1Address: framework.agreementIDAv1,
                        gdaV1Address: framework.agreementGDAv1,
                        governanceAddress,
                        cfaV1ForwarderAddress,
                        gdaV1ForwarderAddress,
                    },
                };

                return new Framework(options, settings);
            }
        } catch (err) {
            throw new SFError({
                type: "FRAMEWORK_INITIALIZATION",
                message: "There was an error initializing the framework",
                cause: err,
            });
        }
    };

    /**
     * Create a signer which can be used to sign transactions.
     * @param options.web3Provider a Web3Provider object (e.g. client side - metamask, web3modal)
     * @param options.provider an ethers Provider object (e.g. via Hardhat ethers)
     * @param options.privateKey a test account private key
     * @param options.signer a signer object (e.g. ethers.Wallet instance)
     * @returns `ethers.Signer` object
     */
    createSigner = (options: ISignerConstructorOptions): ethers.Signer => {
        if (
            !options.privateKey &&
            !options.provider &&
            !options.signer &&
            !options.web3Provider
        ) {
            throw new SFError({
                type: "CREATE_SIGNER",
                message: "You must pass in a private key, provider or signer.",
            });
        }

        /* istanbul ignore else  */
        if (options.privateKey) {
            if (!options.provider) {
                throw new SFError({
                    type: "CREATE_SIGNER",
                    message:
                        "You must pass in a provider with your private key.",
                });
            }
            return new ethers.Wallet(options.privateKey, options.provider);
        } else if (options.signer) {
            return options.signer;
        }
        // NOTE: tested by sdk-redux already
        else if (options.web3Provider) {
            return options.web3Provider.getSigner();
        }

        /* istanbul ignore next */
        throw new SFError({
            type: "CREATE_SIGNER",
            message: "Something went wrong, this should never occur.",
        });
    };

    /**
     * Create a `BatchCall` class from the `Framework`.
     * @param operations the list of operations to execute
     * @returns `BatchCall` class
     */
    batchCall = (operations: Operation[]) => {
        return new BatchCall({
            operations,
            hostAddress: this.settings.config.hostAddress,
        });
    };

    /**
     * Create an `Operation` class from the `Framework`.
     * @param txn the populated transaction to execute
     * @param type the operation type
     * @returns `Operation` class
     */
    operation = (
        txn: Promise<ethers.PopulatedTransaction>,
        type: BatchOperationType
    ) => {
        return new Operation(txn, type);
    };

    /**
     * Loads `NativeAssetSuperToken` class from the `Framework`. Will throw if token is not NativeAssetSuperToken.
     * @param tokenAddressOrSymbol
     * @returns `NativeAssetSuperToken` class
     */
    loadNativeAssetSuperToken = async (
        tokenAddressOrSymbol: string
    ): Promise<NativeAssetSuperToken> => {
        const superToken = await this.loadSuperToken(tokenAddressOrSymbol);

        // The NativeAssetSuperToken class should have the nativeTokenSymbol property
        const isNativeAssetSuperToken = !!(superToken as any).nativeTokenSymbol;

        if (!isNativeAssetSuperToken) {
            throw new SFError({
                type: "SUPERTOKEN_INITIALIZATION",
                message: "The token is not a native asset supertoken.",
            });
        }
        return superToken as NativeAssetSuperToken;
    };

    /**
     * Loads `PureSuperToken` class from the `Framework`. Will throw if token is not PureSuperToken.
     * @param tokenAddressOrSymbol
     * @returns `PureSuperToken` class
     */
    loadPureSuperToken = async (
        tokenAddressOrSymbol: string
    ): Promise<PureSuperToken> => {
        const superToken = await this.loadSuperToken(tokenAddressOrSymbol);

        // The PureSuperToken class should not have the downgrade (and upgrade) function
        // we can just check if downgrade doesn't exist
        const isPureSuperToken = !!(superToken as any).downgrade === false;
        if (!isPureSuperToken) {
            throw new SFError({
                type: "SUPERTOKEN_INITIALIZATION",
                message: "The token is not a pure supertoken.",
            });
        }
        return superToken as PureSuperToken;
    };

    /**
     * Loads `WrapperSuperToken` class from the `Framework`. Will throw if token is not WrapperSuperToken.
     * @param tokenAddressOrSymbol
     * @returns `WrapperSuperToken` class
     */
    loadWrapperSuperToken = async (
        tokenAddressOrSymbol: string
    ): Promise<WrapperSuperToken> => {
        const superToken = await this.loadSuperToken(tokenAddressOrSymbol);

        // The WrapperSuperToken class should have the underlyingToken property
        const isWrapperSuperToken = !!(superToken as any).underlyingToken;
        if (!isWrapperSuperToken) {
            throw new SFError({
                type: "SUPERTOKEN_INITIALIZATION",
                message: "The token is not a wrapper supertoken.",
            });
        }
        return superToken as WrapperSuperToken;
    };

    /**
     * Loads `SuperToken` class from the `Framework`. Use this when you're unsure of the token type.
     * @param tokenAddressOrSymbol the `SuperToken` address or symbol (if symbol, it must be on the resolver)
     * @returns `SuperToken` class
     */
    loadSuperToken = async (
        tokenAddressOrSymbol: string
    ): Promise<SuperToken> => {
        const address = await this._tryGetTokenAddress(tokenAddressOrSymbol);
        return await SuperToken.create({
            ...this.settings,
            address,
        });
    };

    /**
     * Try to get the token address given an address (returns if valid) or the token symbol via the resolver.
     * @param tokenAddressOrSymbol
     * @returns token address
     */
    private _tryGetTokenAddress = async (
        tokenAddressOrSymbol: string
    ): Promise<string> => {
        const isInputValidAddress =
            ethers.utils.isAddress(tokenAddressOrSymbol);

        if (isInputValidAddress) {
            return tokenAddressOrSymbol;
        } else {
            try {
                const superTokenKey =
                    "supertokens." +
                    this.settings.protocolReleaseVersion +
                    "." +
                    tokenAddressOrSymbol;

                const resolver = Resolver__factory.connect(
                    this.settings.config.resolverAddress,
                    this.settings.provider
                );
                return await resolver.get(superTokenKey);
            } catch (err) {
                throw new SFError({
                    type: "SUPERTOKEN_INITIALIZATION",
                    message:
                        "There was an error with loading the SuperToken with symbol: " +
                        tokenAddressOrSymbol +
                        " with the resolver.",
                    cause: err,
                });
            }
        }
    };
}
