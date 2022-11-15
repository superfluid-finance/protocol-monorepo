import { IFrameworkOptions } from "./Framework";
import { SFError } from "./SFError";
import {
    chainIds,
    chainIdToResolverDataMap,
    networkNames,
    networkNameToChainIdMap,
} from "./constants";
import { isNullOrEmpty } from "./utils";

export const validateFrameworkConstructorOptions = (
    options: IFrameworkOptions
) => {
    if (!options.chainId && !options.networkName) {
        throw new SFError({
            type: "FRAMEWORK_INITIALIZATION",
            customMessage: "You must input chainId or networkName.",
        });
    }
    // if the user inputs a custom network (local, they have to specify this)
    if (
        options.networkName &&
        networkNames.includes(options.networkName) &&
        options.chainId &&
        chainIds.includes(options.chainId) &&
        networkNameToChainIdMap.get(options.networkName) !== options.chainId
    ) {
        throw new SFError({
            type: "FRAMEWORK_INITIALIZATION",
            customMessage:
                "The network name and chainId you have selected don't match.",
        });
    }

    if (!options.provider) {
        throw new SFError({
            type: "FRAMEWORK_INITIALIZATION",
            customMessage:
                "You must pass in a provider, an injected web3.js or ethers.js instance when initializing the framework.",
        });
    }

    // if the user inputs a networkName or chainId that isn't part of the resolver
    // that is, an unsupported network/chain
    if (
        (options.networkName != null &&
            !networkNames.includes(options.networkName)) ||
        (options.chainId != null && !chainIds.includes(options.chainId))
    ) {
        if (
            options.dataMode !== "WEB3_ONLY" &&
            isNullOrEmpty(options.customSubgraphQueriesEndpoint)
        ) {
            throw new SFError({
                type: "FRAMEWORK_INITIALIZATION",
                customMessage:
                    "You must input your own custom subgraph query endpoint if you use an unsupported network with dataMode set to SUBGRAPH_ONLY or SUBGRAPH_WEB3.",
            });
        }
        if (isNullOrEmpty(options.resolverAddress)) {
            throw new SFError({
                type: "FRAMEWORK_INITIALIZATION",
                customMessage:
                    "You must input your own resolver address if you use an unsupported network.",
            });
        }
    }
};

/**
 * @dev options.networkName is casted as not null as we check to ensure chainId or networkName is not null.
 * @param options
 * @returns SubgraphQueriesEndpoint which is a custom endpoint or based on selected network
 */
export const getSubgraphQueriesEndpoint = (options: IFrameworkOptions) => {
    const chainId = options.networkName
        ? networkNameToChainIdMap.get(options.networkName)
        : options.chainId;
    const resolverData = chainId ? chainIdToResolverDataMap.get(chainId) : null;
    if (resolverData) {
        return resolverData.subgraphAPIEndpoint;
    }

    /* istanbul ignore next */
    throw new SFError({
        type: "FRAMEWORK_INITIALIZATION",
        customMessage: "Something went wrong, this should never occur.",
    });
};

interface INetworkNameParams {
    readonly chainId?: number;
    readonly networkName?: string;
}

/**
 * @dev We check that the user has input a networkName or chainId and that they are both supported.
 * @param options.chainId the chainId of the desired network
 * @param options.networkName the name of the desired network
 * @returns the network name
 */
export const getNetworkName = (options: INetworkNameParams): string => {
    return (
        options.networkName ||
        (options.chainId
            ? chainIdToResolverDataMap.get(options.chainId)?.networkName
            : undefined) ||
        "custom"
    );
};
