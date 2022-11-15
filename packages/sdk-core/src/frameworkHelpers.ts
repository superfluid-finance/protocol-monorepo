import { IFrameworkOptions } from "./Framework";
import { SFError } from "./SFError";
import { chainIds, chainIdToResolverDataMap } from "./constants";
import { isNullOrEmpty } from "./utils";

export const validateFrameworkConstructorOptions = (
    options: IFrameworkOptions
) => {
    if (!options.chainId) {
        throw new SFError({
            type: "FRAMEWORK_INITIALIZATION",
            message: "You must input chainId.",
        });
    }

    if (!options.provider) {
        throw new SFError({
            type: "FRAMEWORK_INITIALIZATION",
            message:
                "You must pass in a provider, an injected web3.js or ethers.js instance when initializing the framework.",
        });
    }

    // if the user inputs a chainId that isn't part of the resolver
    // that is, an unsupported network/chain
    if (options.chainId != null && !chainIds.includes(options.chainId)) {
        if (isNullOrEmpty(options.resolverAddress)) {
            throw new SFError({
                type: "FRAMEWORK_INITIALIZATION",
                message:
                    "You must input your own resolver address if you use an unsupported network.",
            });
        }
    }
};

/**
 * Subgraph Query endpoint is empty string and will break at runtime if this is not handled.
 * @param options
 * @returns SubgraphQueriesEndpoint which is a custom endpoint or based on selected network
 */
export const getSubgraphQueriesEndpoint = (options: IFrameworkOptions) => {
    const resolverData = options.chainId
        ? chainIdToResolverDataMap.get(options.chainId)
        : null;
    if (resolverData) {
        return resolverData.subgraphAPIEndpoint;
    }

    return ""; // return empty string
};

interface INetworkNameParams {
    readonly chainId?: number;
}

/**
 * We check that the user has input a networkName or chainId and that they are both supported.
 * @param options.chainId the chainId of the desired network
 * @param options.networkName the name of the desired network
 * @returns the network name
 */
export const getNetworkName = (options: INetworkNameParams): string => {
    return (
        (options.chainId
            ? chainIdToResolverDataMap.get(options.chainId)?.networkName
            : undefined) || "custom"
    );
};
