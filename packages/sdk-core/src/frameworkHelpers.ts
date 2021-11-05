import { isNullOrEmpty } from "./utils";
import {
    chainIdToDataMap,
    chainIds,
    networkNames,
    networkNameToChainIdMap,
} from "./constants";
import { IFrameworkOptions } from "./Framework";
import { ChainId, NetworkName } from "./types";
import { handleError } from "./errorHelper";

export const validateFrameworkConstructorOptions = (
    options: IFrameworkOptions
) => {
    if (!options.chainId && !options.networkName) {
        handleError(
            "FRAMEWORK_INITIALIZATION",
            "You must input chainId or networkName."
        );
    }
    // if the user inputs a custom network (local, they have to specify this)
    if (
        options.networkName &&
        networkNames.includes(options.networkName) &&
        options.chainId &&
        chainIds.includes(options.chainId) &&
        networkNameToChainIdMap.get(options.networkName) !== options.chainId
    ) {
        handleError(
            "FRAMEWORK_INITIALIZATION",
            "The network name and chainId you have selected don't match."
        );
    }

    if (!options.provider) {
        handleError(
            "FRAMEWORK_INITIALIZATION",
            "You must pass in a provider when initializing the framework."
        );
    }

    // if the user inputs a networkName or chainId that isn't part of the resolver
    if (
        (options.networkName != null &&
            !networkNames.includes(options.networkName)) ||
        (options.chainId != null && !chainIds.includes(options.chainId))
    ) {
        if (
            (options.dataMode === "SUBGRAPH_ONLY" ||
                options.dataMode === "SUBGRAPH_WEB3") &&
            isNullOrEmpty(options.customSubgraphQueriesEndpoint)
        ) {
            handleError(
                "FRAMEWORK_INITIALIZATION",
                "You must input your own custom subgraph query endpoint if you use an unsupported network with dataMode set to SUBGRAPH_ONLY or SUBGRAPH_WEB3."
            );
        }
        if (isNullOrEmpty(options.resolverAddress)) {
            handleError(
                "FRAMEWORK_INITIALIZATION",
                "You must input your own resolver address if you use an unsupported network with dataMode set to SUBGRAPH_ONLY or SUBGRAPH_WEB3."
            );
        }
    }
};

/**
 * @dev options.networkName is casted as not null as we check
 * to ensure at least one of the settings are not null.
 * @param options
 * @returns
 */
export const getSubgraphQueriesEndpoint = (options: IFrameworkOptions) => {
    return options.customSubgraphQueriesEndpoint != null
        ? options.customSubgraphQueriesEndpoint
        : options.chainId
        ? chainIdToDataMap.get(options.chainId)!.subgraphAPIEndpoint
        : chainIdToDataMap.get(
              networkNameToChainIdMap.get(options.networkName!)!
          )!.subgraphAPIEndpoint;
};

interface INetworkNameParams {
    readonly chainId?: ChainId;
    readonly networkName?: NetworkName;
}

/**
 * @dev We check that the user has input a networkName or chainId and that
 * they are both supported.
 * @param options
 * @returns
 */
export const getNetworkName = (options: INetworkNameParams): NetworkName => {
    const networkName =
        chainIdToDataMap.get(options.chainId!) != null
            ? chainIdToDataMap.get(options.chainId!)!.networkName
            : "custom";

    return options.networkName || networkName;
};
