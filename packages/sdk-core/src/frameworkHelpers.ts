import { isNullOrEmpty } from "./utils";
import {
    chainIdToDataMap,
    chainIds,
    networkNames,
    networkNameToChainIdMap,
} from "./constants";
import { IFrameworkOptions } from "./Framework";
import { NetworkName } from "./interfaces";

export const validateFrameworkConstructorOptions = (
    options: IFrameworkOptions
) => {
    if (!options.chainId && !options.networkName) {
        throw new Error("You must input chainId or networkName.");
    }
    // if the user inputs a custom network (local, they have to specify this)
    if (
        options.networkName &&
        networkNames.includes(options.networkName) &&
        options.chainId &&
        chainIds.includes(options.chainId) &&
        networkNameToChainIdMap.get(options.networkName) !== options.chainId
    ) {
        throw new Error(
            "The network name and chainId you have selected don't match."
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
            throw new Error(
                "You must input your own custom subgraph query endpoint if you use an unsupported network with dataMode set to SUBGRAPH_ONLY or SUBGRAPH_WEB3."
            );
        }
        if (isNullOrEmpty(options.resolverAddress)) {
            throw new Error(
                "You must input your own resolver address if you use an unsupported network with dataMode set to SUBGRAPH_ONLY or SUBGRAPH_WEB3."
            );
        }
    }
    // TODO: you have to input a provider if you change from the default: (SUBGRAPH_ONLY)
    // TODO: if you are inputting a networkName and or chainId that is not part of
    // the resolver:
    // - you have to input your own subgraph url if you select (SUBGRAPH_ONLY or SUBGRAPH_WEB)
    // - you have to input your own resolverAddress
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

/**
 * @dev We check that the user has input a networkName or chainId and that
 * they are both supported.
 * @param options
 * @returns
 */
export const getNetworkName = (options: IFrameworkOptions): NetworkName => {
    const networkName =
        chainIdToDataMap.get(options.chainId!) != null
            ? chainIdToDataMap.get(options.chainId!)!.networkName
            : "custom";

    return options.networkName || networkName;
};
