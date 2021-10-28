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
    if (options.chainId != null && !chainIds.includes(options.chainId)) {
        throw new Error("You have selected an unsupported chain id.");
    }
    if (
        options.networkName != null &&
        !networkNames.includes(options.networkName)
    ) {
        throw new Error("You have selected an unsupported network name.");
    }
    if (
        options.networkName &&
        options.chainId &&
        networkNameToChainIdMap.get(options.networkName) !== options.chainId
    ) {
        throw new Error(
            "The network name and chainId you have selected don't match."
        );
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

/**
 * @dev We check that the user has input a networkName or chainId and that
 * they are both supported.
 * @param options
 * @returns
 */
export const getNetworkName = (options: IFrameworkOptions): NetworkName => {
    return options.networkName || chainIdToDataMap.get(options.chainId!)!.name;
};
