import {
    chainIdToDataMap,
    chainIds,
    networkNames,
    networkNameToChainIdMap,
} from "./constants";
import { IConstructorFrameworkOptions } from "./interfaces";

export const validateFrameworkConstructorOptions = (
    options: IConstructorFrameworkOptions
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

export const getSubgraphQueriesEndpoint = (
    options: IConstructorFrameworkOptions
) => {
    return options.chainId
        ? chainIdToDataMap.get(options.chainId)!.subgraphAPIEndpoint
        : options.networkName
        ? chainIdToDataMap.get(
              networkNameToChainIdMap.get(options.networkName)!
          )!.subgraphAPIEndpoint
        : options.customSubgraphQueriesEndpoint;
};

/**
 * @dev We check that the user has input a networkName or chainId and that
 * they are both supported.
 * @param options
 * @returns
 */
export const getNetworkName = (options: IConstructorFrameworkOptions) => {
    return options.networkName || chainIdToDataMap.get(options.chainId!)!.name;
};
