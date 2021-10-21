import {
    chainIdToDataMap,
    chainIds,
    networkNames,
    networkNameToChainIdMap,
} from "./constants";
import { IFrameworkOptions } from "./interfaces";

export const validateFrameworkConstructorOptions = (
    options: IFrameworkOptions
) => {
    if (options.chainId == null && options.networkName == null) {
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

export const getSubgraphQueriesEndpoint = (options: IFrameworkOptions) => {
    return options.chainId
        ? chainIdToDataMap.get(options.chainId)!.subgraphAPIEndpoint
        : options.networkName
        ? chainIdToDataMap.get(
              networkNameToChainIdMap.get(options.networkName)!
          )!.subgraphAPIEndpoint
        : options.customSubgraphQueriesEndpoint;
};

export const getNetworkName = (options: IFrameworkOptions) => {
    return options.networkName || chainIdToDataMap.get(options.chainId!)!.name;
};
