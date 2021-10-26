import { IConstructorFrameworkOptions, IFrameworkOptions } from "./interfaces";
import { DataMode } from "./index";
import {
    getNetworkName,
    getSubgraphQueriesEndpoint,
    validateFrameworkConstructorOptions,
} from "./frameworkHelpers";
import Query from "./Query";
import { networkNameToChainIdMap } from "./constants";

export default class Framework {
    options: IFrameworkOptions;
    query: Query;

    constructor(options: IConstructorFrameworkOptions) {
        validateFrameworkConstructorOptions(options);
        const customSubgraphQueriesEndpoint =
            getSubgraphQueriesEndpoint(options);

        if (customSubgraphQueriesEndpoint == null) {
            throw new Error("You cannot have a null subgaphQueriesEndpoint.");
        }

        const networkName = getNetworkName(options);

        this.options = {
            chainId:
                options.chainId || networkNameToChainIdMap.get(networkName)!,
            customSubgraphQueriesEndpoint,
            dataMode: options.dataMode || DataMode.SUBGRAPH_WEB3,
            protocolReleaseVersion: options.protocolReleaseVersion || "v1",
            networkName,
        };

        this.query = new Query(this.options);
    }
}
