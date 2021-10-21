import { IFrameworkOptions } from "./interfaces";
import { DataMode } from ".";
import {
    getNetworkName,
    getSubgraphQueriesEndpoint,
    validateFrameworkConstructorOptions,
} from "./frameworkHelpers";
import Query from "./Query";

export default class Framework {
    options: IFrameworkOptions;
    query: Query;

    constructor(options: IFrameworkOptions) {
        validateFrameworkConstructorOptions(options);
        const customSubgraphQueriesEndpoint =
            getSubgraphQueriesEndpoint(options);

        if (customSubgraphQueriesEndpoint == null) {
            throw new Error("You cannot have a null subgaphQueriesEndpoint.");
        }

        const networkName = getNetworkName(options);

        this.options = {
            chainId: options.chainId,
            customSubgraphQueriesEndpoint,
            dataMode: options.dataMode || DataMode.SUBGRAPH_WEB3,
            protocolReleaseVersion: options.protocolReleaseVersion || "v1",
            networkName,
        };

        this.query = new Query(customSubgraphQueriesEndpoint);
    }
}
