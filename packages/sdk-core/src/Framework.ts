import { ChainId, DataMode, NetworkName } from "./index";
import {
    getNetworkName,
    getSubgraphQueriesEndpoint,
    validateFrameworkConstructorOptions,
} from "./frameworkHelpers";
import Query from "./Query";
import { networkNameToChainIdMap } from "./constants";
import SuperToken from "./SuperToken";

export interface IConstructorFrameworkOptions {
    chainId?: ChainId;
    customSubgraphQueriesEndpoint?: string;
    dataMode?: DataMode;
    networkName?: NetworkName;
    protocolReleaseVersion?: string;
}

export interface IFrameworkOptions {
    chainId: ChainId;
    customSubgraphQueriesEndpoint: string;
    dataMode: DataMode;
    networkName: NetworkName;
    protocolReleaseVersion: string;
}

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

	// initializes the framework to query the correct resolver contract
	// which will get the host contract and the agreement contract addresses
 	initialize = async () => {

	}

    // TODO: do we only want to take address or should we give users
    // the option to pass in one of a few types of
    loadSuperToken = (address: string): SuperToken => {
        return new SuperToken({ ...this.options, address });
    };
}
