import fs from "fs";
import request from "graphql-request";

interface IndexingStatusBlock {
    readonly number: string;
    readonly hash: string;
}

interface IndexingStatus {
    readonly chains: {
        chainHeadBlock: IndexingStatusBlock;
        latestBlock: IndexingStatusBlock;
    }[];
    readonly entityCount: string;
    readonly health: string;
    readonly synced: boolean;
}

const pendingIndexingStatusQuery = (subgraphName: string) =>
    `{
    response: indexingStatusForPendingVersion(subgraphName: "${subgraphName}") {
        chains {
            chainHeadBlock {
                number
                hash
            }
            latestBlock {
                number
                hash
            }
        }
        entityCount
        health
        synced
    }
}`;

const getNetworkNames = () => {
    const configPath = "./config";
    const files = fs.readdirSync(configPath);
    return files.map((x) => x.replace(".json", ""));
};

const formatIndexingStatus = (data: IndexingStatus) => {
    const formattedData = {
        entityCount: data.entityCount,
        synced: data.synced,
        health: data.health,
        latestBlock: data.chains[0].latestBlock,
        chainHeadBlock: data.chains[0].chainHeadBlock,
    };
    return formattedData;
};

const subgraphRequest = async <T>(query: string): Promise<T> => {
    try {
        const response = await request<T>(
            "https://api.thegraph.com/index-node/graphql",
            query
        );

        return response;
    } catch (err) {
        throw new Error(
            `Failed call to subgraph with query ${query} and error ${err}`
        );
    }
};
const executeIndexingStatusQuery = async (networkName: string) => {
    const subgraphName =
        "superfluid-finance/protocol-" +
        process.env.SUBGRAPH_RELEASE_TAG +
        "-" +
        networkName;
    const query = pendingIndexingStatusQuery(subgraphName);
    const data = await subgraphRequest<{ response: IndexingStatus }>(query);
    if (data.response != null) {
        const formattedData = formatIndexingStatus(data.response);
        return { ...formattedData, subgraphName };
    }
};

const main = async () => {
    const networkNames = getNetworkNames();
    const promises = networkNames.map((x) => executeIndexingStatusQuery(x));
    const resolvedPromises = await Promise.all(promises);
    if (resolvedPromises.some((x) => x != null)) {
        console.log(
            "There are still subgraphs that are indexing for the following",
            process.env.SUBGRAPH_RELEASE_TAG,
            "release endpoint(s):"
        );
        for (let i = 0; i < resolvedPromises.length; i++) {
            const resolvedPromise = resolvedPromises[i];
            if (resolvedPromise != null) {
                console.log("-", resolvedPromise.subgraphName, "\n");
            }
        }
        throw new Error("Indexing not complete for all networks - cannot release SDK-Core yet.");
    }
    console.log("Indexing Complete on All Networks - SDK-Core release can proceed");
};

main()
    .then(() => process.exit(0))
    .catch((err) => {
        console.error(err);
        process.exit(1);
    });
