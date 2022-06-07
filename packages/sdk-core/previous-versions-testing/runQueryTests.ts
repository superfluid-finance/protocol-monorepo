import { Query } from "@superfluid-finance/sdk-core/src";
import { chainIdToResolverDataMap } from "@superfluid-finance/sdk-core/src/constants";
import {
    getChainId,
    testExpectListenerThrow,
    testExpectWeb3OnlyErrors,
    testGetAllEventsQuery,
    testListenerInitialization,
    testQueryClassFunctions,
} from "./queryTests";

export const getSubgraphEndpoint = (chainId: number) => {
    if (process.env.SUBGRAPH_RELEASE_TAG === "local")
        return "http://localhost:8000/subgraphs/name/superfluid-test";

    const resolverData = chainIdToResolverDataMap.get(chainId);
    if (!resolverData) throw new Error("Resolver data is undefined");
    return resolverData.subgraphAPIEndpoint;
};

describe("Query Tests", () => {
    let query: Query;
    before(async () => {
        const chainIdToUse = getChainId();
        let customSubgraphQueriesEndpoint = getSubgraphEndpoint(chainIdToUse);

        if (process.env.SUBGRAPH_RELEASE_TAG) {
            customSubgraphQueriesEndpoint =
                customSubgraphQueriesEndpoint.replace(
                    "v1",
                    process.env.SUBGRAPH_RELEASE_TAG
                );
        }
        query = new Query({
            dataMode: "SUBGRAPH_ONLY",
            customSubgraphQueriesEndpoint,
        });
        console.log("Testing with endpoint:", customSubgraphQueriesEndpoint);
    });

    describe("Query Class Tests", () => {
        it("Should be able to execute all of the query class", async () => {
            await testQueryClassFunctions(query);
        });

        it("Should be able to make the getAllEvents query", async () => {
            await testGetAllEventsQuery(query);
        });

        it("Should throw if listener ms < 1000", async () => {
            await testExpectListenerThrow(query);
        });

        it("Should be able to use the listener", async () => {
            await testListenerInitialization(query);
        });
    });

    describe("WEB3_ONLY mode should not allow queries", () => {
        it("Should fail when trying to execute any of the query class in WEB3_ONLY mode", async () => {
            await testExpectWeb3OnlyErrors(query);
        });
    });
});
