import { Query } from "../src";
import { chainIdToResolverDataMap } from "../src/constants";
import {
    getChainId,
    testExpectListenerThrow,
    testExpectWeb3OnlyErrors,
    testGetAllEventsQuery,
    testListenerInitialization,
    testQueryClassFunctions,
} from "../previous-versions-testing/queryTests";

describe("Subgraph Tests", () => {
    let query: Query;

    const chainIdToUse = getChainId();
    const resolverData = chainIdToResolverDataMap.get(chainIdToUse);
    if (!resolverData) throw new Error("Resolver data is undefined");

    const subgraphEndpoint =
        process.env.LOCAL_SUBGRAPH_URL || resolverData.subgraphAPIEndpoint;

    before(() => {
        query = new Query({
            dataMode: "SUBGRAPH_ONLY",
            customSubgraphQueriesEndpoint: subgraphEndpoint,
        });
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
