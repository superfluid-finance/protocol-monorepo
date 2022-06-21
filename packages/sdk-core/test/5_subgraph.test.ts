import { Query } from "../src";
import {
    getChainId,
    testExpectListenerThrow,
    testGetAllEventsQuery,
    testListenerInitialization,
    testQueryClassFunctions,
} from "../previous-versions-testing/queryTests";
import { getSubgraphEndpoint } from "../previous-versions-testing/runQueryTests";

describe("Subgraph Tests", () => {
    let query: Query;

    const chainIdToUse = getChainId();
    const customSubgraphQueriesEndpoint = getSubgraphEndpoint(chainIdToUse);

    before(() => {
        query = new Query({
            customSubgraphQueriesEndpoint,
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
});
