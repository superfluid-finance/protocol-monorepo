import { chainIdToResolverDataMap, Query } from "../src";
import {
    getChainId,
    testExpectListenerThrow,
    testGetAllEventsQuery,
    testListenerInitialization,
    testQueryClassFunctions,
} from "../previous-versions-testing/queryTests";
import { getSubgraphEndpoint } from "../previous-versions-testing/runQueryTests";
import { expect } from "chai";

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

        it("Should have the correct subgraph endpoints", async () => {
            const resolverDataArray = Array.from(
                chainIdToResolverDataMap.values()
            );
            await Promise.all(
                resolverDataArray.map(async (x) => {
                    // @note this handles arbitrum-goerli not being ready
                    if (!x.subgraphAPIEndpoint.includes("arbitrum-goerli")) {
                        if (
                            // @note this handles feature endpoint only having goerli/matic endpoints
                            (process.env.SUBGRAPH_RELEASE_TAG === "feature" &&
                                (x.networkName === "eth-goerli" ||
                                    x.networkName === "polygon-mainnet")) ||
                            process.env.SUBGRAPH_RELEASE_TAG !== "feature"
                        ) {
                            const query = new Query({
                                customSubgraphQueriesEndpoint:
                                    x.subgraphAPIEndpoint,
                            });
                            const event = await query.listEvents(
                                {},
                                { take: 1 }
                            );
                            expect(event.data.length).to.be.greaterThan(0);
                        }
                    }
                })
            );
        });
    });
});
