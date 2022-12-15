import { chainIdToResolverDataMap, NetworkData, Query } from "../src";
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
            ) as NetworkData[];
            await Promise.all(
                resolverDataArray.map(async (x) => {
                    // @note this handles arbitrum-goerli not being ready
                    if (x.subgraphAPIEndpoint.includes("arbitrum-goerli")) {
                        return;
                    }
                    // @note if SUBGRAPH_RELEASE_TAG is feature, we only test goerli
                    const isValidFeatureEndpoint =
                        process.env.SUBGRAPH_RELEASE_TAG === "feature" &&
                        x.networkName === "eth-goerli";
                    // @note else we test all networks
                    const isNotFeatureEndpoint =
                        process.env.SUBGRAPH_RELEASE_TAG !== "feature";

                    // @note MUST be eth-goerli for feature release tag OR NOT feature release tag
                    if (isValidFeatureEndpoint || isNotFeatureEndpoint) {
                        const query = new Query({
                            customSubgraphQueriesEndpoint:
                                x.subgraphAPIEndpoint,
                        });
                        const event = await query.listEvents({}, { take: 1 });
                        expect(event.data.length).to.be.greaterThan(0);
                    }
                })
            );
        });
    });
});
