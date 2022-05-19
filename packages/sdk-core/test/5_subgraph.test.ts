import { Framework } from "../src";
import { setup } from "../scripts/setup";
import { chainIdToResolverDataMap, MATIC_CHAIN_ID } from "../src/constants";
import {
    testExpectListenerThrow,
    testExpectWeb3OnlyErrors,
    testGetAllEventsQuery,
    testListenerInitialization,
    testQueryClassFunctions,
} from "../previous-versions-testing/queryTests";

describe("Subgraph Tests", () => {
    let framework: Framework;

    // NOTE: we use MATIC_CHAIN_ID as default for the live endpoint because
    // MATIC is normally the bottleneck
    const resolverData = chainIdToResolverDataMap.get(MATIC_CHAIN_ID);
    if (!resolverData) throw new Error("Resolver data is undefined");

    const subgraphEndpoint =
        process.env.LOCAL_SUBGRAPH_URL || resolverData.subgraphAPIEndpoint;

    before(async () => {
        const { frameworkClass } = await setup({ subgraphEndpoint });
        framework = frameworkClass;
    });

    describe("Query Class Tests", () => {
        it("Should be able to execute all of the query class", async () => {
            await testQueryClassFunctions(framework);
        });

        it("Should be able to make the getAllEvents query", async () => {
            await testGetAllEventsQuery(framework);
        });

        it("Should throw if listener ms < 1000", async () => {
            await testExpectListenerThrow(framework);
        });

        it("Should be able to use the listener", async () => {
            await testListenerInitialization(framework);
        });
    });

    describe("WEB3_ONLY mode should not allow queries", () => {
        before(async () => {
            const { frameworkClass } = await setup({
                subgraphEndpoint,
                dataMode: "WEB3_ONLY",
            });
            framework = frameworkClass;
        });

        it("Should fail when trying to execute any of the query class in WEB3_ONLY mode", async () => {
            await testExpectWeb3OnlyErrors(framework);
        });
    });
});
