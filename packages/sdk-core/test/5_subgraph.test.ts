import { Framework } from "../src";
import { setup } from "../scripts/setup";
import { expect } from "chai";
import { chainIdToResolverDataMap, MATIC_CHAIN_ID } from "../src/constants";

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

    describe("WEB3_ONLY mode should not allow queries", () => {
        before(async () => {
            const { frameworkClass } = await setup({
                subgraphEndpoint,
                dataMode: "WEB3_ONLY",
            });
            framework = frameworkClass;
        });

        it("Should fail when trying to execute any of the query class in WEB3_ONLY mode", async () => {
            try {
                await framework.query.listAllSuperTokens({});
            } catch (err: any) {
                expect(err.message).to.contain(
                    "Unsupported Web 3 Only Error - This query is not supported in WEB3_ONLY mode."
                );
            }
            try {
                await framework.query.listIndexes({});
            } catch (err: any) {
                expect(err.message).to.contain(
                    "Unsupported Web 3 Only Error - This query is not supported in WEB3_ONLY mode."
                );
            }
            try {
                await framework.query.listIndexSubscriptions({});
            } catch (err: any) {
                expect(err.message).to.contain(
                    "Unsupported Web 3 Only Error - This query is not supported in WEB3_ONLY mode."
                );
            }
            try {
                await framework.query.listStreams({});
            } catch (err: any) {
                expect(err.message).to.contain(
                    "Unsupported Web 3 Only Error - This query is not supported in WEB3_ONLY mode."
                );
            }
            try {
                await framework.query.listUserInteractedSuperTokens({});
            } catch (err: any) {
                expect(err.message).to.contain(
                    "Unsupported Web 3 Only Error - This query is not supported in WEB3_ONLY mode."
                );
            }
            try {
                await framework.query.listEvents({});
            } catch (err: any) {
                expect(err.message).to.contain(
                    "Unsupported Web 3 Only Error - This query is not supported in WEB3_ONLY mode."
                );
            }
        });
    });
    describe("Query Class Tests", () => {
        it("Should be able to execute all of the query class", async () => {
            const { frameworkClass } = await setup({ subgraphEndpoint });
            framework = frameworkClass;
            const tokens = await framework.query.listAllSuperTokens(
                {},
                { take: 10 }
            );
            const indexes = await framework.query.listIndexes({}, { take: 10 });
            const indexSubscriptions =
                await framework.query.listIndexSubscriptions({}, { take: 10 });
            const streams = await framework.query.listStreams({}, { take: 10 });
            const userInteractedSuperTokens =
                await framework.query.listUserInteractedSuperTokens(
                    {},
                    { take: 10 }
                );
            expect(tokens.data.length).to.be.greaterThan(0);
            expect(indexes.data.length).to.be.greaterThan(0);
            expect(indexSubscriptions.data.length).to.be.greaterThan(0);
            expect(streams.data.length).to.be.greaterThan(0);
            expect(userInteractedSuperTokens.data.length).to.be.greaterThan(0);
        });

        it("Should be able to make the getAllEvents query", async () => {
            // NOTE: when testing a live endpoint, we just want to make sure that
            // this version of SDK-Core will be able to handle the deployed subgraph endpoint
            // However, when we test the locally deployed endpoint, we want to test
            // as many of the mapGetAllEventsQueryEvents cases.
            const events = await framework.query.listEvents({}, { take: 100 });
            expect(events.data.length).to.be.greaterThan(0);
        });

        it("Should throw if listener ms < 1000", async () => {
            try {
                framework.query.on((e, u) => {
                    console.log(e);
                    u();
                }, 999);
            } catch (err: any) {
                expect(err.message).to.equal(
                    "Let's not go crazy with the queries..."
                );
            }
        });

        it("Should be able to use the listener", async () => {
            framework.query.on((e, u) => {
                console.log(e);
                u();
            }, 1000);
        });
    });
});
