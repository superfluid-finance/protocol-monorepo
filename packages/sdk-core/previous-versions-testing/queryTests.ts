import { Query } from "@superfluid-finance/sdk-core/src";
import { expect } from "chai";
import metadata from "@superfluid-finance/metadata";

/**
 * We only use matic network endpoints for v1 release tests
 * otherwise, we use goerli
 * @returns chainId
 */
export const getChainId = () => {
    // null coalesce, but this should NEVER return null for either
    return process.env.SUBGRAPH_RELEASE_TAG == "v1"
        ? metadata.getNetworkByShortName("matic")?.chainId ?? 0
        : metadata.getNetworkByShortName("goerli")?.chainId ?? 0;
};

export const testQueryClassFunctions = async (query: Query) => {
    const tokens = await query.listAllSuperTokens({}, { take: 10 });
    const indexes = await query.listIndexes({}, { take: 10 });
    const indexSubscriptions = await query.listIndexSubscriptions(
        {},
        { take: 10 }
    );
    const streams = await query.listStreams({}, { take: 10 });
    const userInteractedSuperTokens = await query.listUserInteractedSuperTokens(
        {},
        { take: 10 }
    );
    expect(tokens.data.length).to.be.greaterThan(0);
    expect(indexes.data.length).to.be.greaterThan(0);
    expect(indexSubscriptions.data.length).to.be.greaterThan(0);
    expect(streams.data.length).to.be.greaterThan(0);
    expect(userInteractedSuperTokens.data.length).to.be.greaterThan(0);
};

export const testGetAllEventsQuery = async (query: Query) => {
    // NOTE: when testing a live endpoint, we just want to make sure that
    // this version of SDK-Core will be able to handle the deployed subgraph endpoint
    // However, when we test the locally deployed endpoint, we want to test
    // as many of the mapGetAllEventsQueryEvents cases.
    const events = await query.listEvents({}, { take: 100 });
    expect(events.data.length).to.be.greaterThan(0);
};

export const testExpectListenerThrow = async (query: Query) => {
    try {
        query.on((e, u) => {
            console.log(e);
            u();
        }, 999);
    } catch (err: any) {
        expect(err.message).to.equal("Let's not go crazy with the queries...");
    }
};

export const testListenerInitialization = async (query: Query) => {
    query.on((e, u) => {
        console.log(e);
        u();
    }, 1000);
};
