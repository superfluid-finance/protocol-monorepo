import { Query } from "@superfluid-finance/sdk-core/src";
import { expect } from "chai";
import {
    ETH_GOERLI_CHAIN_ID,
    MATIC_CHAIN_ID,
} from "@superfluid-finance/sdk-core/src/constants";

/**
 * We only use matic network endpoints for v1 release tests
 * otherwise, we use goerli
 * @returns chainId
 */
export const getChainId = () => {
    return process.env.SUBGRAPH_RELEASE_TAG == "v1"
        ? MATIC_CHAIN_ID
        : ETH_GOERLI_CHAIN_ID;
};

export const testQueryClassFunctions = async (query: Query) => {
    await query.listAllSuperTokens({}, { take: 10 });
    await query.listIndexes({}, { take: 10 });
    await query.listIndexSubscriptions({}, { take: 10 });
    await query.listStreams({}, { take: 10 });
    await query.listUserInteractedSuperTokens({}, { take: 10 });
};

export const testGetAllEventsQuery = async (query: Query) => {
    // NOTE: when testing a live endpoint, we just want to make sure that
    // this version of SDK-Core will be able to handle the deployed subgraph endpoint
    // However, when we test the locally deployed endpoint, we want to test
    // as many of the mapGetAllEventsQueryEvents cases.
    await query.listEvents({}, { take: 100 });
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
