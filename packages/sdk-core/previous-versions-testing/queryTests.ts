import { Framework } from "@superfluid-finance/sdk-core/src";
import { expect } from "chai";

export const testExpectWeb3OnlyErrors = async (framework: Framework) => {
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
};

export const testQueryClassFunctions = async (framework: Framework) => {
    const tokens = await framework.query.listAllSuperTokens({}, { take: 10 });
    const indexes = await framework.query.listIndexes({}, { take: 10 });
    const indexSubscriptions = await framework.query.listIndexSubscriptions(
        {},
        { take: 10 }
    );
    const streams = await framework.query.listStreams({}, { take: 10 });
    const userInteractedSuperTokens =
        await framework.query.listUserInteractedSuperTokens({}, { take: 10 });
    expect(tokens.data.length).to.be.greaterThan(0);
    expect(indexes.data.length).to.be.greaterThan(0);
    expect(indexSubscriptions.data.length).to.be.greaterThan(0);
    expect(streams.data.length).to.be.greaterThan(0);
    expect(userInteractedSuperTokens.data.length).to.be.greaterThan(0);
};

export const testGetAllEventsQuery = async (framework: Framework) => {
    // NOTE: when testing a live endpoint, we just want to make sure that
    // this version of SDK-Core will be able to handle the deployed subgraph endpoint
    // However, when we test the locally deployed endpoint, we want to test
    // as many of the mapGetAllEventsQueryEvents cases.
    const events = await framework.query.listEvents({}, { take: 100 });
    expect(events.data.length).to.be.greaterThan(0);
};

export const testExpectListenerThrow = async (framework: Framework) => {
    try {
        framework.query.on((e, u) => {
            console.log(e);
            u();
        }, 999);
    } catch (err: any) {
        expect(err.message).to.equal("Let's not go crazy with the queries...");
    }
};

export const testListenerInitialization = async (framework: Framework) => {
    framework.query.on((e, u) => {
        console.log(e);
        u();
    }, 1000);
};
