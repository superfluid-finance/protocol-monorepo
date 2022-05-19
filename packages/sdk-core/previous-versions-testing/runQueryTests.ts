import { Framework } from "@superfluid-finance/sdk-core/src";
import {
    chainIdToResolverDataMap,
    MATIC_CHAIN_ID,
} from "@superfluid-finance/sdk-core/src/constants";
import { ethers } from "hardhat";
import {
    testExpectListenerThrow,
    testExpectWeb3OnlyErrors,
    testGetAllEventsQuery,
    testListenerInitialization,
    testQueryClassFunctions,
} from "./queryTests";

const getSubgraphEndpoint = (chainId: number) => {
    const resolverData = chainIdToResolverDataMap.get(chainId);
    if (!resolverData) throw new Error("Resolver data is undefined");
    return resolverData.subgraphAPIEndpoint;
};

const RESOLVER_ADDRESS =
    process.env.RESOLVER_ADDRESS ||
    "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";

describe("Query Tests", () => {
    let framework: Framework;
    before(async () => {
        const [Deployer] = await ethers.getSigners();
        framework = await Framework.create({
            customSubgraphQueriesEndpoint: getSubgraphEndpoint(MATIC_CHAIN_ID),
            provider: Deployer!.provider!,
            networkName: "custom",
            resolverAddress: RESOLVER_ADDRESS,
            protocolReleaseVersion: "test",
        });
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
            const [Deployer] = await ethers.getSigners();
            framework = await Framework.create({
                customSubgraphQueriesEndpoint:
                    getSubgraphEndpoint(MATIC_CHAIN_ID),
                dataMode: "WEB3_ONLY",
                provider: Deployer!.provider!,
                networkName: "custom",
                resolverAddress: RESOLVER_ADDRESS,
                protocolReleaseVersion: "test",
            });
        });

        it("Should fail when trying to execute any of the query class in WEB3_ONLY mode", async () => {
            await testExpectWeb3OnlyErrors(framework);
        });
    });
});
