const {assert} = require("chai");
const SuperfluidSDK = require("../src");

describe("getConfig", () => {
    before(() => {
        delete process.env.RESOLVER_ADDRESS;
    });

    it("goerli v1", async () => {
        // goerli
        const goerliConfig = SuperfluidSDK.getConfig(5, "v1");
        assert.isNotEmpty(goerliConfig.resolverAddress);
        assert.isNotEmpty(goerliConfig.subgraphQueryEndpoint);
        assert.isUndefined(goerliConfig.versions);
    });

    it("goerli test", async () => {
        // goerli
        const goerliConfig = SuperfluidSDK.getConfig(5, "test");
        assert.isNotEmpty(goerliConfig.resolverAddress);
        assert.isUndefined(goerliConfig.subgraphQueryEndpoint);
        assert.isUndefined(goerliConfig.versions);
    });

    it("defaultConfig", async () => {
        // defaultConfig
        const defaultConfig = SuperfluidSDK.getConfig(8888);
        assert.isUndefined(defaultConfig.resolverAddress);
        assert.isUndefined(defaultConfig.versions);
    });

    it("defaultConfig overriding RESOLVER_ADDRESS", async () => {
        // test environment
        process.env.RESOLVER_ADDRESS = "0x42";
        const testConfig = SuperfluidSDK.getConfig(5555);
        assert.equal(testConfig.resolverAddress, "0x42");
        assert.isUndefined(testConfig.versions);
        delete process.env.RESOLVER_ADDRESS;
    });
});
