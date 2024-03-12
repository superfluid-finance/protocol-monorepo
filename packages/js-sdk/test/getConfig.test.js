const {assert} = require("chai");
const SuperfluidSDK = require("../src");

describe("getConfig", () => {
    before(() => {
        delete process.env.RESOLVER_ADDRESS;
    });

    it("mumbai v1", async () => {
        const mumbaiConfig = SuperfluidSDK.getConfig(80001, "v1");
        assert.isNotEmpty(mumbaiConfig.resolverAddress);
        assert.isNotEmpty(mumbaiConfig.subgraphQueryEndpoint);
        assert.isUndefined(mumbaiConfig.versions);
    });

    it("mumbai test", async () => {
        const mumbaiConfig = SuperfluidSDK.getConfig(80001, "test");
        assert.isNotEmpty(mumbaiConfig.resolverAddress);
        assert.isUndefined(mumbaiConfig.subgraphQueryEndpoint);
        assert.isUndefined(mumbaiConfig.versions);
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
