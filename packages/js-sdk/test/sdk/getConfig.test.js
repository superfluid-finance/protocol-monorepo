const { assert } = require("chai");
const SuperfluidSDK = require("../..");

contract("SuperfluidSDK JS SDK", () => {
    it("getConfig", async () => {
        delete process.env.TEST_RESOLVER_ADDRESS;
        // goerli
        const goerliConfig = SuperfluidSDK.getConfig(5);
        assert.isNotEmpty(goerliConfig.resolverAddress);
        // defaultConfig
        const defaultConfig = SuperfluidSDK.getConfig(8888);
        assert.isUndefined(defaultConfig.resolverAddress);
        // test environment
        process.env.TEST_RESOLVER_ADDRESS = "0x42";
        const testConfig = SuperfluidSDK.getConfig(5555);
        assert.equal(testConfig.resolverAddress, "0x42");
        delete process.env.TEST_RESOLVER_ADDRESS;
    });
});
