const { assert } = require("chai");
const SuperfluidSDK = require("../src");

contract("SuperfluidSDK JS SDK", () => {
    it("getConfig", async () => {
        delete process.env.TEST_RESOLVER_ADDRESS;
        // goerli
        const goerliConfig = SuperfluidSDK.getConfig(5);
        assert.isNotEmpty(goerliConfig.resolverAddress);
        // defaultConfig
        const defaultConfig = SuperfluidSDK.getConfig(8888);
        assert.isUndefined(defaultConfig.resolverAddress);
    });
});
