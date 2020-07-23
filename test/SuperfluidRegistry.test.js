const TestGovernance = artifacts.require("TestGovernance");
const SuperfluidRegistry = artifacts.require("SuperfluidRegistry");
const TestToken = artifacts.require("TestToken");

const {
    web3tx
} = require("@decentral.ee/web3-helpers");

contract("Superfluid Registry", accounts => {

    it("#1 SuperfluidRegistry.createERC20Wrapper", async () => {
        const governance = await web3tx(TestGovernance.new, "TestGovernance.new")(
            accounts[0],
            1,
            3600);
        const registry = await web3tx(SuperfluidRegistry.new, "SuperfluidRegistry.new")();
        await web3tx(registry.initialize, "SuperfluidRegistry.initialize")(governance.address);
        const token1 = await web3tx(TestToken.new, "TestToken.new 1")();
        const token2 = await web3tx(TestToken.new, "TestToken.new 2")();
        const result1 = await registry.getERC20Wrapper.call(
            "TEST1x",
            "Super Test Token 1",
            18,
            token1.address,
        );
        assert.isFalse(result1.created);
        const result2 = await registry.getERC20Wrapper.call(
            "TEST2x",
            "Super Test Token 2",
            18,
            token2.address,
        );
        assert.notEqual(result1.wrapperAddress, result2.wrapperAddress);
        assert.isFalse(result2.created);
        await web3tx(registry.createERC20Wrapper, "registry.createERC20Wrapper 1")(
            "TEST1x",
            "Super Test Token 1",
            18,
            token1.address
        );
        const result1b = await registry.getERC20Wrapper.call(
            "TEST1x",
            "Super Test Token 1",
            18,
            token1.address,
        );
        assert.equal(result1.wrapperAddress, result1b.wrapperAddress);
        assert.isTrue(result1b.created);
    });

});
