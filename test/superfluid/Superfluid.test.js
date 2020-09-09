const TestGovernance = artifacts.require("TestGovernance");
const Superfluid = artifacts.require("Superfluid");
const SuperToken = artifacts.require("SuperToken");
const TestToken = artifacts.require("TestToken");

const {
    web3tx
} = require("@decentral.ee/web3-helpers");

contract("Superfluid", accounts => {

    it("#1 ERC20Wrapper", async () => {

        const superfluid = await web3tx(Superfluid.new, "Superfluid.new")(
            {
                from: accounts[0]
            });
        const governance = await web3tx(TestGovernance.new, "TestGovernance.new")(
            accounts[0],
            1,
            3600,
            10000,
            10000,
            superfluid.address);
        await web3tx(superfluid.initialize, "superfluid.initialize")();
        await web3tx(superfluid.setGovernance, "superfluid.setGovernance")(
            governance.address
        );

        const superTokenLogic = await web3tx(SuperToken.new, "SuperToken.new due to code change")();
        await web3tx(superfluid.setSuperTokenLogic, "registry.setSuperTokenLogic")(
            superTokenLogic.address
        );
        const token1 = await web3tx(TestToken.new, "TestToken.new 1")("Test Token 1", "TT1");
        const token2 = await web3tx(TestToken.new, "TestToken.new 2")("Test Token 2", "TT2");
        const result1 = await superfluid.getERC20Wrapper.call(
            "TEST1x",
            18,
            token1.address,
        );
        assert.isFalse(result1.created);
        const result2 = await superfluid.getERC20Wrapper.call(
            "TEST2x",
            18,
            token2.address,
        );
        assert.notEqual(result1.wrapperAddress, result2.wrapperAddress);
        assert.isFalse(result2.created);
        await web3tx(superfluid.createERC20Wrapper, "registry.createERC20Wrapper 1")(
            "Super Test Token 1",
            "TEST1x",
            18,
            token1.address
        );
        const result1b = await superfluid.getERC20Wrapper.call(
            "TEST1x",
            18,
            token1.address,
        );
        assert.isTrue(result1b.created);
        assert.equal(result1.wrapperAddress, result1b.wrapperAddress);
    });

});
