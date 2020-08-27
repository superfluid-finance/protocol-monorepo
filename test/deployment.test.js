//const { assert } = require("chai");
const { web3tx } = require("@decentral.ee/web3-helpers");
const deployFramework = require("../scripts/deploy-framework");
const deployTestToken = require("../scripts/deploy-test-token");
const deploySuperToken = require("../scripts/deploy-super-token");
const TestResolver = artifacts.require("TestResolver");
const Superfluid = artifacts.require("Superfluid");

contract("deployment test", () => {

    const errorHandler = err => { if (err) throw err; };

    it("codeChanged function", async () => {
        const ConstantFlowAgreementV1 = artifacts.require("ConstantFlowAgreementV1");
        const {
            codeChanged
        } = require("../scripts/utils");
        const a1 = await web3tx(ConstantFlowAgreementV1.new, "ConstantFlowAgreementV1.new 1")();
        assert.isFalse(await codeChanged(ConstantFlowAgreementV1, a1.address));
    });

    it("Deploy/upgrade/reset Superfluid Framework", async () => {
        const version = process.env.RELEASE_VERSION || "test";

        const superfluidName = `Superfluid.${version}`;
        const cfaName = `ConstantFlowAgreementV1.${version}`;
        const superTokenName = `SuperTokenLogic.${version}`;
        const govName = `TestGovernance.${version}`;

        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        console.log("First deployment");
        await deployFramework(errorHandler);
        const superfluid1 = await testResolver.get(superfluidName);
        const cfa1 = await testResolver.get(cfaName);
        const gov1 = await testResolver.get(govName);
        const superToken1 = await testResolver.get(superTokenName);
        assert.notEqual(superfluid1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(cfa1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superToken1, "0x0000000000000000000000000000000000000000");

        console.log("Upgrade logic contract");
        await deployFramework(errorHandler);
        const superfluid2 = await testResolver.get(superfluidName);
        const cfa2 = await testResolver.get(cfaName);
        const gov2 = await testResolver.get(govName);
        const superToken2 = await testResolver.get(superTokenName);
        assert.equal(superfluid1, superfluid2);
        assert.equal(
            await (await Superfluid.at(superfluid1)).getCodeAddress(),
            await (await Superfluid.at(superfluid2)).getCodeAddress());
        assert.equal(cfa1, cfa2, "cfa deployment not required");
        assert.equal(gov1, gov2, "Governance deployment not required");
        assert.equal(superToken1, superToken2);

        console.log("Reset all");
        process.env.RESET = 1;
        await deployFramework(errorHandler);
        const superfluid3 = await testResolver.get(superfluidName);
        const cfa3 = await testResolver.get(cfaName);
        const gov3 = await testResolver.get(govName);
        const superToken3 = await testResolver.get(superTokenName);
        assert.notEqual(superfluid3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(cfa3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superToken3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superfluid1, superfluid3);
        assert.notEqual(cfa1, cfa3);
        assert.notEqual(gov1, gov3);
        assert.notEqual(superToken1, superToken3);
    });

    it("Deploy/upgrade/reset Super Token", async () => {
        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        await deployFramework(errorHandler);
        await deployTestToken(errorHandler, [":", "TEST"]);
        await deploySuperToken(errorHandler, [":", "TEST"]);
    });
});
