//const { assert } = require("chai");
const { web3tx } = require("@decentral.ee/web3-helpers");
const deploy = require("../scripts/deploy");
const deployTestToken = require("../scripts/deploy-test-token");
const deploySuperToken = require("../scripts/deploy-super-token");
const TestResolver = artifacts.require("TestResolver");
//const SuperToken = artifacts.require("SuperToken");
const SuperfluidRegistry = artifacts.require("SuperfluidRegistry");

contract("deployment test", () => {

    const errorHandler = err => { if (err) throw err; };

    it("codeChanged function", async () => {
        const FlowAgreement = artifacts.require("FlowAgreement");
        const {
            codeChanged
        } = require("../scripts/utils");
        const a1 = await web3tx(FlowAgreement.new, "FlowAgreement.new 1")();
        assert.isFalse(await codeChanged(FlowAgreement, a1.address));
    });

    it("Deploy/upgrade/reset Superfluid Framework", async () => {

        const version = process.env.RELEASE_VERSION || "test";

        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        console.log("First deployment");
        await deploy(errorHandler);
        const registry1Name = `SuperfluidRegistry.${version}`;
        const registry1 = await testResolver.get(registry1Name);
        const flowAgreementName = `FlowAgreement.${version}`;
        const flowAgreement1 = await testResolver.get(flowAgreementName);
        const gov1Name = `TestGovernance.${version}`;
        const gov1 = await testResolver.get(gov1Name);
        const superTokenName = `SuperTokenLogic.${version}`;
        const superToken1 = await testResolver.get(superTokenName);
        assert.notEqual(registry1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(flowAgreement1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superToken1, "0x0000000000000000000000000000000000000000");

        console.log("Upgrade logic contract");
        await deploy(errorHandler);
        const registry2Name = `SuperfluidRegistry.${version}`;
        const registry2 = await testResolver.get(registry2Name);
        const flowAgreement2Name = `FlowAgreement.${version}`;
        const flowAgreement2 = await testResolver.get(flowAgreement2Name);
        const gov2Name = `TestGovernance.${version}`;
        const gov2 = await testResolver.get(gov2Name);
        const superToken2Name= `SuperTokenLogic.${version}`;
        const superToken2 = await testResolver.get(superToken2Name);
        assert.equal(registry1, registry2);
        assert.equal(
            await (await SuperfluidRegistry.at(registry1)).getCodeAddress(),
            await (await SuperfluidRegistry.at(registry2)).getCodeAddress());
        assert.equal(flowAgreement1, flowAgreement2, "FlowAgreement deployment not required");
        assert.equal(gov1, gov2, "Governance deployment not required");
        assert.equal(superToken1, superToken2);

        console.log("Reset all");
        process.env.RESET = 1;
        await deploy(errorHandler);
        const registry3Name = `SuperfluidRegistry.${version}`;
        const registry3 = await testResolver.get(registry3Name);
        const flowAgreement3Name = `FlowAgreement.${version}`;
        const flowAgreement3 = await testResolver.get(flowAgreement3Name);
        const gov3Name = `TestGovernance.${version}`;
        const gov3 = await testResolver.get(gov3Name);
        const superToken3Name = `SuperTokenLogic.${version}`;
        const superToken3 = await testResolver.get(superToken3Name);
        assert.notEqual(registry3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(flowAgreement3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superToken3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(registry1, registry3);
        assert.notEqual(flowAgreement1, flowAgreement3);
        assert.notEqual(gov1, gov3);
        assert.notEqual(superToken1, superToken3);
    });

    it("Deploy/upgrade/reset Super Token", async () => {
        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        await deploy(errorHandler);
        await deployTestToken(errorHandler, [":", "TEST"]);
        await deploySuperToken(errorHandler, [":", "TEST"]);
    });
});
