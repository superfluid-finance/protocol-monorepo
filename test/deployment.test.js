//const { assert } = require("chai");
const { web3tx } = require("@decentral.ee/web3-helpers");
const deploy = require("../scripts/deploy");
const TestResolver = artifacts.require("TestResolver");
const SuperToken = artifacts.require("SuperToken");

contract("deployment test", () => {

    it("Fresh deployment", async () => {
        const errorHandler = err => { if (err) throw err; };
        await deploy(errorHandler);
    });

    it("Deploy/upgrade/reset", async () => {
        const errorHandler = err => { if (err) throw err; };
        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        console.log("First deployment");
        await deploy(errorHandler);
        const testToken1 = await testResolver.get("TestToken.test");
        const flowAgreement1 = await testResolver.get("FlowAgreement.test");
        const gov1 = await testResolver.get("TestGovernance.test");
        const superToken1 = await testResolver.get("SuperTestToken.test");
        assert.notEqual(testToken1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(flowAgreement1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superToken1, "0x0000000000000000000000000000000000000000");
        const code1 = await (await SuperToken.at(superToken1)).getCodeAddress.call();

        console.log("Upgrade logic contract");
        await deploy(errorHandler);
        const testToken2 = await testResolver.get("TestToken.test");
        const flowAgreement2 = await testResolver.get("FlowAgreement.test");
        const gov2 = await testResolver.get("TestGovernance.test");
        const superToken2 = await testResolver.get("SuperTestToken.test");
        const code2 = await (await SuperToken.at(superToken2)).getCodeAddress.call();
        assert.equal(testToken1, testToken2);
        assert.equal(flowAgreement1, flowAgreement2, "FlowAgreement deployment not required");
        assert.equal(gov1, gov2, "Governance deployment not required");
        assert.equal(superToken1, superToken2);
        assert.notEqual(code1, code2);

        console.log("Reset all");
        process.env.RESET = 1;
        await deploy(errorHandler);
        const testToken3 = await testResolver.get("TestToken.test");
        const flowAgreement3 = await testResolver.get("FlowAgreement.test");
        const gov3 = await testResolver.get("TestGovernance.test");
        const superToken3 = await testResolver.get("SuperTestToken.test");
        assert.notEqual(testToken3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(flowAgreement3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superToken3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(testToken1, testToken3);
        assert.notEqual(flowAgreement1, flowAgreement3);
        assert.notEqual(gov1, gov3);
        assert.notEqual(superToken1, superToken3);
    });

});
