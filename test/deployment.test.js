//const { assert } = require("chai");
const { web3tx } = require("@decentral.ee/web3-helpers");
const deployFramework = require("../scripts/deploy-framework");
const deployTestToken = require("../scripts/deploy-test-token");
const deploySuperToken = require("../scripts/deploy-super-token");
const TestResolver = artifacts.require("TestResolver");
const Proxiable = artifacts.require("Proxiable");
const Superfluid = artifacts.require("Superfluid");

contract("deployment test", () => {

    const errorHandler = err => { if (err) throw err; };

    const cfav1Type = web3.utils.sha3("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    const idav1Type = web3.utils.sha3("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");

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
        const govName = `TestGovernance.${version}`;

        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        console.log("First deployment");
        await deployFramework(errorHandler);
        const superfluid1 = await Superfluid.at(await testResolver.get(superfluidName));
        const gov1 = await testResolver.get(govName);
        const cfa1 = await (await Proxiable.at(await superfluid1.getAgreementClass(cfav1Type))).getCodeAddress.call();
        const ida1 = await (await Proxiable.at(await superfluid1.getAgreementClass(idav1Type))).getCodeAddress.call();
        assert.notEqual(superfluid1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(cfa1, "0x0000000000000000000000000000000000000000");
        assert.notEqual(ida1, "0x0000000000000000000000000000000000000000");
        assert.isTrue(await superfluid1.isAgreementClassListed.call(await superfluid1.getAgreementClass(cfav1Type)));
        assert.isTrue(await superfluid1.isAgreementClassListed.call(await superfluid1.getAgreementClass(idav1Type)));
        assert.isTrue(await superfluid1.isAgreementTypeListed.call(cfav1Type));
        assert.isTrue(await superfluid1.isAgreementTypeListed.call(idav1Type));

        console.log("Upgrade logic contract");
        await deployFramework(errorHandler);
        const superfluid2 = await Superfluid.at(await testResolver.get(superfluidName));
        const gov2 = await testResolver.get(govName);
        const cfa2 = await (await Proxiable.at(await superfluid2.getAgreementClass(cfav1Type))).getCodeAddress.call();
        const ida2 = await (await Proxiable.at(await superfluid2.getAgreementClass(idav1Type))).getCodeAddress.call();
        assert.equal(superfluid1.address, superfluid2.address);
        assert.equal(await superfluid1.getCodeAddress.call(), await superfluid2.getCodeAddress.call());
        assert.equal(gov1, gov2, "Governance deployment not required");
        assert.equal(cfa1, cfa2, "cfa deployment not required");
        assert.equal(ida1, ida2, "cfa deployment not required");

        console.log("Reset all");
        process.env.RESET = 1;
        await deployFramework(errorHandler);
        const superfluid3 = await Superfluid.at(await testResolver.get(superfluidName));
        const gov3 = await testResolver.get(govName);
        const cfa3 = await (await Proxiable.at(await superfluid3.getAgreementClass(cfav1Type))).getCodeAddress.call();
        const ida3 = await (await Proxiable.at(await superfluid3.getAgreementClass(idav1Type))).getCodeAddress.call();
        assert.notEqual(superfluid3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(cfa3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(ida3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superfluid1, superfluid3);
        assert.notEqual(gov1, gov3);
        assert.notEqual(cfa1, cfa3);
        assert.notEqual(ida1, ida3);
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
