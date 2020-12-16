//const { assert } = require("chai");
const { web3tx } = require("@decentral.ee/web3-helpers");
const deployFramework = require("../scripts/deploy-framework");
const deployTestToken = require("../scripts/deploy-test-token");
const deploySuperToken = require("../scripts/deploy-super-token");
const TestResolver = artifacts.require("TestResolver");
const Proxiable = artifacts.require("UUPSProxiable");
const Superfluid = artifacts.require("Superfluid");

contract("deployment test (outside truffle environment)", () => {

    const errorHandler = err => { if (err) throw err; };

    const cfav1Type = web3.utils.sha3("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    const idav1Type = web3.utils.sha3("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");

    // some cleanup
    after(() => {
        delete process.env.RESET;
        delete process.env.TEST_RESOLVER_ADDRESS;
    });

    it("codeChanged function", async () => {
        const {
            codeChanged
        } = require("../scripts/utils");
        {
            // with constructor param
            const a1 = await web3tx(Superfluid.new, "Superfluid.new 1")(true);
            assert.isFalse(await codeChanged(Superfluid, a1.address));
        }
        {
            // without constructor param
            const ConstantFlowAgreementV1 = artifacts.require("ConstantFlowAgreementV1");
            const a1 = await web3tx(ConstantFlowAgreementV1.new, "ConstantFlowAgreementV1.new 1")();
            assert.isFalse(await codeChanged(ConstantFlowAgreementV1, a1.address));
        }
    });

    it("Superfluid framework test environment deployment script", async () => {
        const version = process.env.RELEASE_VERSION || "test";

        const superfluidName = `Superfluid.${version}`;
        const govName = `TestGovernance.${version}`;

        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        console.log("==== First deployment");
        await deployFramework(errorHandler);
        const superfluid1 = await Superfluid.at(await testResolver.get(superfluidName));
        const superfluid1Code = await superfluid1.getCodeAddress.call();
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

        console.log("==== Deploy again without logic contract changes");
        await deployFramework(errorHandler);
        const superfluid2 = await Superfluid.at(await testResolver.get(superfluidName));
        const superfluid2Code = await superfluid2.getCodeAddress.call();
        const gov2 = await testResolver.get(govName);
        const cfa2 = await (await Proxiable.at(await superfluid2.getAgreementClass(cfav1Type))).getCodeAddress.call();
        const ida2 = await (await Proxiable.at(await superfluid2.getAgreementClass(idav1Type))).getCodeAddress.call();
        assert.equal(superfluid1.address, superfluid2.address, "Proxy stays the same address");
        assert.equal(superfluid1Code, superfluid2Code, "superfluid logic deployment not required");
        assert.equal(gov1, gov2, "Governance deployment not required");
        assert.equal(cfa1, cfa2, "cfa deployment not required");
        assert.equal(ida1, ida2, "cfa deployment not required");

        console.log("==== Reset all");
        process.env.RESET = 1;
        await deployFramework(errorHandler);
        const superfluid3 = await Superfluid.at(await testResolver.get(superfluidName));
        const superfluid3Code = await superfluid3.getCodeAddress.call();
        const gov3 = await testResolver.get(govName);
        const cfa3 = await (await Proxiable.at(await superfluid3.getAgreementClass(cfav1Type))).getCodeAddress.call();
        const ida3 = await (await Proxiable.at(await superfluid3.getAgreementClass(idav1Type))).getCodeAddress.call();
        assert.notEqual(superfluid3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(gov3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(cfa3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(ida3, "0x0000000000000000000000000000000000000000");
        assert.notEqual(superfluid1, superfluid3);
        assert.notEqual(superfluid2Code, superfluid3Code);
        assert.notEqual(gov1, gov3);
        assert.notEqual(cfa1, cfa3);
        assert.notEqual(ida1, ida3);

        console.log("==== Deploy again with mock logic contract");
        delete process.env.RESET;
        await deployFramework(errorHandler, { useMocks: true });
        const superfluid4 = await Superfluid.at(await testResolver.get(superfluidName));
        const superfluid4Code = await superfluid4.getCodeAddress.call();
        const gov4 = await testResolver.get(govName);
        const cfa4 = await (await Proxiable.at(await superfluid4.getAgreementClass(cfav1Type))).getCodeAddress.call();
        const ida4 = await (await Proxiable.at(await superfluid4.getAgreementClass(idav1Type))).getCodeAddress.call();
        assert.equal(superfluid3.address, superfluid4.address, "Proxy stays the same address");
        assert.notEqual(superfluid3Code, superfluid4Code, "superfluid logic deployment required");
        assert.equal(gov3, gov4, "Governance deployment not required"); // no mock contract
        assert.equal(cfa3, cfa4, "cfa deployment not required"); // no mock contract
        assert.equal(ida3, ida4, "cfa deployment not required"); // no mock contract
    });

    it("Deployment without resolver", async () => {
        await deployFramework(errorHandler);
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
