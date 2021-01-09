//const { assert } = require("chai");
const { web3tx } = require("@decentral.ee/web3-helpers");
const deployFramework = require("../scripts/deploy-framework");
const deployTestToken = require("../scripts/deploy-test-token");
const deploySuperToken = require("../scripts/deploy-super-token");
const TestResolver = artifacts.require("TestResolver");
const Proxiable = artifacts.require("UUPSProxiable");
const Superfluid = artifacts.require("Superfluid");
const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const { ZERO_ADDRESS } = require("@openzeppelin/test-helpers").constants;
//const { assert } = require("chai");
const { web3tx } = require("@decentral.ee/web3-helpers");
const deployFramework = require("../scripts/deploy-framework");
const deployTestToken = require("../scripts/deploy-test-token");
const deploySuperToken = require("../scripts/deploy-super-token");
const TestResolver = artifacts.require("TestResolver");
const Proxiable = artifacts.require("UUPSProxiable");
const Superfluid = artifacts.require("Superfluid");
const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const { ZERO_ADDRESS } = require("@openzeppelin/test-helpers").constants;

contract("deployment test (outside truffle environment)", () => {

    const errorHandler = err => { if (err) throw err; };
    const cfav1Type = web3.utils.sha3("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    const idav1Type = web3.utils.sha3("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");

    beforeEach(() => {
        delete process.env.RESET;
        delete process.env.RELEASE_VERSION;
        delete process.env.TEST_RESOLVER_ADDRESS;
    });

    // some cleanup
    afterEach(() => {
        delete process.env.RESET;
        delete process.env.TEST_RESOLVER_ADDRESS;
    });

    async function getSuperfluidAddresses() {
        const version = "test";
        const superfluidName = `Superfluid.${version}`;
        const govName = `TestGovernance.${version}`;
        const testResolver = await TestResolver.at(process.env.TEST_RESOLVER_ADDRESS);
        const superfluid = await Superfluid.at(await testResolver.get(superfluidName));
        const superfluidCode = await superfluid.getCodeAddress.call();
        const gov = await testResolver.get(govName);
        const superTokenFactory = await superfluid.getSuperTokenFactory.call();
        const superTokenFactoryLogic = await superfluid.getSuperTokenFactoryLogic.call();
        const superTokenLogic = await (await ISuperTokenFactory.at(superTokenFactory)).getSuperTokenLogic();
        const cfa = await (await Proxiable.at(await superfluid.getAgreementClass(cfav1Type))).getCodeAddress.call();
        const ida = await (await Proxiable.at(await superfluid.getAgreementClass(idav1Type))).getCodeAddress.call();
        return {
            superfluid,
            superfluidCode,
            gov,
            superTokenFactory,
            superTokenFactoryLogic,
            superTokenLogic,
            cfa,
            ida
        };
    }

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

    it("Superfluid fresh deployment", async () => {
        await deployFramework(errorHandler);
        const s = await getSuperfluidAddresses();
        assert.notEqual(s.superfluidCode, ZERO_ADDRESS, "superfluidCode not set");
        assert.notEqual(s.gov, ZERO_ADDRESS, "gov not set");
        assert.notEqual(s.superTokenFactory, ZERO_ADDRESS, "superTokenFactory not set");
        assert.notEqual(s.superTokenFactoryLogic, ZERO_ADDRESS, "superTokenFactoryLogic not set");
        assert.notEqual(s.superTokenLogic, ZERO_ADDRESS, "superTokenLogic not set");
        assert.notEqual(s.cfa, ZERO_ADDRESS, "cfa not registered");
        assert.notEqual(s.ida, ZERO_ADDRESS, "ida not registered");
        assert.isTrue(await s.superfluid.isAgreementClassListed.call(await s.superfluid.getAgreementClass(cfav1Type)));
        assert.isTrue(await s.superfluid.isAgreementClassListed.call(await s.superfluid.getAgreementClass(idav1Type)));
        assert.isTrue(await s.superfluid.isAgreementTypeListed.call(cfav1Type));
        assert.isTrue(await s.superfluid.isAgreementTypeListed.call(idav1Type));
    });

    it("Superfluid deployment and updates", async () => {
        // use the same resolver for the entire test
        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;

        console.log("==== First deployment");
        await deployFramework(errorHandler);
        const s1 = await getSuperfluidAddresses();

        console.log("==== Deploy again without logic contract changes");
        await deployFramework(errorHandler);
        const s2 = await getSuperfluidAddresses();
        assert.equal(s1.superfluid.address, s2.superfluid.address,
            "Superfluid proxy should stay the same address");
        assert.equal(s1.superfluidCode, s2.superfluidCode,
            "superfluid logic deployment not required");
        assert.equal(s1.gov, s2.gov,
            "Governance deployment not required");
        assert.equal(s1.superTokenFactory, s2.superTokenFactory,
            "superTokenFactory deployment not required");
        assert.equal(s1.superTokenFactoryLogic, s2.superTokenFactoryLogic,
            "superTokenFactoryLogic deployment not required");
        assert.equal(s1.superTokenLogic, s2.superTokenLogic,
            "superTokenLogic deployment not required");
        assert.equal(s1.cfa, s2.cfa,
            "cfa deployment not required");
        assert.equal(s1.ida, s2.ida,
            "cfa deployment not required");

        console.log("==== Reset all");
        process.env.RESET = 1;
        await deployFramework(errorHandler);
        const s3 = await getSuperfluidAddresses();
        assert.notEqual(s3.superfluidCode, ZERO_ADDRESS, "superfluidCode not set");
        assert.notEqual(s3.gov, ZERO_ADDRESS, "gov not set");
        assert.notEqual(s3.superTokenFactory, ZERO_ADDRESS, "superTokenFactory not set");
        assert.notEqual(s3.superTokenFactoryLogic, ZERO_ADDRESS, "superTokenFactoryLogic not set");
        assert.notEqual(s3.superTokenLogic, ZERO_ADDRESS, "superTokenLogic not set");
        assert.notEqual(s3.cfa, ZERO_ADDRESS, "cfa not registered");
        assert.notEqual(s3.ida, ZERO_ADDRESS, "ida not registered");
        assert.notEqual(s1.superfluid.address, s3.superfluid.address);
        assert.notEqual(s1.superfluidCode, s3.superfluidCode);
        assert.notEqual(s1.gov, s3.gov);
        assert.notEqual(s1.superTokenFactory, s3.superTokenFactory);
        assert.notEqual(s1.superTokenLogic, s3.superTokenLogic);
        assert.notEqual(s1.cfa, s3.cfa);
        assert.notEqual(s1.ida, s3.ida);

        console.log("==== Deploy again with mock logic contract");
        delete process.env.RESET;
        await deployFramework(errorHandler, { useMocks: true });
        const s4 = await getSuperfluidAddresses();
        assert.equal(s3.superfluid.address, s4.superfluid.address,
            "Superfluid proxy should stay the same address");
        assert.notEqual(s3.superfluidCode, s4.superfluidCode,
            "superfluid logic deployment required");
        assert.equal(s3.gov, s4.gov,
            "Governance deployment not required");
        assert.equal(s3.superTokenFactory, s4.superTokenFactory,
            "superTokenFactory proxy should stay the same address");
        assert.notEqual(s3.superTokenFactoryLogic, s4.superTokenFactoryLogic,
            "superTokenFactoryLogic deployment required");
        assert.notEqual(s3.superTokenLogic, s4.superTokenLogic,
            "superTokenLogic update required");
        assert.equal(s3.cfa, s4.cfa,
            "cfa deployment not required");
        assert.equal(s3.ida, s4.ida,
            "cfa deployment not required");
    });

    it("Test Token deployment", async () => {
        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        await deployFramework(errorHandler);

        // first deployment
        assert.equal(await testResolver.get("tokens.TEST7262"), ZERO_ADDRESS);
        await deployTestToken(errorHandler, [":", "TEST7262"]);
        const address1 = await testResolver.get("tokens.TEST7262");
        assert.notEqual(address1, ZERO_ADDRESS);

        // second deployment
        await deployTestToken(errorHandler, [":", "TEST7262"]);
        const address2 = await testResolver.get("tokens.TEST7262");
        assert.equal(address2, address1);

        // new deployment after framework reset
        process.env.RESET = 1;
        await deployFramework(errorHandler);
        await deployTestToken(errorHandler, [":", "TEST7262"]);
        const address3 = await testResolver.get("tokens.TEST7262");
        assert.equal(address3, address2);
    });

    it("Super Token deployment", async () => {
        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        delete process.env.RESET;
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        await deployFramework(errorHandler);

        // deploy test token first
        await deployTestToken(errorHandler, [":", "TEST7262"]);

        // first deployment
        assert.equal(await testResolver.get("supertokens.test.TEST7262x"), ZERO_ADDRESS);
        await deploySuperToken(errorHandler, [":", "TEST7262"]);
        const address1 = await testResolver.get("supertokens.test.TEST7262x");
        assert.notEqual(address1, ZERO_ADDRESS);

        // second deployment
        await deploySuperToken(errorHandler, [":", "TEST7262"]);
        const address2 = await testResolver.get("supertokens.test.TEST7262x");
        assert.equal(address1, address2);

        // new deployment after framework reset
        process.env.RESET = 1;
        await deployFramework(errorHandler);
        await deploySuperToken(errorHandler, [":", "TEST7262"]);
        const address3 = await testResolver.get("supertokens.test.TEST7262x");
        assert.notEqual(address3, address2);
    });
});
