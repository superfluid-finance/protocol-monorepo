const deployFramework = require("../scripts/deploy-framework");
const { web3tx } = require("@decentral.ee/web3-helpers");
const { assert } = require("chai");


contract("sdk test", () => {

    const errorHandler = err => { if (err) throw err; };

    const Superfluid = require("..");

    function testLoadedContracts(sf) {
        const {
            IERC20,
            IResolver,
            TokenInfo,
            ISuperfluid,
            ISuperToken,
            IConstantFlowAgreementV1,
            IInstantDistributionAgreementV1,
        } = sf.contracts;

        assert.isDefined(IERC20.abi);
        assert.equal(IERC20.contractName, "IERC20");
        assert.isTrue(IERC20.abi.filter(i => i.name === "Transfer").length > 0);

        assert.isDefined(IResolver.abi);
        assert.equal(IResolver.contractName, "IResolver");
        assert.isTrue(IResolver.abi.filter(i => i.name === "set").length > 0);

        assert.isDefined(TokenInfo.abi);
        assert.equal(TokenInfo.contractName, "TokenInfo");
        assert.isTrue(TokenInfo.abi.filter(i => i.name === "symbol").length > 0);

        assert.isDefined(ISuperfluid.abi);
        assert.equal(ISuperfluid.contractName, "ISuperfluid");
        assert.isTrue(ISuperfluid.abi.filter(i => i.name === "getERC20Wrapper").length > 0);

        assert.isDefined(ISuperToken.abi);
        assert.equal(ISuperToken.contractName, "ISuperToken");
        assert.isTrue(ISuperToken.abi.filter(i => i.name === "upgrade").length > 0);

        assert.isDefined(IConstantFlowAgreementV1.abi);
        assert.equal(IConstantFlowAgreementV1.contractName, "IConstantFlowAgreementV1");
        assert.isTrue(IConstantFlowAgreementV1.abi.filter(i => i.name === "updateFlow").length > 0);

        assert.isDefined(IInstantDistributionAgreementV1.abi);
        assert.equal(IInstantDistributionAgreementV1.contractName, "IInstantDistributionAgreementV1");
        assert.isTrue(IInstantDistributionAgreementV1.abi.filter(i => i.name === "createIndex").length > 0);
    }

    it("get config", async () => {
        // goerli
        const goerliConfig = Superfluid.getConfig(5);
        assert.isNotEmpty(goerliConfig.resolverAddress);
        // kovan
        const kovanConfig = Superfluid.getConfig(42);
        assert.isNotEmpty(kovanConfig.resolverAddress);
        /*
        // test environment
        let testConfig = Superfluid.getConfig(5555);
        assert.isUndefined(testConfig.resolverAddress);
        process.env.TEST_RESOLVER_ADDRESS="0x42";
        testConfig = Superfluid.getConfig(5555);
        assert.equal(testConfig.resolverAddress, "0x42");
        */
    });

    it("load framework without truffle framework", async () => {
        const TestResolver = artifacts.require("TestResolver");
        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        process.env.RESET = 1;
        await deployFramework(errorHandler);

        const sf = new Superfluid.Framework({ web3Provider: web3.currentProvider });
        testLoadedContracts(sf);

        await sf.initialize();
    });

    it("load framework with truffle framework", async () => {
        const TestResolver = artifacts.require("TestResolver");
        const testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        process.env.RESET = 1;
        await deployFramework(errorHandler);

        const sf = new Superfluid.Framework({ isTruffle: true });
        testLoadedContracts(sf);

        await sf.initialize();
    });

});
