const Web3 = require("web3");
const { assert } = require("chai");

describe("package test", () => {

    const Superfluid = require("..");

    it("load contracts", async () => {
        const provider = new Web3.providers.HttpProvider("http://vitalik.mob");

        const {
            IERC20,
            TestResolver,
            TokenInfo,
            SuperfluidRegistry,
            IFlowAgreement,
            ISuperToken
        }  = Superfluid.load(provider);

        assert.isDefined(IERC20.abi);
        assert.equal(IERC20.contractName, "IERC20");
        assert.isTrue(IERC20.abi.filter(i => i.name === "Transfer").length > 0);

        assert.isDefined(TestResolver.abi);
        assert.equal(TestResolver.contractName, "TestResolver");
        assert.isTrue(TestResolver.abi.filter(i => i.name === "set").length > 0);

        assert.isDefined(TokenInfo.abi);
        assert.equal(TokenInfo.contractName, "TokenInfo");
        assert.isTrue(TokenInfo.abi.filter(i => i.name === "symbol").length > 0);

        assert.isDefined(SuperfluidRegistry.abi);
        assert.equal(SuperfluidRegistry.contractName, "SuperfluidRegistry");
        assert.isTrue(SuperfluidRegistry.abi.filter(i => i.name === "getERC20Wrapper").length > 0);

        assert.isDefined(IFlowAgreement.abi);
        assert.equal(IFlowAgreement.contractName, "IFlowAgreement");
        assert.isTrue(IFlowAgreement.abi.filter(i => i.name === "updateFlow").length > 0);

        assert.isDefined(ISuperToken.abi);
        assert.equal(ISuperToken.contractName, "ISuperToken");
        assert.isTrue(ISuperToken.abi.filter(i => i.name === "upgrade").length > 0);
    });

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

});
