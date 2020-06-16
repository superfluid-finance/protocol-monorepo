const Web3 = require("web3");
const { assert } = require("chai");

describe("package test", () => {

    it("load contracts", async () => {
        const provider = new Web3.providers.HttpProvider("http://vitalik.mob");
        const Superfluid = require("..");

        const {
            IERC20,
            TestResolver,
            TestToken,
            IFlowAgreement,
            ISuperToken
        }  = Superfluid.load(provider);

        assert.isDefined(IERC20.abi);
        assert.equal(IERC20.contractName, "IERC20");
        assert.isTrue(IERC20.abi.filter(i => i.name === "Transfer").length > 0);

        assert.isDefined(TestResolver.abi);
        assert.equal(TestResolver.contractName, "TestResolver");
        assert.isTrue(TestResolver.abi.filter(i => i.name === "set").length > 0);

        assert.isDefined(TestToken.abi);
        assert.equal(TestToken.contractName, "TestToken");
        assert.isTrue(TestToken.abi.filter(i => i.name === "mint").length > 0);

        assert.isDefined(IFlowAgreement.abi);
        assert.equal(IFlowAgreement.contractName, "IFlowAgreement");
        assert.isTrue(IFlowAgreement.abi.filter(i => i.name === "updateFlow").length > 0);

        assert.isDefined(ISuperToken.abi);
        assert.equal(ISuperToken.contractName, "ISuperToken");
        assert.isTrue(ISuperToken.abi.filter(i => i.name === "upgrade").length > 0);
    });

});
