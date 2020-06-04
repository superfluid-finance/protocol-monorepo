const Web3 = require("web3");
const { assert } = require("chai");

describe("package test", () => {

    it("load contracts", async () => {
        const provider = new Web3.providers.HttpProvider("http://vitalik.mob");
        const Superfluid = require("..");
        const SuperfluidABI = require("../build/abi");

        const {
            IERC20,
            TestResolver,
            TestToken,
            IFlowAgreement,
            ISuperToken
        }  = Superfluid.load(provider);

        assert.isDefined(IERC20.abi);
        assert.equal(IERC20.contractName, "IERC20");
        assert.isDefined(SuperfluidABI.IERC20);

        assert.isDefined(TestResolver.abi);
        assert.equal(TestResolver.contractName, "TestResolver");
        assert.isDefined(SuperfluidABI.TestResolver);

        assert.isDefined(TestToken.abi);
        assert.equal(TestToken.contractName, "TestToken");
        assert.isDefined(SuperfluidABI.TestToken);

        assert.isDefined(IFlowAgreement.abi);
        assert.equal(IFlowAgreement.contractName, "IFlowAgreement");
        assert.isDefined(SuperfluidABI.IFlowAgreement);

        assert.isDefined(ISuperToken.abi);
        assert.equal(ISuperToken.contractName, "ISuperToken");
        assert.isDefined(SuperfluidABI.ISuperToken);
    });

});
