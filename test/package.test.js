const Web3 = require("web3");
const { assert } = require("chai");

describe("package test", () => {

    it("load contracts", async () => {
        const provider = new Web3.providers.HttpProvider("http://vitalik.mob");
        const { IERC20, SimpleVault }  = require("..").load(provider);
        assert.isDefined(IERC20.abi);
        assert.isDefined(SimpleVault.abi);
    });

});
