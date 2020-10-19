const { assert } = require("chai");
const {
    toWad,
} = require("@decentral.ee/web3-helpers");
// const {
//     expectRevert
// } = require("@openzeppelin/test-helpers");
const deployTestEnvironment = require("../../scripts/deploy-test-environment");
const SuperfluidSDK = require("../..");
const TestToken = artifacts.require("TestToken");


contract("ConstantFlowAgreementV1 helper class", accounts => {

    const errorHandler = err => { if (err) throw err; };

    const alice = accounts[0];
    const bob = accounts[1];
    let sf;

    beforeEach(async () => {
        await deployTestEnvironment(errorHandler);
        sf = new SuperfluidSDK.Framework({
            web3Provider: web3.currentProvider,
            tokens: ["fUSDC"]
        });
        await sf.initialize();
        await (await TestToken.at(sf.tokens.fUSDC.address)).mint(alice, toWad("100"), { from: alice });
        await sf.tokens.fUSDC.approve(sf.tokens.fUSDCx.address, toWad("100"), { from: alice });
        await sf.tokens.fUSDCx.upgrade(toWad("100"), {from: alice});
    });

    it("createFlow", async () => {
        await sf.cfa.createFlow({
            superToken: sf.tokens.fUSDCx.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580" // 100 / mo
        });
        assert.equal((await sf.cfa.getNetFlow({
            superToken: sf.tokens.fUSDCx.address,
            account: alice,
        })).toString(), "-38580246913580");
        assert.equal((await sf.cfa.getNetFlow({
            superToken: sf.tokens.fUSDCx.address,
            account: bob,
        })).toString(), "38580246913580");
    });

});
