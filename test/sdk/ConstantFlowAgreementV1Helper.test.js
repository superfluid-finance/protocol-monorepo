const { assert } = require("chai");
// const {
//     expectRevert
// } = require("@openzeppelin/test-helpers");
const TestEnvironment = require("../TestEnvironment");
const SuperfluidSDK = require("../..");


contract("ConstantFlowAgreementV1 helper class", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 3));
    const { alice, bob } = t.aliases;

    let sf;
    let superToken;

    before(async () => {
        await t.reset();
    });

    beforeEach(async () => {
        sf = new SuperfluidSDK.Framework({
            web3Provider: web3.currentProvider
        });
        await sf.initialize();
        await t.createNewToken({ doUpgrade: true });
        ({ superToken } = t.contracts);
    });

    it("createFlow", async () => {
        await sf.cfa.createFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580" // 100 / mo
        });
        assert.equal((await sf.cfa.getNetFlow({
            superToken: superToken.address,
            account: alice,
        })).toString(), "-38580246913580");
        assert.equal((await sf.cfa.getNetFlow({
            superToken: superToken.address,
            account: bob,
        })).toString(), "38580246913580");
    });

});
