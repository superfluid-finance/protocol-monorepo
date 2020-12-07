const {
    toBN
} = require("@decentral.ee/web3-helpers");
const {
    expectRevert
} = require("@openzeppelin/test-helpers");
const TestEnvironment = require("../TestEnvironment");

contract("ConstantFlowAgreementV1 helper class", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 4));
    const { admin, alice, bob, carol } = t.aliases;

    let sf;
    let superToken;


    before(async () => {
        await t.reset();
        sf = t.sf;
    });
    after(function() {

        sf.generateGasReport("ConstantFlowAgreementV1");
    });

    beforeEach(async () => {
        await t.createNewToken({ doUpgrade: true });
        ({ superToken } = t.contracts);
    });

    it("createFlow", async () => {
        const tx = await sf.cfa.createFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580" // 100 / mo
        });
        // validate flow data
        const flow = await sf.cfa.getFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob
        });
        const block = await web3.eth.getBlock(tx.receipt.blockNumber);
        assert.equal(flow.timestamp.getTime(), block.timestamp*1000);
        assert.equal(flow.flowRate, "38580246913580");
        assert.notEqual(flow.deposit, "0");
        assert.equal(flow.owedDeposit, "0");
        // validate account net flows
        assert.equal((await sf.cfa.getNetFlow({
            superToken: superToken.address,
            account: alice,
        })).toString(), "-38580246913580");
        assert.equal((await sf.cfa.getNetFlow({
            superToken: superToken.address,
            account: bob,
        })).toString(), "38580246913580");
    });

    it("updateFlow", async () => {
        await sf.cfa.createFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580" // 100 / mo
        });
        await sf.cfa.updateFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "19290123456790" // 100 / mo
        });
        // validate account net flows
        assert.equal((await sf.cfa.getNetFlow({
            superToken: superToken.address,
            account: alice,
        })).toString(), "-19290123456790");
        assert.equal((await sf.cfa.getNetFlow({
            superToken: superToken.address,
            account: bob,
        })).toString(), "19290123456790");
    });

    describe("deleteFlow", () => {
        beforeEach(async () => {
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: "38580246913580" // 100 / mo
            });
        });

        it("by sender", async () => {
            const ethBefore = await web3.eth.getBalance(alice);
            await sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
            });
            const ethAfter = await web3.eth.getBalance(alice);
            assert.isTrue(toBN(ethAfter).lt(toBN(ethBefore)));
            assert.equal((await sf.cfa.getNetFlow({
                superToken: superToken.address,
                account: alice,
            })).toString(), "0");
            assert.equal((await sf.cfa.getNetFlow({
                superToken: superToken.address,
                account: bob,
            })).toString(), "0");
        });

        it("by receiver", async () => {
            const ethBefore = await web3.eth.getBalance(bob);
            await sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                by: bob
            });
            const ethAfter = await web3.eth.getBalance(bob);
            assert.isTrue(toBN(ethAfter).lt(toBN(ethBefore)));
            assert.equal((await sf.cfa.getNetFlow({
                superToken: superToken.address,
                account: alice,
            })).toString(), "0");
            assert.equal((await sf.cfa.getNetFlow({
                superToken: superToken.address,
                account: bob,
            })).toString(), "0");
        });

        it("by wrong person", async () => {
            await expectRevert(sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                by: admin
            }), "CFA: account is not critical");
        });
    });

    describe("listFlows", () => {
        it("normally", async () => {
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: "38580246913580" // 100 / mo
            });
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: bob,
                receiver: carol,
                flowRate: "19290123456790" // 100 / mo
            });
            assert.deepEqual(await sf.cfa.listFlows({
                superToken: superToken.address,
                account: alice,
            }), {
                inFlows: [],
                outFlows: [{
                    sender: alice,
                    receiver: bob,
                    flowRate: "38580246913580"
                }]
            });
            assert.deepEqual(await sf.cfa.listFlows({
                superToken: superToken.address,
                account: bob,
            }), {
                inFlows: [{
                    sender: alice,
                    receiver: bob,
                    flowRate: "38580246913580"
                }],
                outFlows: [{
                    sender: bob,
                    receiver: carol,
                    flowRate: "19290123456790"
                }],
            });
        });

        it("onlyInFlows and onlyOutFlows", async () => {
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: "38580246913580" // 100 / mo
            });
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: bob,
                receiver: carol,
                flowRate: "19290123456790" // 100 / mo
            });
            assert.deepEqual(await sf.cfa.listFlows({
                superToken: superToken.address,
                account: bob,
                onlyOutFlows: true
            }), {
                outFlows: [{
                    sender: bob,
                    receiver: carol,
                    flowRate: "19290123456790"
                }],
            });
            assert.deepEqual(await sf.cfa.listFlows({
                superToken: superToken.address,
                account: bob,
                onlyInFlows: true
            }), {
                inFlows: [{
                    sender: alice,
                    receiver: bob,
                    flowRate: "38580246913580"
                }],
            });
        });
    });

});
