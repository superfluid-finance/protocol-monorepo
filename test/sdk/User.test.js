const { toBN } = require("@decentral.ee/web3-helpers");
const { expectRevert } = require("@openzeppelin/test-helpers");
const TestEnvironment = require("../TestEnvironment");
var should = require("should");

contract("ConstantFlowAgreementV1 helper class", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 4));
    const { admin: adminAddress, alice: aliceAddress, bob: bobAddress, carol: carolAddress } = t.aliases;

    let sf;
    let superToken;
    let alice;
    let bob;
    let carol;

    before(async () => {
        await t.reset();
        sf = t.sf;
    });

    beforeEach(async () => {
        await t.createNewToken({ doUpgrade: true });
        ({ superToken } = t.contracts);
        alice = sf.user({ address: aliceAddress, token: superToken.address });
        bob = sf.user({ address: bobAddress, token: superToken.address });
        carol = sf.user({ address: carolAddress, token: superToken.address });
    });

    describe("initialize", () => {
        it("user", async () => {
            const admin = sf.user({ address: adminAddress, token: superToken.address });
            assert.equal(admin.address, adminAddress);
            assert.equal(admin.token, superToken.address);
            assert.equal(admin.sf, sf);
        });
    });

    describe.only("new flows", () => {
        it("should fail without recipient", async () => {
            (
                await alice.flow({
                    recipient: null,
                    flowRate: "0"
                })
            ).should.throwError(/^You must provide a recipient and flowRate*/);
        });
        it("should fail without flowRate", async () => {
            (
                await alice.flow({
                    recipient: bob.address,
                    flowRate: null
                })
            ).should.throwError(/^You must provide a recipient and flowRate*/);
        });
        it("should create a new flow", async () => {
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580" // 100 / mo
            });
            // validate flow data
            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address
            });
            const block = await web3.eth.getBlock(tx.receipt.blockNumber);
            assert.equal(flow.timestamp.getTime(), block.timestamp * 1000);
            assert.equal(flow.flowRate, "38580246913580");
            assert.notEqual(flow.deposit, "0");
            assert.equal(flow.owedDeposit, "0");
            // validate account net flows
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: alice.address
                    })
                ).toString(),
                "-38580246913580"
            );
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: bob.address
                    })
                ).toString(),
                "38580246913580"
            );
        });
    });
    describe("existing flows", () => {
        beforeEach(async () => {
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580" // 100 / mo
            });
            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address
            });
            const block = await web3.eth.getBlock(tx.receipt.blockNumber);
            assert.equal(flow.timestamp.getTime(), block.timestamp * 1000);
            assert.equal(flow.flowRate, "38580246913580");
        });
        it("should modify an existing flow", async () => {
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "19290123456790" // 50 / mo
            });

            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address
            });
            const block = await web3.eth.getBlock(tx.receipt.blockNumber);
            assert.equal(flow.timestamp.getTime(), block.timestamp * 1000);
            assert.equal(flow.flowRate, "19290123456790");
        });

        it("should stop an existing flow", async () => {
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "0" // 0 / mo
            });

            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address
            });
            const block = await web3.eth.getBlock(tx.receipt.blockNumber);
            assert.equal(flow.timestamp.getTime(), block.timestamp * 1000);
            assert.equal(flow.flowRate, "0");
        });
    });
    //
    // it("createFlow with onTransaction", async () => {
    //     let txHash = "";
    //     const tx = await sf.cfa.createFlow({
    //         superToken: superToken.address,
    //         sender: alice,
    //         receiver: bob,
    //         flowRate: "38580246913580", // 100 / mo
    //         onTransaction: hash => { txHash = hash; }
    //     });
    //     assert.equal(txHash, tx.receipt.transactionHash);
    // });
    //
    // it("updateFlow", async () => {
    //     await sf.cfa.createFlow({
    //         superToken: superToken.address,
    //         sender: alice,
    //         receiver: bob,
    //         flowRate: "38580246913580" // 100 / mo
    //     });
    //     await sf.cfa.updateFlow({
    //         superToken: superToken.address,
    //         sender: alice,
    //         receiver: bob,
    //         flowRate: "19290123456790", // 100 / mo
    //     });
    //     // validate account net flows
    //     assert.equal((await sf.cfa.getNetFlow({
    //         superToken: superToken.address,
    //         account: alice,
    //     })).toString(), "-19290123456790");
    //     assert.equal((await sf.cfa.getNetFlow({
    //         superToken: superToken.address,
    //         account: bob,
    //     })).toString(), "19290123456790");
    // });
    //
    // it("updateFlow with onTransaction", async () => {
    //     let txHash = "";
    //     await sf.cfa.createFlow({
    //         superToken: superToken.address,
    //         sender: alice,
    //         receiver: bob,
    //         flowRate: "38580246913580" // 100 / mo
    //     });
    //     const tx = await sf.cfa.updateFlow({
    //         superToken: superToken.address,
    //         sender: alice,
    //         receiver: bob,
    //         flowRate: "19290123456790", // 100 / mo
    //         onTransaction: hash => { txHash = hash; }
    //     });
    //     assert.equal(txHash, tx.receipt.transactionHash);
    // });
    //
    // describe("deleteFlow", () => {
    //     beforeEach(async () => {
    //         await sf.cfa.createFlow({
    //             superToken: superToken.address,
    //             sender: alice,
    //             receiver: bob,
    //             flowRate: "38580246913580" // 100 / mo
    //         });
    //     });
    //
    //     it("by sender", async () => {
    //         const ethBefore = await web3.eth.getBalance(alice);
    //         await sf.cfa.deleteFlow({
    //             superToken: superToken.address,
    //             sender: alice,
    //             receiver: bob,
    //         });
    //         const ethAfter = await web3.eth.getBalance(alice);
    //         assert.isTrue(toBN(ethAfter).lt(toBN(ethBefore)));
    //         assert.equal((await sf.cfa.getNetFlow({
    //             superToken: superToken.address,
    //             account: alice,
    //         })).toString(), "0");
    //         assert.equal((await sf.cfa.getNetFlow({
    //             superToken: superToken.address,
    //             account: bob,
    //         })).toString(), "0");
    //     });
    //
    //     it("by receiver", async () => {
    //         const ethBefore = await web3.eth.getBalance(bob);
    //         await sf.cfa.deleteFlow({
    //             superToken: superToken.address,
    //             sender: alice,
    //             receiver: bob,
    //             by: bob,
    //         });
    //         const ethAfter = await web3.eth.getBalance(bob);
    //         assert.isTrue(toBN(ethAfter).lt(toBN(ethBefore)));
    //         assert.equal((await sf.cfa.getNetFlow({
    //             superToken: superToken.address,
    //             account: alice,
    //         })).toString(), "0");
    //         assert.equal((await sf.cfa.getNetFlow({
    //             superToken: superToken.address,
    //             account: bob,
    //         })).toString(), "0");
    //     });
    //
    //     it("by wrong person", async () => {
    //         await expectRevert(sf.cfa.deleteFlow({
    //             superToken: superToken.address,
    //             sender: alice,
    //             receiver: bob,
    //             by: admin
    //         }), "CFA: sender account is not critical");
    //     });
    //
    //     it("by sender with onTransaction", async () => {
    //         let txHash = "";
    //         const tx = await sf.cfa.deleteFlow({
    //             superToken: superToken.address,
    //             sender: alice,
    //             receiver: bob,
    //             onTransaction: hash => { txHash = hash; }
    //         });
    //         assert.equal(txHash, tx.receipt.transactionHash);
    //     });
    // });
    //
    // describe("listFlows", () => {
    //     it("normally", async () => {
    //         await sf.cfa.createFlow({
    //             superToken: superToken.address,
    //             sender: alice,
    //             receiver: bob,
    //             flowRate: "38580246913580" // 100 / mo
    //         });
    //         await sf.cfa.createFlow({
    //             superToken: superToken.address,
    //             sender: bob,
    //             receiver: carol,
    //             flowRate: "19290123456790" // 100 / mo
    //         });
    //         assert.deepEqual(await sf.cfa.listFlows({
    //             superToken: superToken.address,
    //             account: alice,
    //         }), {
    //             inFlows: [],
    //             outFlows: [{
    //                 sender: alice,
    //                 receiver: bob,
    //                 flowRate: "38580246913580"
    //             }]
    //         });
    //         assert.deepEqual(await sf.cfa.listFlows({
    //             superToken: superToken.address,
    //             account: bob,
    //         }), {
    //             inFlows: [{
    //                 sender: alice,
    //                 receiver: bob,
    //                 flowRate: "38580246913580"
    //             }],
    //             outFlows: [{
    //                 sender: bob,
    //                 receiver: carol,
    //                 flowRate: "19290123456790"
    //             }],
    //         });
    //     });
    //
    //     it("onlyInFlows and onlyOutFlows", async () => {
    //         await sf.cfa.createFlow({
    //             superToken: superToken.address,
    //             sender: alice,
    //             receiver: bob,
    //             flowRate: "38580246913580" // 100 / mo
    //         });
    //         await sf.cfa.createFlow({
    //             superToken: superToken.address,
    //             sender: bob,
    //             receiver: carol,
    //             flowRate: "19290123456790" // 100 / mo
    //         });
    //         assert.deepEqual(await sf.cfa.listFlows({
    //             superToken: superToken.address,
    //             account: bob,
    //             onlyOutFlows: true
    //         }), {
    //             outFlows: [{
    //                 sender: bob,
    //                 receiver: carol,
    //                 flowRate: "19290123456790"
    //             }],
    //         });
    //         assert.deepEqual(await sf.cfa.listFlows({
    //             superToken: superToken.address,
    //             account: bob,
    //             onlyInFlows: true
    //         }), {
    //             inFlows: [{
    //                 sender: alice,
    //                 receiver: bob,
    //                 flowRate: "38580246913580"
    //             }],
    //         });
    //     });
    // });
});
