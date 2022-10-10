const {toBN} = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");
const {expect} = require("chai");

describe("ConstantFlowAgreementV1Helper class", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let admin, alice, bob, carol;
    let sf;
    let superToken;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: false,
            web3,
            nAccounts: 4,
        });

        ({admin, alice, bob, carol} = t.aliases);
        sf = t.sf;

        ({superToken} = await t.deployNewToken("TEST2", {
            isTruffle: false,
            web3,
            doUpgrade: true,
        }));
        await t.pushEvmSnapshot();
    });

    after(async () => {
        await t.popEvmSnapshot();
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    it("eip155 protection check", async () => {
        const tx = await sf.cfa.createFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580", // 100 / mo
        });
        const txReceipt = await web3.eth.getTransaction(tx.tx);
        // per https://eips.ethereum.org/EIPS/eip-155
        // to use "v" to validate if it is eip-155 enabled transaction
        console.log("txReceipt", txReceipt);
        assert.isDefined(txReceipt.v);
    });

    it("createFlow", async () => {
        const tx = await sf.cfa.createFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580", // 100 / mo
        });
        // validate flow data
        const flow = await sf.cfa.getFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
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
                    account: alice,
                })
            ).toString(),
            "-38580246913580"
        );
        assert.equal(
            (
                await sf.cfa.getNetFlow({
                    superToken: superToken.address,
                    account: bob,
                })
            ).toString(),
            "38580246913580"
        );
        const aliceFlows = await sf.cfa.getAccountFlowInfo({
            superToken: superToken.address,
            account: alice,
        });
        assert.equal(aliceFlows.flowRate, "-38580246913580");
        const bobFlows = await sf.cfa.getAccountFlowInfo({
            superToken: superToken.address,
            account: bob,
        });
        assert.equal(bobFlows.flowRate, "38580246913580");
        assert.equal(
            aliceFlows.timestamp.toString(),
            bobFlows.timestamp.toString()
        );
    });

    it("createFlow with onTransaction", async () => {
        let txHash = "";
        const tx = await sf.cfa.createFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580", // 100 / mo
            onTransaction: (hash) => {
                txHash = hash;
            },
        });
        assert.equal(txHash, tx.receipt.transactionHash);
    });

    it("updateFlow", async () => {
        await sf.cfa.createFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580", // 100 / mo
        });
        await sf.cfa.updateFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "19290123456790", // 100 / mo
        });
        // validate account net flows
        assert.equal(
            (
                await sf.cfa.getNetFlow({
                    superToken: superToken.address,
                    account: alice,
                })
            ).toString(),
            "-19290123456790"
        );
        assert.equal(
            (
                await sf.cfa.getNetFlow({
                    superToken: superToken.address,
                    account: bob,
                })
            ).toString(),
            "19290123456790"
        );
    });

    it("updateFlow with onTransaction", async () => {
        let txHash = "";
        await sf.cfa.createFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "38580246913580", // 100 / mo
        });
        const tx = await sf.cfa.updateFlow({
            superToken: superToken.address,
            sender: alice,
            receiver: bob,
            flowRate: "19290123456790", // 100 / mo
            onTransaction: (hash) => {
                txHash = hash;
            },
        });
        assert.equal(txHash, tx.receipt.transactionHash);
    });

    describe("deleteFlow", () => {
        beforeEach(async () => {
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: "38580246913580", // 100 / mo
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
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: alice,
                    })
                ).toString(),
                "0"
            );
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: bob,
                    })
                ).toString(),
                "0"
            );
        });

        it("by receiver", async () => {
            const ethBefore = await web3.eth.getBalance(bob);
            await sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                by: bob,
            });
            const ethAfter = await web3.eth.getBalance(bob);
            assert.isTrue(toBN(ethAfter).lt(toBN(ethBefore)));
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: alice,
                    })
                ).toString(),
                "0"
            );
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: bob,
                    })
                ).toString(),
                "0"
            );
        });

        it("by wrong person", async () => {
            try {
                await sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: alice,
                    receiver: bob,
                    by: admin,
                });
            } catch (err) {
                expect(err.message).to.not.be.null;
            }
        });

        it("by sender with onTransaction", async () => {
            let txHash = "";
            const tx = await sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                onTransaction: (hash) => {
                    txHash = hash;
                },
            });
            assert.equal(txHash, tx.receipt.transactionHash);
        });
    });

    describe("listFlows", () => {
        it("normally", async () => {
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: "38580246913580", // 100 / mo
            });
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: bob,
                receiver: carol,
                flowRate: "19290123456790", // 100 / mo
            });
            assert.deepEqual(
                await sf.cfa.listFlows({
                    superToken: superToken.address,
                    account: alice,
                }),
                {
                    inFlows: [],
                    outFlows: [
                        {
                            sender: alice,
                            receiver: bob,
                            flowRate: "38580246913580",
                        },
                    ],
                }
            );
            assert.deepEqual(
                await sf.cfa.listFlows({
                    superToken: superToken.address,
                    account: bob,
                }),
                {
                    inFlows: [
                        {
                            sender: alice,
                            receiver: bob,
                            flowRate: "38580246913580",
                        },
                    ],
                    outFlows: [
                        {
                            sender: bob,
                            receiver: carol,
                            flowRate: "19290123456790",
                        },
                    ],
                }
            );
        });

        it("onlyInFlows and onlyOutFlows", async () => {
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: "38580246913580", // 100 / mo
            });
            await sf.cfa.createFlow({
                superToken: superToken.address,
                sender: bob,
                receiver: carol,
                flowRate: "19290123456790", // 100 / mo
            });
            assert.deepEqual(
                await sf.cfa.listFlows({
                    superToken: superToken.address,
                    account: bob,
                    onlyOutFlows: true,
                }),
                {
                    outFlows: [
                        {
                            sender: bob,
                            receiver: carol,
                            flowRate: "19290123456790",
                        },
                    ],
                }
            );
            assert.deepEqual(
                await sf.cfa.listFlows({
                    superToken: superToken.address,
                    account: bob,
                    onlyInFlows: true,
                }),
                {
                    inFlows: [
                        {
                            sender: alice,
                            receiver: bob,
                            flowRate: "38580246913580",
                        },
                    ],
                }
            );
        });
    });
});
