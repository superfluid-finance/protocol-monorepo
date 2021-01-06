const { toBN } = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("../TestEnvironment");

const chai = require("chai");
const chaiAsPromised = require("chai-as-promised");
chai.use(chaiAsPromised);
const expect = chai.expect;

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

    describe.only("details", () => {
        it("shows user details", async () => {
            await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580" // 100 / mo
            });
            await bob.flow({
                recipient: carol.address,
                flowRate: "19290123456790" // 50 / mo
            });
            assert.deepEqual(await alice.details(), {
                cfa: {
                    flows: {
                        inFlows: [],
                        outFlows: [
                            {
                                sender: alice.address,
                                receiver: bob.address,
                                flowRate: "38580246913580"
                            }
                        ]
                    },
                    netFlow: "-38580246913580"
                }
            });
            assert.deepEqual(await bob.details(), {
                cfa: {
                    flows: {
                        inFlows: [
                            {
                                sender: alice.address,
                                receiver: bob.address,
                                flowRate: "38580246913580"
                            }
                        ],
                        outFlows: [
                            {
                                sender: bob.address,
                                receiver: carol.address,
                                flowRate: "19290123456790"
                            }
                        ]
                    },
                    netFlow: "19290123456790"
                }
            });
            console.log(JSON.stringify(await bob.details()));
        });
    });
    describe("new flows", () => {
        it("fail without recipient", async () => {
            // This method fails
            // await alice.flow({
            //     recipient: null,
            //     flowRate: "0"
            // });
            //
            // assert.fail(/^You must provide a recipient and flowRate*/);

            // This method also fails
            // try {
            //     await alice.flow({
            //         recipient: null,
            //         flowRate: "0"
            //     });
            // } catch (e) {
            //     assert.equal(e, /^You must provide a recipient and flowRate*/);
            // }

            // This also fails

            // await expect(
            //     await alice.flow({
            //         recipient: bob.address,
            //         flowRate: null
            //     })
            // ).to.throw(/^You must provide a recipient and flowRate*/);

            await expect(
                alice.flow({
                    recipient: null,
                    flowRate: "0"
                })
            ).to.be.rejectedWith(/You must provide a recipient and flowRate/);
        });
        it("fail without flowRate", async () => {
            // Using https://github.com/domenic/chai-as-promised
            await expect(
                alice.flow({
                    recipient: adminAddress,
                    flowRate: null
                })
            ).to.be.rejectedWith(/You must provide a recipient and flowRate/);
        });
        it("create a new flow", async () => {
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
        it("create a new flow with onTransaction", async () => {
            let txHash;
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580", // 100 / mo
                onTransaction: hash => {
                    txHash = hash;
                }
            });
            assert.equal(txHash, tx.receipt.transactionHash);
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
        it("modify an existing flow", async () => {
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
        it("modify an existing flow with onTransaction", async () => {
            let txHash;
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "19290123456790", // 100 / mo
                onTransaction: hash => {
                    txHash = hash;
                }
            });
            assert.equal(txHash, tx.receipt.transactionHash);
        });
        it("stop an existing flow", async () => {
            const ethBefore = await web3.eth.getBalance(alice.address);
            await alice.flow({
                recipient: bob.address,
                flowRate: "0" // 0 / mo
            });
            const ethAfter = await web3.eth.getBalance(alice.address);
            assert.isTrue(toBN(ethAfter).lt(toBN(ethBefore)));
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: alice.address
                    })
                ).toString(),
                "0"
            );
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: bob.address
                    })
                ).toString(),
                "0"
            );
        });
        it("stop an existing flow with onTransaction", async () => {
            let txHash;
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "0", // 100 / mo
                onTransaction: hash => {
                    txHash = hash;
                }
            });
            assert.equal(txHash, tx.receipt.transactionHash);
        });
    });
});
