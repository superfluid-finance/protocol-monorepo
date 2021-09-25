const { toWad } = require("@decentral.ee/web3-helpers");

const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");
const SuperfluidSDK = require("../src");

const { Web3Provider } = require("@ethersproject/providers");

const chai = require("chai");
const chaiAsPromised = require("chai-as-promised");
chai.use(chaiAsPromised);
const expect = chai.expect;

const emptyIda = {
    ida: {
        subscriptions: {
            indexIds: [],
            publishers: [],
            unitsList: [],
        },
    },
};

describe("User helper class", function () {
    this.timeout(600e3);
    const t = TestEnvironment.getSingleton();

    let adminAddress, aliceAddress, bobAddress, carolAddress;
    let alice, bob, carol;
    let sf;
    let superToken;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
        });

        ({
            admin: adminAddress,
            alice: aliceAddress,
            bob: bobAddress,
            carol: carolAddress,
        } = t.aliases);

        superToken = t.sf.tokens.TESTx;

        sf = new SuperfluidSDK.Framework({
            ethers: new Web3Provider(web3.currentProvider),
            version: "test",
        });
        await sf.initialize();
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
        alice = sf.user({ address: aliceAddress, token: superToken.address });
        bob = sf.user({ address: bobAddress, token: superToken.address });
        carol = sf.user({ address: carolAddress, token: superToken.address });
    });

    describe("initialize", () => {
        it("user", async () => {
            const admin = sf.user({
                address: adminAddress,
                token: superToken.address,
            });
            assert.equal(admin.address, adminAddress);
            assert.equal(admin.token, superToken.address);
            assert.equal(admin.sf, sf);
        });
    });

    describe.skip("details", () => {
        it("shows user details", async () => {
            await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580", // 100 / mo
            });
            await bob.flow({
                recipient: carol.address,
                flowRate: "19290123456790", // 50 / mo
            });
            assert.deepEqual(await alice.details(), {
                cfa: {
                    flows: {
                        inFlows: [],
                        outFlows: [
                            {
                                sender: alice.address,
                                receiver: bob.address,
                                flowRate: "38580246913580",
                            },
                        ],
                    },
                    netFlow: "-38580246913580",
                },
                ...emptyIda,
            });
            assert.deepEqual(await bob.details(), {
                cfa: {
                    flows: {
                        inFlows: [
                            {
                                sender: alice.address,
                                receiver: bob.address,
                                flowRate: "38580246913580",
                            },
                        ],
                        outFlows: [
                            {
                                sender: bob.address,
                                receiver: carol.address,
                                flowRate: "19290123456790",
                            },
                        ],
                    },
                    netFlow: "19290123456790",
                },
                ...emptyIda,
            });
            console.log(JSON.stringify(await bob.details()));
        });
    });
    describe.skip("new flows", () => {
        it("fail without recipient", async () => {
            await expect(
                alice.flow({
                    recipient: null,
                    flowRate: "0",
                })
            ).to.be.rejectedWith(/You must provide a recipient and flowRate/);
        });
        it("fail without flowRate", async () => {
            // Using https://github.com/domenic/chai-as-promised
            await expect(
                alice.flow({
                    recipient: adminAddress,
                    flowRate: null,
                })
            ).to.be.rejectedWith(/You must provide a recipient and flowRate/);
        });
        it("create a new flow", async () => {
            await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580", // 100 / mo
            });
            // validate flow data
            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address,
            });
            assert.equal(flow.flowRate, "38580246913580");
            assert.notEqual(flow.deposit, "0");
            assert.equal(flow.owedDeposit, "0");
            // validate account net flows
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: alice.address,
                    })
                ).toString(),
                "-38580246913580"
            );
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account: bob.address,
                    })
                ).toString(),
                "38580246913580"
            );
        });
        it.skip("create a new flow with onTransaction", async () => {
            let txHash;
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580", // 100 / mo
                onTransaction: (hash) => {
                    txHash = hash;
                },
            });
            assert.equal(txHash, tx.receipt.transactionHash);
        });
        it("create a new flow with User object argument", async () => {
            await alice.flow({
                // "bob" rather than "bob.address"
                recipient: bob,
                flowRate: "38580246913580", // 100 / mo
            });
            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address,
            });
            assert.equal(flow.flowRate, "38580246913580");
        });
    });
    describe.skip("existing flows", () => {
        beforeEach(async () => {
            await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580", // 100 / mo
            });
            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address,
            });
            assert.equal(flow.flowRate, "38580246913580");
        });
        it("modify an existing flow", async () => {
            await alice.flow({
                recipient: bob.address,
                flowRate: "19290123456790", // 50 / mo
            });

            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address,
            });
            assert.equal(flow.flowRate, "19290123456790");
        });
    });
    describe.skip("pools", () => {
        const poolId = 1;
        beforeEach(async () => {
            await alice.createPool({ poolId });
        });
        it("create a new ppol", async () => {
            const { exist } = await sf.ida.getIndex({
                superToken: superToken.address,
                publisher: aliceAddress,
                indexId: poolId,
            });
            assert.equal(exist, true);
        });
        it("giveShares", async () => {
            await alice.giveShares({
                poolId,
                shares: 100,
                recipient: bobAddress,
            });
            const { totalUnitsPending } = await sf.ida.getIndex({
                superToken: superToken.address,
                publisher: aliceAddress,
                indexId: poolId,
            });
            assert.equal(totalUnitsPending, 100);
        });
        it("distributeToPool", async () => {
            await alice.giveShares({
                poolId,
                shares: 100,
                recipient: bobAddress,
            });

            await sf.ida.approveSubscription({
                superToken: superToken.address,
                indexId: poolId,
                publisher: aliceAddress,
                sender: bobAddress,
            });

            await alice.distributeToPool({
                poolId,
                amount: toWad(100).toString(),
            });
            const balance = await superToken.balanceOf(bobAddress);
            assert.equal(balance.toString(), toWad(200).toString());
        });
    });
});
