const { toBN, toWad } = require("@decentral.ee/web3-helpers");

const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const chai = require("chai");
const chaiAsPromised = require("chai-as-promised");
chai.use(chaiAsPromised);
const expect = chai.expect;

const emptyIda = {
    ida: {
        subscriptions: [],
    },
};

contract("ConstantFlowAgreementV1", accounts => {
    const errorHandler = err => {
        if (err) throw err;
    };

    const [adminAddress, bobAddress, carolAddress, danAddress] = accounts;

    let sf;
    let dai;
    let daix;
    let superToken;
    let alice;
    let bob;
    let carol;
    let admin;

    beforeEach(async function() {
        await deployTestToken(errorHandler, [":", "fDAI"], {
            web3,
            from: adminAddress,
        });
        await deploySuperToken(errorHandler, [":", "fDAI"], {
            web3,
            from: adminAddress,
        });

        sf = new SuperfluidSDK.Framework({
            web3,
            tokens: ["fDAI"],
            resolverAddress: "0xa36FfB4643C11307515F9851f2320a0556fD2687",
        });
        await sf.initialize();

        if (!dai) {
            const daiAddress = await sf.tokens.fDAI.address;
            dai = await sf.contracts.TestToken.at(daiAddress);
            for (let i = 0; i < accounts.length; ++i) {
                await web3tx(dai.mint, `Account ${i} mints many dai`)(
                    accounts[i],
                    toWad(10000000),
                    { from: accounts[i] }
                );
            }
        }

        daix = sf.tokens.fDAIx;

        for (let i = 0; i < accounts.length; ++i) {
            await web3tx(
                dai.approve,
                `Account ${i} approves daix`
            )(daix.address, toWad(100), { from: accounts[i] });
        }

        alice = sf.user({ address: aliceAddress, token: superToken.address });
        bob = sf.user({ address: bobAddress, token: superToken.address });
        carol = sf.user({ address: carolAddress, token: superToken.address });
    });

    describe("flows", () => {
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
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580", // 100 / mo
            });
            // validate flow data
            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address,
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
        it("create a new flow with onTransaction", async () => {
            let txHash;
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580", // 100 / mo
                onTransaction: hash => {
                    txHash = hash;
                },
            });
            assert.equal(txHash, tx.receipt.transactionHash);
        });
        it("create a new flow with User object argument", async () => {
            const tx = await alice.flow({
                // "bob" rather than "bob.address"
                recipient: bob,
                flowRate: "38580246913580", // 100 / mo
            });
            const flow = await sf.cfa.getFlow({
                superToken: superToken.address,
                sender: alice.address,
                receiver: bob.address,
            });
            const block = await web3.eth.getBlock(tx.receipt.blockNumber);
            assert.equal(flow.timestamp.getTime(), block.timestamp * 1000);
            assert.equal(flow.flowRate, "38580246913580");
        });
    });
});
