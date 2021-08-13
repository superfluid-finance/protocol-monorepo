const { web3tx, toWad } = require("@decentral.ee/web3-helpers");

const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const traveler = require("ganache-time-traveler");

const chai = require("chai");
const chaiAsPromised = require("chai-as-promised");
chai.use(chaiAsPromised);

const TEST_TRAVEL_TIME = 3600 * 24; // 24 hours
const INIT_BALANCE = toWad(100);

contract("ConstantFlowAgreementV1", (accounts) => {
    const [aliceAddress, bobAddress, carolAddress] = accounts;

    let sf;
    let dai;
    let daix;
    let alice;
    let bob;

    before(async function () {
        sf = new SuperfluidSDK.Framework({
            web3,
            version: "test",
            tokens: ["fDAI"],
        });
        await sf.initialize();

        const daiAddress = await sf.tokens.fDAI.address;
        dai = await sf.contracts.TestToken.at(daiAddress);
        daix = sf.tokens.fDAIx;
        await Promise.all(
            accounts.map(async (account, i) => {
                await web3tx(dai.mint, `Account ${i} mints many dai`)(
                    accounts[i],
                    INIT_BALANCE,
                    { from: account }
                );
                await web3tx(dai.approve, `Account ${i} approves daix`)(
                    daix.address,
                    toWad(100),
                    { from: account }
                );
            })
        );

        await daix.upgrade(INIT_BALANCE, { from: aliceAddress });
        await daix.upgrade(INIT_BALANCE, { from: bobAddress });
        await daix.upgrade(INIT_BALANCE, { from: carolAddress });

        alice = sf.user({ address: aliceAddress, token: daix.address });
        bob = sf.user({ address: bobAddress, token: daix.address });
    });

    async function _timeTravelOnce(time = TEST_TRAVEL_TIME) {
        const block1 = await web3.eth.getBlock("latest");
        console.log("current block time", block1.timestamp);
        console.log(`time traveler going to the future +${time}...`);
        await traveler.advanceTimeAndBlock(time);
        const block2 = await web3.eth.getBlock("latest");
        console.log("new block time", block2.timestamp);
    }

    describe("flows", () => {
        it("create a new flow", async () => {
            const tx = await alice.flow({
                recipient: bob.address,
                flowRate: "38580246913580", // 100 / mo
            });
            // validate flow data
            const flow = await sf.cfa.getFlow({
                superToken: daix.address,
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
                        superToken: daix.address,
                        account: alice.address,
                    })
                ).toString(),
                "-38580246913580"
            );
            assert.equal(
                (
                    await sf.cfa.getNetFlow({
                        superToken: daix.address,
                        account: bob.address,
                    })
                ).toString(),
                "38580246913580"
            );
            await _timeTravelOnce();
        });
        it("create a new flow with User object argument", async () => {
            const tx = await alice.flow({
                // "bob" rather than "bob.address"
                recipient: bob,
                flowRate: "38580246913580", // 100 / mo
            });
            const flow = await sf.cfa.getFlow({
                superToken: daix.address,
                sender: alice.address,
                receiver: bob.address,
            });
            const block = await web3.eth.getBlock(tx.receipt.blockNumber);
            assert.equal(flow.timestamp.getTime(), block.timestamp * 1000);
            assert.equal(flow.flowRate, "38580246913580");
        });
    });
});
