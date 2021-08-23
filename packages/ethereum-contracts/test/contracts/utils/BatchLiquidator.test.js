const { expectRevert } = require("@openzeppelin/test-helpers");

const TestEnvironment = require("../../TestEnvironment");
const BatchLiquidator = artifacts.require("BatchLiquidator");

const { toWad, toBN } = require("@decentral.ee/web3-helpers");

const traveler = require("ganache-time-traveler");
contract("Superfluid Liquidator Contract", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 9), {
        isTruffle: true,
        useMocks: true,
    });

    const FLOW_RATE = toWad("1").div(toBN(3600)); // 1 per hour
    const { admin, alice } = t.aliases;
    let superToken;
    let batch;

    before(async () => {
        await t.reset();
        batch = await BatchLiquidator.new({ from: accounts[0] });
    });

    beforeEach(async function () {
        await t.createNewToken({ doUpgrade: false });
        ({ superToken } = t.contracts);
    });

    async function timeTravelOnce(time) {
        const block1 = await web3.eth.getBlock("latest");
        console.log("current block time", block1.timestamp);
        console.log(`time traveler going to the future +${time}...`);
        await traveler.advanceTimeAndBlock(time);
        const block2 = await web3.eth.getBlock("latest");
        console.log("new block time", block2.timestamp);
    }

    describe("#1 Send multi liquidations", async () => {
        it("#1.1 Terminate all sender flows in one tx", async () => {
            const liquidatorBalance = await superToken.balanceOf(admin);
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            for (let i = 2; i <= 9; i++) {
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: accounts[1],
                    receiver: accounts[i],
                    flowRate: FLOW_RATE.toString(),
                });
            }

            await superToken.transferAll(accounts[9], { from: alice });
            await timeTravelOnce(3600);
            await batch.deleteFlows(
                t.sf.host.address,
                t.sf.agreements.cfa.address,
                superToken.address,
                Array(8).fill(alice),
                accounts.slice(1, 9),
                { from: admin }
            );

            assert.ok(
                (await superToken.balanceOf(admin)).gt(liquidatorBalance)
            );
        });

        it("#1.2 Terminate all sender flows in one tx with revert in batch", async () => {
            const liquidatorBalance = await superToken.balanceOf(admin);
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            for (let i = 2; i <= 9; i++) {
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: accounts[1],
                    receiver: accounts[i],
                    flowRate: FLOW_RATE.toString(),
                });
            }

            await t.sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: accounts[1],
                receiver: accounts[5],
            });

            await superToken.transferAll(accounts[9], { from: alice });
            await timeTravelOnce(3600);
            await batch.deleteFlows(
                t.sf.host.address,
                t.sf.agreements.cfa.address,
                superToken.address,
                Array(8).fill(alice),
                accounts.slice(1, 9),
                { from: admin }
            );

            assert.ok(
                (await superToken.balanceOf(admin)).gt(liquidatorBalance)
            );
        });

        it("#1.3 Revert if size of senders and receivers don't match", async () => {
            await expectRevert(
                batch.deleteFlows(
                    t.sf.host.address,
                    t.sf.agreements.cfa.address,
                    superToken.address,
                    Array(8).fill(alice),
                    accounts.slice(1, 5)
                ),
                "arrays different sizes"
            );
        });
    });
});
