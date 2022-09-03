const traveler = require("ganache-time-traveler");
import {assert} from "chai";
import {ethers, web3} from "hardhat";
const {expectRevertedWith} = require("../../utils/expectRevert");

const TestEnvironment = require("../../TestEnvironment");
import {BatchLiquidator, SuperToken} from "../../../typechain-types";

describe("Superfluid Liquidator Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const FLOW_RATE = t.configs.FLOW_RATE1; // 1 per hour

    let superToken: SuperToken;
    let batch: BatchLiquidator;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 10,
        });
        const BatchLiquidatorFactory = await ethers.getContractFactory(
            "BatchLiquidator"
        );
        const admin = await ethers.getSigner(t.accounts[0]);
        batch = await BatchLiquidatorFactory.connect(admin).deploy();
        await t.pushEvmSnapshot();

        superToken = t.sf.tokens.TESTx;
    });

    after(async () => {
        await t.popEvmSnapshot();
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    async function timeTravelOnce(time: number) {
        const block1 = await web3.eth.getBlock("latest");
        console.log("current block time", block1.timestamp);
        console.log(`time traveler going to the future +${time}...`);
        await traveler.advanceTimeAndBlock(time);
        const block2 = await web3.eth.getBlock("latest");
        console.log("new block time", block2.timestamp);
    }

    describe("#1 Send multi liquidations", async () => {
        it("#1.1 Terminate all sender flows in one tx", async () => {
            const liquidatorBalance = await superToken.balanceOf(t.accounts[9]);
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            for (let i = 2; i <= 8; i++) {
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.accounts[1],
                    receiver: t.accounts[i],
                    flowRate: FLOW_RATE.toString(),
                });
            }

            await superToken.transferAll(t.accounts[5], {
                from: t.accounts[1],
            });
            await timeTravelOnce(5 * 3600);
            const signer = await ethers.getSigner(t.accounts[9]);
            await batch
                .connect(signer)
                .deleteFlows(
                    t.sf.host.address,
                    t.sf.agreements.cfa.address,
                    superToken.address,
                    Array(7).fill(t.accounts[1]),
                    t.accounts.slice(1, 8)
                );

            assert.ok(
                (await superToken.balanceOf(t.accounts[9])).gt(
                    liquidatorBalance
                )
            );
        });

        it("#1.2 Terminate all sender flows in one tx - rewards go to bond", async () => {
            const rewardAccount = await t.contracts.governance.getRewardAddress(
                t.sf.host.address,
                t.constants.ZERO_ADDRESS
            );

            const rewardBalance = await superToken.balanceOf(rewardAccount);
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            for (let i = 2; i <= 8; i++) {
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.accounts[1],
                    receiver: t.accounts[i],
                    flowRate: FLOW_RATE.toString(),
                });
            }

            await superToken.transferAll(t.accounts[5], {
                from: t.accounts[1],
            });
            await timeTravelOnce(1);
            const signer = await ethers.getSigner(t.accounts[0]);
            await batch
                .connect(signer)
                .deleteFlows(
                    t.sf.host.address,
                    t.sf.agreements.cfa.address,
                    superToken.address,
                    Array(7).fill(t.accounts[1]),
                    t.accounts.slice(1, 8)
                );
            assert.ok(
                (await superToken.balanceOf(rewardAccount)).gt(rewardBalance)
            );
        });

        it("#1.3 Terminate all sender flows in one tx with revert in batch", async () => {
            const liquidatorBalance = await superToken.balanceOf(t.accounts[9]);
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            for (let i = 2; i <= 8; i++) {
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.accounts[1],
                    receiver: t.accounts[i],
                    flowRate: FLOW_RATE.toString(),
                });
            }

            await t.sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: t.accounts[1],
                receiver: t.accounts[5],
            });

            await superToken.transferAll(t.accounts[5], {
                from: t.accounts[1],
            });
            await timeTravelOnce(5 * 3600);
            const signer = await ethers.getSigner(t.accounts[9]);
            await batch
                .connect(signer)
                .deleteFlows(
                    t.sf.host.address,
                    t.sf.agreements.cfa.address,
                    superToken.address,
                    Array(7).fill(t.accounts[1]),
                    t.accounts.slice(1, 8)
                );

            assert.ok(
                (await superToken.balanceOf(t.accounts[9])).gt(
                    liquidatorBalance
                )
            );
        });

        it("#1.4 Revert if size of senders and receivers don't match", async () => {
            await expectRevertedWith(
                batch.deleteFlows(
                    t.sf.host.address,
                    t.sf.agreements.cfa.address,
                    superToken.address,
                    Array(8).fill(t.accounts[1]),
                    t.accounts.slice(1, 5)
                ),
                "arrays different sizes"
            );
        });
    });
});
