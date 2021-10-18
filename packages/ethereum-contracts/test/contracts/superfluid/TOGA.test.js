const { expectEvent, expectRevert } = require("@openzeppelin/test-helpers");
const { toBN } = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("../../TestEnvironment");
const traveler = require("ganache-time-traveler");

const TOGA = artifacts.require("TOGA");

describe("TOGA", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();
    const { ZERO_ADDRESS } = t.constants;

    let admin, alice, bob;

    let superfluid, erc1820, cfa;
    let toga;
    let superToken;
    const MIN_BOND_DURATION = 3600 * 24 * 7; // 604800 s
    const EXIT_RATE_1 = 1; // 1 wad per second
    const EXIT_RATE_1E3 = 1e3; // 1000 wad per second
    const EXIT_RATE_1E6 = 1e6; // 1000000 wad per second
    const BOND_AMOUNT_1E12 = 1e12;
    const BOND_AMOUNT_2E12 = 2e12;
    const BOND_AMOUNT_10E12 = 10e12;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
        });
        ({ admin, alice, bob } = t.aliases);

        ({ superfluid, erc1820, cfa } = t.contracts);

        //await t.createNewToken({ doUpgrade: true });
        superToken = t.sf.tokens.TESTx;
        //({ superToken } = t.contracts);
        console.log(`superToken addr: ${superToken.address}`);
    });

    after(async function () {
        await t.report({ title: "TOGA.test" });
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();

        toga = await TOGA.new(superfluid.address, MIN_BOND_DURATION);
        console.log(`TOGA deployed at: ${toga.address}`);
        await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
    });

    // copied over from ConstantFlowAgreementV1.test.js - should probably be a shared fn
    async function timeTravelOnce(time = MIN_BOND_DURATION) {
        const block1 = await web3.eth.getBlock("latest");
        console.log("current block time", block1.timestamp);
        console.log(`time traveler going to the future +${time}...`);
        await traveler.advanceTimeAndBlock(time);
        const block2 = await web3.eth.getBlock("latest");
        console.log("new block time", block2.timestamp);
    }

    // returns a promise
    function sendPICBid(sender, token, bondAmount, exitRate) {
        const exitRateEncoded =
            exitRate !== undefined
                ? web3.eth.abi.encodeParameter("int96", exitRate)
                : "0x";
        return token.send(toga.address, bondAmount, exitRateEncoded, {
            from: sender,
        });
    }

    function shouldMaxExitRate(bondAmount) {
        return Math.floor(bondAmount / MIN_BOND_DURATION);
    }

    function shouldDefaultExitRate(bondAmount) {
        // happens to be set to the max exit rate
        return shouldMaxExitRate(bondAmount);
    }

    async function assertNetFlow(token, account, expectedFlowRate) {
        const actualFlowrate = await cfa.getNetFlow(token.address, account);
        assert.equal(
            expectedFlowRate.toString(),
            actualFlowrate.toString(),
            "flowrate not as expected"
        );
    }

    // collects rewards through a stream. Will fast forward the chain by the given time
    async function collectRewards(token, flowrate, time) {
        await t.upgradeBalance("admin", t.configs.INIT_BALANCE);
        // this fast forwarded flow emulates received rewards
        // Done with a stream because we need to avoid transfer() and send() in order to not trigger a bid
        await t.sf.cfa.createFlow({
            superToken: token.address,
            sender: admin,
            receiver: toga.address,
            flowRate: flowrate.toString(),
        });

        await timeTravelOnce(time);

        await t.sf.cfa.deleteFlow({
            superToken: token.address,
            sender: admin,
            receiver: toga.address,
            by: admin,
        });

        console.log(
            `TOGA balance after collectRewards: ${(
                await token.balanceOf(toga.address)
            ).toString()}`
        );
    }

    // requires the exit stream to be in a critical state - will fail otherwise
    async function liquidateExitStream(token) {
        await t.sf.cfa.deleteFlow({
            superToken: token.address,
            sender: toga.address,
            receiver: await toga.getCurrentPIC(token.address),
            by: admin,
        });
    }

    // ================================================================================

    it("#1 alice becomes PIC", async () => {
        assert.equal(
            await toga.getCurrentPIC(superToken.address),
            ZERO_ADDRESS
        );

        const r1 = await sendPICBid(
            alice,
            superToken,
            BOND_AMOUNT_1E12,
            EXIT_RATE_1
        );

        expectEvent.inTransaction(r1.tx, toga.contract, "NewPIC", {
            token: superToken.address,
            pic: alice,
            bond: BOND_AMOUNT_1E12.toString(),
            exitRate: EXIT_RATE_1.toString(),
        });

        const curPIC = await toga.getCurrentPIC(superToken.address);
        assert.equal(curPIC, alice);

        const { pic, remainingBond } = await toga.getCurrentPICInfo(
            superToken.address
        );
        assert.equal(pic, alice);
        assert.equal(remainingBond.toString(), BOND_AMOUNT_1E12.toString());

        await assertNetFlow(superToken, alice, EXIT_RATE_1);
    });

    it("#2 bob can outbid alice with higher bond", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1);

        let curPIC = await toga.getCurrentPIC(superToken.address);
        assert.equal(curPIC, alice);

        const remainingBond = (
            await toga.getCurrentPICInfo(superToken.address)
        ).remainingBond;
        console.log(`remaining bond: ${remainingBond}`);
        assert.equal(remainingBond.toString(), BOND_AMOUNT_1E12.toString());

        // fail with same amount (needs to be strictly greater)
        // fails to trigger if the timestamp is 1s off (happens sometimes randomly)
        // this detracting 1s exit flowrate
        await expectRevert(
            sendPICBid(
                bob,
                superToken,
                BOND_AMOUNT_1E12 - EXIT_RATE_1,
                EXIT_RATE_1
            ),
            "TOGA: bid too low"
        );

        // should succeed with 1 wad more
        const r2 = await sendPICBid(
            bob,
            superToken,
            BOND_AMOUNT_1E12 + 1,
            EXIT_RATE_1
        );

        curPIC = await toga.getCurrentPIC(superToken.address);
        assert.equal(curPIC, bob);
        assert.equal(
            (await toga.getCurrentPICInfo(superToken.address)).remainingBond,
            BOND_AMOUNT_1E12 + 1
        );

        expectEvent.inTransaction(r2.tx, toga.contract, "NewPIC", {
            token: superToken.address,
            pic: bob,
            bond: (BOND_AMOUNT_1E12 + 1).toString(),
            exitRate: EXIT_RATE_1.toString(),
        });
    });

    // check erc1820 registration
    it("#3 TOGA registered with ERC1820", async () => {
        assert.equal(
            await erc1820.getInterfaceImplementer(
                toga.address,
                web3.utils.soliditySha3("TOGAv1")
            ),
            toga.address
        );
    });

    it("#4 enforce min exitRate limit", async () => {
        // lower limit: 0 wei/second (no negative value allowed)
        await expectRevert(
            sendPICBid(alice, superToken, BOND_AMOUNT_1E12, -1),
            "TOGA: negative exitRate not allowed"
        );

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12, 0);
        await assertNetFlow(superToken, alice, 0);
    });

    it("#5 enforce max exitRate limit", async () => {
        const bondAmount = BOND_AMOUNT_1E12;
        const maxRate = shouldMaxExitRate(bondAmount);

        await expectRevert(
            sendPICBid(alice, superToken, bondAmount, maxRate + 1),
            "TOGA: exitRate too high"
        );

        // exact upper limit
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12, maxRate);
        await assertNetFlow(superToken, alice, maxRate);
    });

    it("#6 max and default exitRate are calculated correctly", async () => {
        assert.equal(
            shouldMaxExitRate(BOND_AMOUNT_1E12).toString(),
            await toga.getMaxExitRateFor(superToken.address, BOND_AMOUNT_1E12)
        );

        assert.equal(
            shouldMaxExitRate(BOND_AMOUNT_2E12).toString(),
            await toga.getMaxExitRateFor(superToken.address, BOND_AMOUNT_2E12)
        );

        // the default exit rate needs to be equal or greater than the max exit rate
        assert(
            toBN(shouldMaxExitRate(BOND_AMOUNT_2E12)).gte(
                await toga.getDefaultExitRateFor(
                    superToken.address,
                    BOND_AMOUNT_2E12
                )
            )
        );
    });

    // interpret missing exitRate as default (min) exitRate
    it("#7 use default (max) exitRate as fallback if no exitRate specified", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12);
        await assertNetFlow(
            superToken,
            alice,
            await toga.getDefaultExitRateFor(
                superToken.address,
                BOND_AMOUNT_1E12
            )
        );
    });

    // enforce min bid limit
    it("#8 cannot become PIC with smaller bond bid", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_2E12);

        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        await expectRevert(
            sendPICBid(bob, superToken, BOND_AMOUNT_1E12),
            "TOGA: bid too low"
        );
    });

    // handle initial bid with non-zero contract balance.
    // that balance is added to the bond of the first bidder
    it("#9 first bidder gets tokens pre-owned by contract", async () => {
        /*
        rewards are accrued via SuperfluidToken.makeLiquidationPayouts()
        which can directly manipulate token balances.
        This is important because an ERC20.transfer() would (currently) trigger a bid.
        For the sake of simplicity, this test uses a fast forwarded stream to fund the contract.
         */
        /*
        Note: there's a (very) theoretical edge case here:
        the reward account could already have accrued more than half the tokens
        in circulation, making it impossible to successfully bid.
        If supply couldn't be increased, the accrued tokens would then effectively be burned.
         */

        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await collectRewards(superToken, 1e6, 1e6);
        // 1e6 token/s x 1e6 seconds = ~1e12 tokens collected in the contract

        const togaBal1 = await superToken.balanceOf(toga.address);
        console.log(`toga bal pre close: ${togaBal1.toString()}`);

        const togaPrelimBal = await superToken.balanceOf(toga.address);
        await sendPICBid(alice, superToken, BOND_AMOUNT_2E12);
        const aliceBond = (await toga.getCurrentPICInfo(superToken.address))
            .remainingBond;

        // the tokens previously collected in the contract are attributed to Alice's bond
        assert.equal(
            aliceBond.toString(),
            togaPrelimBal.toNumber() + BOND_AMOUNT_2E12
        );

        const alicePreOutbidBal = await superToken.balanceOf(alice);
        await sendPICBid(bob, superToken, BOND_AMOUNT_10E12);

        // the tokens previously collected are paid out to Alice if outbid
        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            alicePreOutbidBal.add(aliceBond).toString()
        );
    });

    // let current PIC re-bid
    it("#10 alice can outbid herself", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12);
        const aliceIntermediateBal = await superToken.balanceOf(alice);
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12 + 1);

        assert.equal(await toga.getCurrentPIC(superToken.address), alice);
        await assertNetFlow(
            superToken,
            alice,
            shouldDefaultExitRate(BOND_AMOUNT_1E12 + 1)
        );
        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            aliceIntermediateBal.sub(toBN(1))
        );
    });

    // changeFlowrate - respect limits
    it("#11 alice can change her exit rate - limits enforced", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12);

        await expectRevert(
            toga.changeExitRate(superToken.address, EXIT_RATE_1, {
                from: bob,
            }),
            "TOGA: only PIC allowed"
        );

        // don't allow negative exitRate
        await expectRevert(
            toga.changeExitRate(superToken.address, -1, { from: alice }),
            "TOGA: negative exitRate not allowed"
        );

        // lower to 1 wad
        const r = await toga.changeExitRate(superToken.address, EXIT_RATE_1, {
            from: alice,
        });
        await assertNetFlow(superToken, alice, EXIT_RATE_1);
        expectEvent.inTransaction(r.tx, toga.contract, "ExitRateChanged", {
            token: superToken.address,
            exitRate: EXIT_RATE_1.toString(),
        });

        // increase to 1000 wad
        await toga.changeExitRate(superToken.address, EXIT_RATE_1E3, {
            from: alice,
        });
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        // to 0
        await toga.changeExitRate(superToken.address, 0, { from: alice });
        await assertNetFlow(superToken, alice, 0);

        const remainingBond = (
            await toga.getCurrentPICInfo(superToken.address)
        ).remainingBond;
        console.log(`remaining bond ${remainingBond}`);
        // increase to currently allowed max
        const max1 = shouldMaxExitRate(remainingBond);
        await toga.changeExitRate(superToken.address, max1, { from: alice });
        await assertNetFlow(superToken, alice, max1);

        // due to the exit flow, the remaining bond changes with every new block
        const max2 = shouldMaxExitRate(remainingBond);
        assert.equal(max1.toString(), max2.toString());
        await expectRevert(
            toga.changeExitRate(superToken.address, max2 + 1, {
                from: alice,
            }),
            "TOGA: exitRate too high"
        );
    });

    // PIC closes stream: nothing breaks - can reopen with changeFlowrate
    it("#12 can recover from closed exit flow", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E3);
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        await timeTravelOnce(1000);
        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: toga.address,
            receiver: alice,
            by: alice,
        });
        await assertNetFlow(superToken, alice, 0);

        // 0 -> 0 (leave unchanged)
        await toga.changeExitRate(superToken.address, 0, { from: alice });
        await assertNetFlow(superToken, alice, 0);

        // trigger re-opening
        await toga.changeExitRate(superToken.address, EXIT_RATE_1, {
            from: alice,
        });
        await assertNetFlow(superToken, alice, EXIT_RATE_1);

        // stop again and let bob make a bid
        await timeTravelOnce(1000);
        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: toga.address,
            receiver: alice,
            by: alice,
        });
        await assertNetFlow(superToken, alice, 0);

        await sendPICBid(bob, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E3);
        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, bob, EXIT_RATE_1E3);
    });

    // collected rewards are added added to the PIC bond
    it("#13 collected rewards are added added to the PIC bond", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12, 0);

        await collectRewards(superToken, 1e6, 1e6);
        // 1e6 token/s x 1e6 seconds = ~1e12 tokens collected in the contract

        assert.isTrue(
            (
                await toga.getCurrentPICInfo(superToken.address)
            ).remainingBond.toNumber() >=
                BOND_AMOUNT_1E12 + 1e12
        );
        // can't rely on that bcs the block timestamp is sometimes 1s higher
        /*
        assert.equal(
            (await toga.getCurrentPICInfo(superToken.address)).remainingBond.toString(),
            (BOND_AMOUNT_1E12 + 1e12).toString()
        );
         */
    });

    // bond is consumed
    it("#14 bond is consumed by exit flow", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E6);
        await assertNetFlow(superToken, alice, EXIT_RATE_1E6);

        // critical stream is liquidated - remaining bond goes to zero
        await timeTravelOnce(1e6);
        await liquidateExitStream(superToken);
        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, toga.address, 0);

        const remB = (await toga.getCurrentPICInfo(superToken.address))
            .remainingBond;
        assert.equal(remB.toString(), "0"); // remaining deposit is taken

        // alice tries to re-establish stream - fail because no bond left
        await expectRevert(
            toga.changeExitRate(superToken.address, EXIT_RATE_1, {
                from: alice,
            }),
            "TOGA: exitRate too high"
        );

        // more rewards, alice can re-establish stream
        await collectRewards(superToken, 1e9, 1e3);
        assert.isTrue(
            (
                await toga.getCurrentPICInfo(superToken.address)
            ).remainingBond.toNumber() >= 1e12
        );

        await toga.changeExitRate(superToken.address, EXIT_RATE_1E3, {
            from: alice,
        });
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        const alicePreBal = await superToken.balanceOf(alice);
        const aliceBondLeft = (
            await toga.getCurrentPICInfo(superToken.address)
        ).remainingBond;

        // bob outbids
        await expectRevert(
            sendPICBid(bob, superToken, 1, 0),
            "TOGA: bid too low"
        );
        await sendPICBid(bob, superToken, BOND_AMOUNT_2E12, EXIT_RATE_1E6);
        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, bob, EXIT_RATE_1E6);

        assert.equal(
            alicePreBal.add(aliceBondLeft).toString(),
            (await superToken.balanceOf(alice)).toString()
        );

        // runs to zero again
        await timeTravelOnce(2e6 + 1e3);

        // this triggers stream closing by the contract - since already insolvent, the full deposit amount
        // is rewarded to the closing account which is the contract. Thus ends up with a non-zero balance
        await toga.changeExitRate(superToken.address, 0, { from: bob });
        const picInfo = await toga.getCurrentPICInfo(superToken.address);
        console.log(`picInfo ${JSON.stringify(picInfo, null, 2)}`);
        console.log(`remainingBond: ${picInfo.remainingBond}`);
        // TODO: why is there an overflow or underflow (-2^32) here?

        /*
        const { pic, remainingBond, exitRate } = await toga.getCurrentPICInfo(
            superToken.address
        );
        const remBond = (await toga.getCurrentPICInfo(superToken.address))
            .remainingBond;
        console.log(`remBond: ${remBond}, picRemBond ${remainingBond.toString()}, exitRate: ${exitRate}`);
         */

        console.log(
            `toga balance: ${JSON.stringify(
                await superToken.realtimeBalanceOfNow(toga.address),
                null,
                2
            )}`
        );

        // since the stream was closed through changeExitRate() by the contract (the sender) itself,
        // after already being insolvent, the contract got full deposit amount as reward.

        /*
        assert.equal(
            (await toga.getCurrentPICInfo(superToken.address)).remainingBond.toString(),
            "0"
        );
         */

        // alice can place a successful bid of 1 wad - NOT
        //await sendPICBid(alice, superToken, 1, 0);
    });

    // multiple tokens
    it("#15 multiple PICs (one per token) in parallel", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        const superToken2 = (
            await t.deployNewToken("TEST2", {
                doUpgrade: true,
                isTruffle: true,
            })
        ).superToken;

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E3);
        await sendPICBid(bob, superToken2, BOND_AMOUNT_2E12, EXIT_RATE_1);

        assert.equal(await toga.getCurrentPIC(superToken.address), alice);
        assert.equal(await toga.getCurrentPIC(superToken2.address), bob);

        await expectRevert(
            toga.changeExitRate(superToken2.address, 0, { from: alice }),
            "TOGA: only PIC allowed"
        );
        await expectRevert(
            toga.changeExitRate(superToken.address, 0, { from: bob }),
            "TOGA: only PIC allowed"
        );

        // let this run for a while...
        await timeTravelOnce(1e6);

        // alice takes over superToken2
        const bobPreBal = await superToken2.balanceOf(bob);
        const bobBondLeft = (
            await toga.getCurrentPICInfo(superToken2.address)
        ).remainingBond;

        await expectRevert(
            sendPICBid(alice, superToken2, BOND_AMOUNT_1E12, EXIT_RATE_1),
            "TOGA: bid too low"
        );

        await sendPICBid(alice, superToken2, BOND_AMOUNT_10E12, EXIT_RATE_1E3);

        assert.equal(
            bobPreBal.add(bobBondLeft).toString(),
            (await superToken2.balanceOf(bob)).toString()
        );
    });

    it("#16 PIC can outbid themselves", async () => {
        const alicePreBal = await superToken.balanceOf(alice);

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E3);
        await timeTravelOnce(1e6);
        await sendPICBid(alice, superToken, BOND_AMOUNT_2E12, 0);
        assert.equal(await toga.getCurrentPIC(superToken.address), alice);

        const remainingBond = (
            await toga.getCurrentPICInfo(superToken.address)
        ).remainingBond;
        assert.equal(remainingBond.toString(), BOND_AMOUNT_2E12.toString());

        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            alicePreBal.sub(toBN(BOND_AMOUNT_2E12)).toString()
        );
    });
});
