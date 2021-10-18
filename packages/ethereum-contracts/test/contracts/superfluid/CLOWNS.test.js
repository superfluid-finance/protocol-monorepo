const { expectEvent, expectRevert } = require("@openzeppelin/test-helpers");
const { toBN } = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("../../TestEnvironment");
const traveler = require("ganache-time-traveler");

const CLOWNS = artifacts.require("CLOWNS");

describe("CLOWNS", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();
    const { ZERO_ADDRESS } = t.constants;

    let admin, alice, bob;

    let superfluid, erc1820, cfa;
    let clowns;
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
        await t.report({ title: "CLOWNS.test" });
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();

        clowns = await CLOWNS.new(superfluid.address, MIN_BOND_DURATION);
        console.log(`CLOWNS deployed at: ${clowns.address}`);
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
    function sendCLOBid(sender, token, bondAmount, exitRate) {
        const exitRateEncoded =
            exitRate !== undefined
                ? web3.eth.abi.encodeParameter("int96", exitRate)
                : "0x";
        return token.send(clowns.address, bondAmount, exitRateEncoded, {
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
            receiver: clowns.address,
            flowRate: flowrate.toString(),
        });

        await timeTravelOnce(time);

        await t.sf.cfa.deleteFlow({
            superToken: token.address,
            sender: admin,
            receiver: clowns.address,
            by: admin,
        });

        console.log(
            `CLOWNS balance after collectRewards: ${(
                await token.balanceOf(clowns.address)
            ).toString()}`
        );
    }

    // requires the exit stream to be in a critical state - will fail otherwise
    async function liquidateExitStream(token) {
        await t.sf.cfa.deleteFlow({
            superToken: token.address,
            sender: clowns.address,
            receiver: await clowns.getCurrentCLO(token.address),
            by: admin,
        });
    }

    // ================================================================================

    it("#1 alice becomes CLO", async () => {
        assert.equal(
            await clowns.getCurrentCLO(superToken.address),
            ZERO_ADDRESS
        );

        const r1 = await sendCLOBid(
            alice,
            superToken,
            BOND_AMOUNT_1E12,
            EXIT_RATE_1
        );

        expectEvent.inTransaction(r1.tx, clowns.contract, "NewCLO", {
            token: superToken.address,
            clo: alice,
            bond: BOND_AMOUNT_1E12.toString(),
            exitRate: EXIT_RATE_1.toString(),
        });

        const curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, alice);

        const { clo, remainingBond } = await clowns.getCurrentCLOInfo(
            superToken.address
        );
        assert.equal(clo, alice);
        assert.equal(remainingBond.toString(), BOND_AMOUNT_1E12.toString());

        await assertNetFlow(superToken, alice, EXIT_RATE_1);
    });

    it("#2 bob can outbid alice with higher bond", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1);

        let curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, alice);

        const remainingBond = (
            await clowns.getCurrentCLOInfo(superToken.address)
        ).remainingBond;
        console.log(`remaining bond: ${remainingBond}`);
        assert.equal(remainingBond.toString(), BOND_AMOUNT_1E12.toString());

        // fail with same amount (needs to be strictly greater)
        // fails to trigger if the timestamp is 1s off (happens sometimes randomly)
        // this detracting 1s exit flowrate
        await expectRevert(
            sendCLOBid(
                bob,
                superToken,
                BOND_AMOUNT_1E12 - EXIT_RATE_1,
                EXIT_RATE_1
            ),
            "CLOWNS: bid too low"
        );

        // should succeed with 1 wad more
        const r2 = await sendCLOBid(
            bob,
            superToken,
            BOND_AMOUNT_1E12 + 1,
            EXIT_RATE_1
        );

        curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, bob);
        assert.equal(
            (await clowns.getCurrentCLOInfo(superToken.address)).remainingBond,
            BOND_AMOUNT_1E12 + 1
        );

        expectEvent.inTransaction(r2.tx, clowns.contract, "NewCLO", {
            token: superToken.address,
            clo: bob,
            bond: (BOND_AMOUNT_1E12 + 1).toString(),
            exitRate: EXIT_RATE_1.toString(),
        });
    });

    // check erc1820 registration
    it("#3 CLOWNS registered with ERC1820", async () => {
        assert.equal(
            await erc1820.getInterfaceImplementer(
                clowns.address,
                web3.utils.soliditySha3("CLOWNSv1")
            ),
            clowns.address
        );
    });

    it("#4 enforce min exitRate limit", async () => {
        // lower limit: 0 wei/second (no negative value allowed)
        await expectRevert(
            sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, -1),
            "CLOWNS: negative exitRate not allowed"
        );

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, 0);
        await assertNetFlow(superToken, alice, 0);
    });

    it("#5 enforce max exitRate limit", async () => {
        const bondAmount = BOND_AMOUNT_1E12;
        const maxRate = shouldMaxExitRate(bondAmount);

        await expectRevert(
            sendCLOBid(alice, superToken, bondAmount, maxRate + 1),
            "CLOWNS: exitRate too high"
        );

        // exact upper limit
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, maxRate);
        await assertNetFlow(superToken, alice, maxRate);
    });

    it("#6 max and default exitRate are calculated correctly", async () => {
        assert.equal(
            shouldMaxExitRate(BOND_AMOUNT_1E12).toString(),
            await clowns.getMaxExitRateFor(superToken.address, BOND_AMOUNT_1E12)
        );

        assert.equal(
            shouldMaxExitRate(BOND_AMOUNT_2E12).toString(),
            await clowns.getMaxExitRateFor(superToken.address, BOND_AMOUNT_2E12)
        );

        // the default exit rate needs to be equal or greater than the max exit rate
        assert(
            toBN(shouldMaxExitRate(BOND_AMOUNT_2E12)).gte(
                await clowns.getDefaultExitRateFor(
                    superToken.address,
                    BOND_AMOUNT_2E12
                )
            )
        );
    });

    // interpret missing exitRate as default (min) exitRate
    it("#7 use default (max) exitRate as fallback if no exitRate specified", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12);
        await assertNetFlow(
            superToken,
            alice,
            await clowns.getDefaultExitRateFor(
                superToken.address,
                BOND_AMOUNT_1E12
            )
        );
    });

    // enforce min bid limit
    it("#8 cannot become CLO with smaller bond bid", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_2E12);

        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        await expectRevert(
            sendCLOBid(bob, superToken, BOND_AMOUNT_1E12),
            "CLOWNS: bid too low"
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

        const clownsBal1 = await superToken.balanceOf(clowns.address);
        console.log(`clowns bal pre close: ${clownsBal1.toString()}`);

        const clownsPrelimBal = await superToken.balanceOf(clowns.address);
        await sendCLOBid(alice, superToken, BOND_AMOUNT_2E12);
        const aliceBond = (await clowns.getCurrentCLOInfo(superToken.address))
            .remainingBond;

        // the tokens previously collected in the contract are attributed to Alice's bond
        assert.equal(
            aliceBond.toString(),
            clownsPrelimBal.toNumber() + BOND_AMOUNT_2E12
        );

        const alicePreOutbidBal = await superToken.balanceOf(alice);
        await sendCLOBid(bob, superToken, BOND_AMOUNT_10E12);

        // the tokens previously collected are paid out to Alice if outbid
        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            alicePreOutbidBal.add(aliceBond).toString()
        );
    });

    // let current CLO re-bid
    it("#10 alice can outbid herself", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12);
        const aliceIntermediateBal = await superToken.balanceOf(alice);
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12 + 1);

        assert.equal(await clowns.getCurrentCLO(superToken.address), alice);
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
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12);

        await expectRevert(
            clowns.changeExitRate(superToken.address, EXIT_RATE_1, {
                from: bob,
            }),
            "CLOWNS: only CLO allowed"
        );

        // don't allow negative exitRate
        await expectRevert(
            clowns.changeExitRate(superToken.address, -1, { from: alice }),
            "CLOWNS: negative exitRate not allowed"
        );

        // lower to 1 wad
        const r = await clowns.changeExitRate(superToken.address, EXIT_RATE_1, {
            from: alice,
        });
        await assertNetFlow(superToken, alice, EXIT_RATE_1);
        expectEvent.inTransaction(r.tx, clowns.contract, "ExitRateChanged", {
            token: superToken.address,
            exitRate: EXIT_RATE_1.toString(),
        });

        // increase to 1000 wad
        await clowns.changeExitRate(superToken.address, EXIT_RATE_1E3, {
            from: alice,
        });
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        // to 0
        await clowns.changeExitRate(superToken.address, 0, { from: alice });
        await assertNetFlow(superToken, alice, 0);

        const remainingBond = (
            await clowns.getCurrentCLOInfo(superToken.address)
        ).remainingBond;
        console.log(`remaining bond ${remainingBond}`);
        // increase to currently allowed max
        const max1 = shouldMaxExitRate(remainingBond);
        await clowns.changeExitRate(superToken.address, max1, { from: alice });
        await assertNetFlow(superToken, alice, max1);

        // due to the exit flow, the remaining bond changes with every new block
        const max2 = shouldMaxExitRate(remainingBond);
        assert.equal(max1.toString(), max2.toString());
        await expectRevert(
            clowns.changeExitRate(superToken.address, max2 + 1, {
                from: alice,
            }),
            "CLOWNS: exitRate too high"
        );
    });

    // CLO closes stream: nothing breaks - can reopen with changeFlowrate
    it("#12 can recover from closed exit flow", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E3);
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        await timeTravelOnce(1000);
        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: clowns.address,
            receiver: alice,
            by: alice,
        });
        await assertNetFlow(superToken, alice, 0);

        // 0 -> 0 (leave unchanged)
        await clowns.changeExitRate(superToken.address, 0, { from: alice });
        await assertNetFlow(superToken, alice, 0);

        // trigger re-opening
        await clowns.changeExitRate(superToken.address, EXIT_RATE_1, {
            from: alice,
        });
        await assertNetFlow(superToken, alice, EXIT_RATE_1);

        // stop again and let bob make a bid
        await timeTravelOnce(1000);
        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: clowns.address,
            receiver: alice,
            by: alice,
        });
        await assertNetFlow(superToken, alice, 0);

        await sendCLOBid(bob, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E3);
        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, bob, EXIT_RATE_1E3);
    });

    // collected rewards are added added to the CLO bond
    it("#13 collected rewards are added added to the CLO bond", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, 0);

        await collectRewards(superToken, 1e6, 1e6);
        // 1e6 token/s x 1e6 seconds = ~1e12 tokens collected in the contract

        assert.isTrue(
            (
                await clowns.getCurrentCLOInfo(superToken.address)
            ).remainingBond.toNumber() >=
                BOND_AMOUNT_1E12 + 1e12
        );
        // can't rely on that bcs the block timestamp is sometimes 1s higher
        /*
        assert.equal(
            (await clowns.getCurrentCLOInfo(superToken.address)).remainingBond.toString(),
            (BOND_AMOUNT_1E12 + 1e12).toString()
        );
         */
    });

    // bond is consumed
    it("#14 bond is consumed by exit flow", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E6);
        await assertNetFlow(superToken, alice, EXIT_RATE_1E6);

        // critical stream is liquidated - remaining bond goes to zero
        await timeTravelOnce(1e6);
        await liquidateExitStream(superToken);
        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, clowns.address, 0);

        const remB = (await clowns.getCurrentCLOInfo(superToken.address))
            .remainingBond;
        assert.equal(remB.toString(), "0"); // remaining deposit is taken

        // alice tries to re-establish stream - fail because no bond left
        await expectRevert(
            clowns.changeExitRate(superToken.address, EXIT_RATE_1, {
                from: alice,
            }),
            "CLOWNS: exitRate too high"
        );

        // more rewards, alice can re-establish stream
        await collectRewards(superToken, 1e9, 1e3);
        assert.isTrue(
            (
                await clowns.getCurrentCLOInfo(superToken.address)
            ).remainingBond.toNumber() >= 1e12
        );

        await clowns.changeExitRate(superToken.address, EXIT_RATE_1E3, {
            from: alice,
        });
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        const alicePreBal = await superToken.balanceOf(alice);
        const aliceBondLeft = (
            await clowns.getCurrentCLOInfo(superToken.address)
        ).remainingBond;

        // bob outbids
        await expectRevert(
            sendCLOBid(bob, superToken, 1, 0),
            "CLOWNS: bid too low"
        );
        await sendCLOBid(bob, superToken, BOND_AMOUNT_2E12, EXIT_RATE_1E6);
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
        await clowns.changeExitRate(superToken.address, 0, { from: bob });
        const cloInfo = await clowns.getCurrentCLOInfo(superToken.address);
        console.log(`cloInfo ${JSON.stringify(cloInfo, null, 2)}`);
        console.log(`remainingBond: ${cloInfo.remainingBond}`);
        // TODO: why is there an overflow or underflow (-2^32) here?

        /*
        const { clo, remainingBond, exitRate } = await clowns.getCurrentCLOInfo(
            superToken.address
        );
        const remBond = (await clowns.getCurrentCLOInfo(superToken.address))
            .remainingBond;
        console.log(`remBond: ${remBond}, cloRemBond ${remainingBond.toString()}, exitRate: ${exitRate}`);
         */

        console.log(
            `clowns balance: ${JSON.stringify(
                await superToken.realtimeBalanceOfNow(clowns.address),
                null,
                2
            )}`
        );

        // since the stream was closed through changeExitRate() by the contract (the sender) itself,
        // after already being insolvent, the contract got full deposit amount as reward.

        /*
        assert.equal(
            (await clowns.getCurrentCLOInfo(superToken.address)).remainingBond.toString(),
            "0"
        );
         */

        // alice can place a successful bid of 1 wad - NOT
        //await sendCLOBid(alice, superToken, 1, 0);
    });

    // multiple tokens
    it("#15 multiple CLOs (one per token) in parallel", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        const superToken2 = (
            await t.deployNewToken("TEST2", {
                doUpgrade: true,
                isTruffle: true,
            })
        ).superToken;

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E3);
        await sendCLOBid(bob, superToken2, BOND_AMOUNT_2E12, EXIT_RATE_1);

        assert.equal(await clowns.getCurrentCLO(superToken.address), alice);
        assert.equal(await clowns.getCurrentCLO(superToken2.address), bob);

        await expectRevert(
            clowns.changeExitRate(superToken2.address, 0, { from: alice }),
            "CLOWNS: only CLO allowed"
        );
        await expectRevert(
            clowns.changeExitRate(superToken.address, 0, { from: bob }),
            "CLOWNS: only CLO allowed"
        );

        // let this run for a while...
        await timeTravelOnce(1e6);

        // alice takes over superToken2
        const bobPreBal = await superToken2.balanceOf(bob);
        const bobBondLeft = (
            await clowns.getCurrentCLOInfo(superToken2.address)
        ).remainingBond;

        await expectRevert(
            sendCLOBid(alice, superToken2, BOND_AMOUNT_1E12, EXIT_RATE_1),
            "CLOWNS: bid too low"
        );

        await sendCLOBid(alice, superToken2, BOND_AMOUNT_10E12, EXIT_RATE_1E3);

        assert.equal(
            bobPreBal.add(bobBondLeft).toString(),
            (await superToken2.balanceOf(bob)).toString()
        );
    });

    it("#16 CLO can outbid themselves", async () => {
        const alicePreBal = await superToken.balanceOf(alice);

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1E3);
        await timeTravelOnce(1e6);
        await sendCLOBid(alice, superToken, BOND_AMOUNT_2E12, 0);
        assert.equal(await clowns.getCurrentCLO(superToken.address), alice);

        const remainingBond = (
            await clowns.getCurrentCLOInfo(superToken.address)
        ).remainingBond;
        assert.equal(remainingBond.toString(), BOND_AMOUNT_2E12.toString());

        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            alicePreBal.sub(toBN(BOND_AMOUNT_2E12)).toString()
        );
    });
});
