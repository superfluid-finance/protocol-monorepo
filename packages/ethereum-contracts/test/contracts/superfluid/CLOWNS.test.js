const { expectEvent, expectRevert } = require("@openzeppelin/test-helpers");
const { toBN } = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("../../TestEnvironment");
const traveler = require("ganache-time-traveler");

const CLOWNS = artifacts.require("CLOWNS");

describe.only("CLOWNS", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();
    const { ZERO_ADDRESS } = t.constants;

    let admin, alice, bob, carol;

    let superfluid, erc1820, cfa;
    let clowns;
    let superToken;
    const MIN_BOND_DURATION = 3600 * 24 * 7;
    const EXIT_RATE_1 = 1; // 1 wad per second
    const EXIT_RATE_1000 = 1000; // 1000 wad per second
    const BOND_AMOUNT_1E12 = 1E12;
    const BOND_AMOUNT_2E12 = 2E12;
    const BOND_AMOUNT_10E12 = 10E12;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({ admin, alice, bob, carol } = t.aliases);

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

    function sendCLOBid(sender, token, bondAmount, exitRate) {
        const exitRateEncoded = exitRate !== undefined ?
            web3.eth.abi.encodeParameter("int96", exitRate) :
            "0x";
        return token.send(clowns.address, bondAmount, exitRateEncoded, { from: sender });
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

    it("#1 alice becomes CLO", async () => {
        assert.equal(await clowns.getCurrentCLO(superToken.address), ZERO_ADDRESS);

        const r1 = await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1);

        expectEvent.inTransaction(r1.tx, clowns.contract, "NewCLO", {
            token: superToken.address,
            clo: alice,
            bond: BOND_AMOUNT_1E12.toString(),
            exitRate: EXIT_RATE_1.toString(),
        });

        const curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, alice);

        const { clo, remainingBond } = await clowns.getCurrentCLOBond(superToken.address);
        assert.equal(clo, alice);
        assert.equal(remainingBond.toString(), BOND_AMOUNT_1E12.toString());

        await assertNetFlow(superToken, alice, EXIT_RATE_1);
    });

    it("#2 bob can outbid alice with higher bond", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        const r1 = await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1);

        let curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, alice);

        const remainingBond = (await clowns.getCurrentCLOBond(superToken.address)).remainingBond;
        console.log(`remaining bond: ${remainingBond}`);
        assert.equal(remainingBond.toString(), BOND_AMOUNT_1E12.toString());

        // fail with same amount (needs to be strictly greater)
        await expectRevert(
            sendCLOBid(bob, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1),
            "CLOWNS: bid too low"
        );

        // should succeed with 1 wad more
        const r2 = await sendCLOBid(bob, superToken, BOND_AMOUNT_1E12+1, EXIT_RATE_1);

        curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, bob);
        assert.equal(
            (await clowns.getCurrentCLOBond(superToken.address)).remainingBond,
            BOND_AMOUNT_1E12+1
        );

        expectEvent.inTransaction(r2.tx, clowns.contract, "NewCLO", {
            token: superToken.address,
            clo: bob,
            bond: (BOND_AMOUNT_1E12+1).toString(),
            exitRate: EXIT_RATE_1.toString(),
        });
    });

        // check erc1820 registration
    it("#3 CLOWNS registered with ERC1820", async () => {
        expect(
            await erc1820.getInterfaceImplementer(
                clowns.address,
                web3.utils.soliditySha3("CLOWNSv1")
            )
        ).to.equal(clowns.address);
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
            sendCLOBid(alice, superToken, bondAmount, maxRate+1),
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
            toBN(shouldMaxExitRate(BOND_AMOUNT_2E12))
                .gte(await clowns.getDefaultExitRateFor(superToken.address, BOND_AMOUNT_2E12))
        );
    });

    // interpret missing exitRate as default (min) exitRate
    it("#7 use default (max) exitRate as fallback if no exitRate specified", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12);
        await assertNetFlow(superToken, alice, await clowns.getDefaultExitRateFor(superToken.address, BOND_AMOUNT_1E12));
    });

    // TODO: remove
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

        await t.upgradeBalance("admin", t.configs.INIT_BALANCE);
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await t.sf.cfa.createFlow({
            superToken: superToken.address,
            sender: admin,
            receiver: clowns.address,
            flowRate: 1e6.toString(),
        });

        await timeTravelOnce(1e6);
        // 1e6 token/s x 1e6 seconds = ~1e12 tokens collected in the contract

        const clownsBal1 = await superToken.balanceOf(clowns.address);
        console.log(`clowns bal pre close: ${clownsBal1.toString()}`);

        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: admin,
            receiver: clowns.address,
        });

        const clownsPrelimBal = await superToken.balanceOf(clowns.address);
        await sendCLOBid(alice, superToken, BOND_AMOUNT_2E12);
        const aliceBond = (await clowns.getCurrentCLOBond(superToken.address)).remainingBond;

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
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12+1);

        assert.equal(await clowns.getCurrentCLO(superToken.address), alice);
        await assertNetFlow(superToken, alice, shouldDefaultExitRate(BOND_AMOUNT_1E12+1));
        assert.equal((await superToken.balanceOf(alice)).toString(), aliceIntermediateBal.sub(toBN(1)));
    });

    // changeFlowrate - respect limits
    it("#11 alice can change her exit rate - limits enforced", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12);

        expectRevert(
            clowns.changeExitRate(superToken.address, EXIT_RATE_1, { from: bob }),
            "CLOWNS: only CLO allowed"
        );

        // lower to 1 wad
        await clowns.changeExitRate(superToken.address, EXIT_RATE_1, { from: alice });
        await assertNetFlow(superToken, alice, EXIT_RATE_1);

        // increase to 1000 wad
        await clowns.changeExitRate(superToken.address, EXIT_RATE_1000, { from: alice });
        await assertNetFlow(superToken, alice, EXIT_RATE_1000);

        const remainingBond = (await clowns.getCurrentCLOBond(superToken.address)).remainingBond;
        console.log(`remaining bond ${remainingBond}`);
        // increase to currently allowed max
        const max1 = shouldMaxExitRate(remainingBond);
        await clowns.changeExitRate(superToken.address, max1, { from: alice });
        await assertNetFlow(superToken, alice, max1);

        // due to the exit flow, the remaining bond changes with every new block
        const max2 = shouldMaxExitRate(remainingBond);
        assert.equal(max1.toString(), max2.toString());
        expectRevert(
            clowns.changeExitRate(superToken.address, max2+1, { from: alice }),
            "CLOWNS: exitRate too high"
        );
    });

    // CLO closes stream: nothing breaks - can reopen with changeFlowrate
    it("#12 can recover from closed exit flow", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1000);
        await assertNetFlow(superToken, alice, EXIT_RATE_1000);

        await timeTravelOnce(1000);
        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: clowns.address,
            receiver: alice,
            by: alice
        });
        await assertNetFlow(superToken, alice, 0);

        await clowns.changeExitRate(superToken.address, EXIT_RATE_1, { from: alice });
        await assertNetFlow(superToken, alice, EXIT_RATE_1);

        // stop again and let bob make a bid
        await timeTravelOnce(1000);
        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: clowns.address,
            receiver: alice,
            by: alice
        });
        await assertNetFlow(superToken, alice, 0);

        await sendCLOBid(bob, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1000);
        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, bob, EXIT_RATE_1000);
    });

    // collected rewards are added added to the CLO bond
    it("#13 collected rewards are added added to the CLO bond", async () => {
        await t.upgradeBalance("admin", t.configs.INIT_BALANCE);

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, 0);

        // this fast forwarded flow emulates received rewards
        // here too we need to avoid transfer() and send() in order to not trigger a bid
        await t.sf.cfa.createFlow({
            superToken: superToken.address,
            sender: admin,
            receiver: clowns.address,
            flowRate: 1e6.toString(),
        });

        await timeTravelOnce(1e6);
        // 1e6 token/s x 1e6 seconds = ~1e12 tokens collected in the contract

        assert.equal(
            (await clowns.getCurrentCLOBond(superToken.address)).remainingBond,
            (BOND_AMOUNT_1E12 + 1e12).toString()
        );
    });

    // bond is consumed
    it("#14 bond is consumed by exit flow", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1000);

        await timeTravelOnce(1e9);

        const remainingBond = (await clowns.getCurrentCLOBond(superToken.address)).remainingBond;
        console.log(`remaining bond: ${remainingBond}`);

        // TODO: why can't admin close?
        // anybody should now be able to close the stream (is critical)
        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: clowns.address,
            receiver: alice,
            by: alice
        });
    });

    // multiple tokens

    // CLO is a contract which reverts on ERC777 hook
});
