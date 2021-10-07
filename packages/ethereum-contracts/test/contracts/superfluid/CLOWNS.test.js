const { expectEvent, expectRevert } = require("@openzeppelin/test-helpers");
const { toWad, wad4human } = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("../../TestEnvironment");
const traveler = require("ganache-time-traveler");

const CLOWNS = artifacts.require("CLOWNS");

describe("CLOWNS", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let admin, alice, bob, carol;

    let superfluid, erc1820, cfa;
    let clowns;
    let superToken;
    const MIN_BOND_DURATION = 3600 * 24 * 7;
    const EXIT_RATE_1 = 1; // 1 wad per second
    const EXIT_RATE_1000 = 1000; // 1000 wad per second
    const BOND_AMOUNT_1E12 = 1E12;
    const BOND_AMOUNT_2E12 = 2E12;

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
        //console.log(`CLOWNS deployed at: ${clowns.address}`);
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

    function maxExitRate(bondAmount) {
        return Math.floor(bondAmount / MIN_BOND_DURATION);
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
        await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

        const r1 = await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1);

        expectEvent.inTransaction(r1.tx, clowns.contract, "NewCLO", {
            token: superToken.address,
            clo: alice,
            bond: BOND_AMOUNT_1E12.toString(),
            exitRate: EXIT_RATE_1.toString(),
        });

        const curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, alice);

        const { remainingBond } = await clowns.getCurrentCLOBond(
            superToken.address
        );
        console.log(
            `remaining bond: ${wad4human(
                remainingBond
            )} (${remainingBond.toString()})`
        );

        // check exit stream
        const fr = await cfa.getNetFlow(superToken.address, alice);
        //console.log(`U1 flowrate: ${netFlowEarly.toString()}`);

        assert.equal(
            fr.toString(),
            EXIT_RATE_1.toString(),
            "wrong exit flowrate"
        );
    });

    it("#1 bob takes over CLO role from alice", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        const r1 = await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1);

        let curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, alice);

        // bob can take over with the same bond amount as alice
        // because the exitStream deposit has been detracted from the contract balance
        const r2 = await sendCLOBid(bob, superToken, BOND_AMOUNT_1E12, EXIT_RATE_1);
        curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, bob);

        expectEvent.inTransaction(r2.tx, clowns.contract, "NewCLO", {
            token: superToken.address,
            clo: bob,
            bond: BOND_AMOUNT_1E12.toString(),
            exitRate: EXIT_RATE_1.toString(),
        });

    });

        // check erc1820 registration
    it("#2 CLOWNS registered with ERC1820", async () => {
        expect(
            await erc1820.getInterfaceImplementer(
                clowns.address,
                web3.utils.soliditySha3("CLOWNSv1")
            )
        ).to.equal(clowns.address);
    });

    it("#3 enforce min exitRate limit", async () => {
        // lower limit: 0 wei/second (no negative value allowed)
        await expectRevert(
            sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, -1),
            "CLOWNS: negative exitRate not allowed"
        );

        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, 0);
        await assertNetFlow(superToken, alice, 0);
    });

    it("#4 enforce max exitRate limit", async () => {
        const bondAmount = BOND_AMOUNT_1E12;
        const maxRate = maxExitRate(bondAmount);

        await expectRevert(
            sendCLOBid(alice, superToken, bondAmount, maxRate+1),
            "CLOWNS: below minimum bond duration"
        );

        // exact upper limit
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12, maxRate);
        await assertNetFlow(superToken, alice, maxRate);
    });

    it("#5 default exitRate is calculated correctly", async () => {
        assert.equal(
            maxExitRate(BOND_AMOUNT_1E12).toString(),
            await clowns.getDefaultExitRate(BOND_AMOUNT_1E12)
        );

        assert.equal(
            maxExitRate(BOND_AMOUNT_2E12).toString(),
            await clowns.getDefaultExitRate(BOND_AMOUNT_2E12)
        );
    });

    // interpret missing exitRate as default (min) exitRate
    it("#6 use default (max) exitRate as fallback if no exitRate specified", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_1E12);
        await assertNetFlow(superToken, alice, await clowns.getDefaultExitRate(BOND_AMOUNT_1E12));
    });

    // enforce min bid limit
    it("#7 cannot become CLO with smaller bond bid", async () => {
        await sendCLOBid(alice, superToken, BOND_AMOUNT_2E12);

        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        await expectRevert(
            sendCLOBid(bob, superToken, BOND_AMOUNT_1E12),
            "CLOWNS: bid too low"
        );
    });

    // handle initial bid with non-zero contract balance
    it("#8 bid for token which has rewards already accrued", async () => {
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

        t.sf.cfa.createFlow({
            superToken: superToken.address,
            sender: admin,
            receiver: clowns.address,
            flowRate: 1e6.toString(),
        });

        await timeTravelOnce(1e6);

        const clownsBal1 = await superToken.balanceOf(clowns.address);
        console.log(`clowns bal pre close: ${clownsBal1.toString()}`);

        t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: admin,
            receiver: clowns.address,
        });

        const clownsBal = await superToken.balanceOf(clowns.address);
        console.log(`clowns bal: ${clownsBal.toString()}`);
        //assert.equal(clownsBal.toString(), 1e12.toString());

        // TODO: why is the balance still 0?
    });

    // changeFlowrate

    // CLO closes stream: nothing breaks - can reopen with changeFlowrate

    // rewards are added

    // bond goes to zero
});
