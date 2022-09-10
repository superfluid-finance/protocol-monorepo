import {assert, expect, ethers, web3} from "hardhat";
import {BigNumber, BigNumberish} from "ethers";
import {
    ConstantFlowAgreementV1,
    IERC1820Registry,
    SuperfluidMock,
    SuperToken,
    TOGA,
    TokenCustodian,
} from "../../../typechain-types";

import {expectRevertedWith} from "../../utils/expectRevert";
import {toBN, toWad} from "./helpers";
import TestEnvironment from "../../TestEnvironment";
const traveler = require("ganache-time-traveler");

describe("TOGA", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();
    const {ZERO_ADDRESS} = t.constants;

    let admin: string, alice: string, bob: string, carol: string;

    let superfluid: SuperfluidMock,
        erc1820: IERC1820Registry,
        cfa: ConstantFlowAgreementV1;
    let toga: TOGA, custodian: TokenCustodian;
    let superToken: SuperToken;
    const MIN_BOND_DURATION = 3600 * 24 * 7; // 604800 s | 7 days
    const EXIT_RATE_1 = 1; // 1 wad per second
    const EXIT_RATE_1E3 = 1e3; // 1000 wad per second
    const EXIT_RATE_1E6 = 1e6; // 1000000 wad per second
    const BOND_AMOUNT_1E18 = toWad(1);
    const BOND_AMOUNT_2E18 = toWad(2);
    const BOND_AMOUNT_3E18 = toWad(3);
    const BOND_AMOUNT_10E18 = toWad(10);

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({admin, alice, bob, carol} = t.aliases);
        ({superfluid, erc1820, cfa} = t.contracts);
        superToken = await ethers.getContractAt(
            "SuperToken",
            t.sf.tokens.TESTx.address
        );
    });

    after(async function () {
        await t.report({title: "TOGA.test"});
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
        const CustodianFactory = await ethers.getContractFactory(
            "TokenCustodian"
        );
        custodian = await CustodianFactory.deploy();
        console.log(`custodian deployed at: ${custodian.address}`);
        const TOGAFactory = await ethers.getContractFactory("TOGA");
        toga = await TOGAFactory.deploy(
            superfluid.address,
            MIN_BOND_DURATION,
            custodian.address
        );
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
    async function sendPICBid(
        sender: string,
        token: SuperToken,
        bondAmount: BigNumberish,
        exitRate?: BigNumberish,
        gasLimit = 3000000
    ) {
        const exitRateEncoded =
            exitRate !== undefined
                ? web3.eth.abi.encodeParameter("int96", exitRate)
                : "0x";
        const signer = await ethers.getSigner(sender);
        return await token
            .connect(signer)
            .send(toga.address, bondAmount, exitRateEncoded, {
                gasLimit,
            });
    }

    function shouldMaxExitRate(bondAmount: BigNumber) {
        return Math.floor(
            Number(bondAmount.div(toBN(MIN_BOND_DURATION)).toString())
        );
    }

    function shouldDefaultExitRate(bondAmount: number) {
        return Math.floor(bondAmount / (MIN_BOND_DURATION * 4));
    }

    async function assertNetFlow(
        token: SuperToken,
        account: string,
        expectedFlowRate: BigNumberish
    ) {
        const actualFlowrate = await cfa.getNetFlow(token.address, account);
        assert.equal(
            expectedFlowRate.toString(),
            actualFlowrate.toString(),
            "flowrate not as expected"
        );
    }

    // emulates reward collection through a stream. Will fast forward the chain by the given time.
    // Done with a stream because a transfer() or send() would trigger a bid
    async function collectRewards(
        token: SuperToken,
        flowrate: BigNumberish,
        time: number
    ) {
        await t.upgradeBalance("admin", t.configs.INIT_BALANCE);

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
    async function liquidateExitStream(token: SuperToken) {
        await t.sf.cfa.deleteFlow({
            superToken: token.address,
            sender: toga.address,
            receiver: await toga.getCurrentPIC(token.address),
            by: admin,
        });
    }

    // ================================================================================

    it("#0 contract setup", async () => {
        const custodianAddr = await toga.custodian();
        assert.equal(custodianAddr, custodian.address);

        const minBondDuration = await toga.minBondDuration();
        assert.equal(minBondDuration.toNumber(), MIN_BOND_DURATION);
    });

    it("#1 alice becomes PIC", async () => {
        assert.equal(
            await toga.getCurrentPIC(superToken.address),
            ZERO_ADDRESS
        );

        await expect(
            sendPICBid(alice, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1)
        )
            .to.emit(toga, "NewPIC")
            .withArgs(
                superToken.address,
                alice,
                BOND_AMOUNT_1E18.toString(),
                EXIT_RATE_1.toString()
            );

        assert.equal(await toga.getCurrentPIC(superToken.address), alice);

        const {pic, bond: bond} = await toga.getCurrentPICInfo(
            superToken.address
        );
        assert.equal(pic, alice);
        assert.equal(bond.toString(), BOND_AMOUNT_1E18.toString());

        await assertNetFlow(superToken, alice, EXIT_RATE_1);
    });

    it("#2 bob can outbid alice with higher bond", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 0);

        assert.equal(await toga.getCurrentPIC(superToken.address), alice);

        const bond = (await toga.getCurrentPICInfo(superToken.address)).bond;
        //console.log(`remaining bond: ${bond}`);
        assert.equal(bond.toString(), BOND_AMOUNT_1E18.toString());

        // fail with same amount (needs to be strictly greater)
        // fails to trigger if the timestamp is 1s off (happens sometimes randomly)
        // this detracting 1s exit flowrate
        await expectRevertedWith(
            sendPICBid(
                bob,
                superToken,
                BOND_AMOUNT_1E18.sub(toBN(EXIT_RATE_1)),
                EXIT_RATE_1
            ),
            "TOGA: bid too low"
        );

        // should succeed with 1 wad more
        await expect(
            sendPICBid(
                bob,
                superToken,
                BOND_AMOUNT_1E18.add(toBN(1)),
                EXIT_RATE_1
            )
        )
            .to.emit(toga, "NewPIC")
            .withArgs(
                superToken.address,
                bob,
                BOND_AMOUNT_1E18.add(toBN(1)).toString(),
                EXIT_RATE_1.toString()
            );

        assert.equal(await toga.getCurrentPIC(superToken.address), bob);
        assert.equal(
            (await toga.getCurrentPICInfo(superToken.address)).bond.toString(),
            BOND_AMOUNT_1E18.add(toBN(1)).toString()
        );
    });

    it("#3 TOGA registered with ERC1820", async () => {
        assert.equal(
            await erc1820.getInterfaceImplementer(
                toga.address,
                web3.utils.soliditySha3("TOGAv1")!
            ),
            toga.address
        );

        assert.equal(
            await erc1820.getInterfaceImplementer(
                toga.address,
                web3.utils.soliditySha3("TOGAv2")!
            ),
            toga.address
        );
    });

    it("#4 enforce min exit rate limit", async () => {
        // lower limit: 0 wei/second (no negative value allowed)
        await expectRevertedWith(
            sendPICBid(alice, superToken, BOND_AMOUNT_1E18, -1),
            "TOGA: negative exitRate not allowed"
        );

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 0);
        await assertNetFlow(superToken, alice, 0);
    });

    it("#5 enforce max exitRate limit", async () => {
        const bondAmount = BOND_AMOUNT_1E18;
        const maxRate = shouldMaxExitRate(bondAmount);

        await expectRevertedWith(
            sendPICBid(alice, superToken, bondAmount, maxRate + 1),
            "TOGA: exitRate too high"
        );

        // exact upper limit
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, maxRate);
        await assertNetFlow(superToken, alice, maxRate);
    });

    it("#6 max and default exitRate are calculated correctly", async () => {
        expect(shouldMaxExitRate(BOND_AMOUNT_1E18).toString()).to.equal(
            await toga.getMaxExitRateFor(superToken.address, BOND_AMOUNT_1E18)
        );

        expect(shouldMaxExitRate(BOND_AMOUNT_2E18).toString()).to.equal(
            await toga.getMaxExitRateFor(superToken.address, BOND_AMOUNT_2E18)
        );

        expect(
            shouldDefaultExitRate(
                Number(BOND_AMOUNT_1E18.toString())
            ).toString()
        ).to.equal(
            await toga.getDefaultExitRateFor(
                superToken.address,
                BOND_AMOUNT_1E18
            )
        );

        expect(
            shouldDefaultExitRate(
                Number(BOND_AMOUNT_2E18.toString())
            ).toString()
        ).to.equal(
            await toga.getDefaultExitRateFor(
                superToken.address,
                BOND_AMOUNT_2E18
            )
        );

        // the default exit rate needs to be equal or greater than the max exit rate
        assert(
            toBN(shouldMaxExitRate(BOND_AMOUNT_2E18)).gte(
                await toga.getDefaultExitRateFor(
                    superToken.address,
                    BOND_AMOUNT_2E18
                )
            )
        );
    });

    it("#7 use default (max) exitRate as fallback if no exitRate specified", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18);
        await assertNetFlow(
            superToken,
            alice,
            await toga.getDefaultExitRateFor(
                superToken.address,
                BOND_AMOUNT_1E18
            )
        );
    });

    it("#8 cannot become PIC with bid smaller than current PIC bond", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_2E18);

        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        await expectRevertedWith(
            sendPICBid(bob, superToken, BOND_AMOUNT_1E18),
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
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await collectRewards(superToken, 1e6, 1e6);
        // 1e6 token/s x 1e6 seconds = ~1e12 tokens collected in the contract

        //const togaBal1 = await superToken.balanceOf(toga.address);
        //console.log(`toga bal pre close: ${togaBal1.toString()}`);

        const togaPrelimBal = await superToken.balanceOf(toga.address);
        await sendPICBid(alice, superToken, BOND_AMOUNT_2E18);
        const aliceBond = (await toga.getCurrentPICInfo(superToken.address))
            .bond;

        // the tokens previously collected in the contract are attributed to Alice's bond
        assert.equal(
            aliceBond.toString(),
            togaPrelimBal.add(BOND_AMOUNT_2E18).toString()
        );

        const alicePreOutbidBal = await superToken.balanceOf(alice);
        await sendPICBid(bob, superToken, BOND_AMOUNT_10E18);

        // the tokens previously collected are paid out to Alice if outbid
        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            alicePreOutbidBal.add(aliceBond).toString()
        );
    });

    it("#10 Current PIC can increase the bond", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_2E18, 0);
        const aliceIntermediateBal = await superToken.balanceOf(alice);
        await expect(sendPICBid(alice, superToken, BOND_AMOUNT_1E18))
            .to.emit(toga, "BondIncreased")
            .withArgs(superToken.address, BOND_AMOUNT_1E18.toString());

        assert.equal(await toga.getCurrentPIC(superToken.address), alice);
        await assertNetFlow(superToken, alice, 0);
        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            aliceIntermediateBal.sub(toBN(BOND_AMOUNT_1E18)).toString()
        );
        assert.equal(
            (await toga.getCurrentPICInfo(superToken.address)).bond.toString(),
            BOND_AMOUNT_2E18.add(BOND_AMOUNT_1E18).toString()
        );
    });

    it("#11 PIC can change exit rate - limits enforced", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18);
        const aliceSigner = await ethers.getSigner(alice);

        await expectRevertedWith(
            toga
                .connect(await ethers.getSigner(bob))
                .changeExitRate(superToken.address, EXIT_RATE_1),
            "TOGA: only PIC allowed"
        );

        // don't allow negative exitRate
        await expectRevertedWith(
            toga.connect(aliceSigner).changeExitRate(superToken.address, -1),
            "TOGA: negative exitRate not allowed"
        );

        // lower to 1 wad
        await expect(
            toga
                .connect(aliceSigner)
                .changeExitRate(superToken.address, EXIT_RATE_1)
        )
            .to.emit(toga, "ExitRateChanged")
            .withArgs(superToken.address, EXIT_RATE_1.toString());
        await assertNetFlow(superToken, alice, EXIT_RATE_1);

        // increase to 1000 wad
        await toga
            .connect(aliceSigner)
            .changeExitRate(superToken.address, EXIT_RATE_1E3);
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        // to 0
        await toga.connect(aliceSigner).changeExitRate(superToken.address, 0);
        await assertNetFlow(superToken, alice, 0);

        const bond = (await toga.getCurrentPICInfo(superToken.address)).bond;

        // increase to currently allowed max
        const max1 = shouldMaxExitRate(bond);
        await toga
            .connect(aliceSigner)
            .changeExitRate(superToken.address, max1);
        await assertNetFlow(superToken, alice, max1);

        // due to the exit flow, the remaining bond changes with every new block
        const max2 = shouldMaxExitRate(bond);
        assert.equal(max1.toString(), max2.toString());
        await expectRevertedWith(
            toga
                .connect(aliceSigner)
                .changeExitRate(superToken.address, max2 + 1),
            "TOGA: exitRate too high"
        );
    });

    // PIC closes stream: nothing breaks - can reopen with changeFlowrate
    it("#12 can re-open closed exit flow", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        await timeTravelOnce(1000);
        await t.sf.cfa.deleteFlow({
            superToken: superToken.address,
            sender: toga.address,
            receiver: alice,
            by: alice,
        });
        await assertNetFlow(superToken, alice, 0);

        const aliceSigner = await ethers.getSigner(alice);
        // 0 -> 0 (leave unchanged)
        await toga.connect(aliceSigner).changeExitRate(superToken.address, 0);
        await assertNetFlow(superToken, alice, 0);

        // trigger re-opening
        await toga
            .connect(aliceSigner)
            .changeExitRate(superToken.address, EXIT_RATE_1);
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

        await sendPICBid(bob, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);
        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, bob, EXIT_RATE_1E3);
    });

    it("#13 collected rewards are added to the PIC bond", async () => {
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 0);

        await collectRewards(superToken, 1e6, 1e6);
        // 1e6 token/s x 1e6 seconds = ~1e12 tokens collected in the contract

        assert.isTrue(
            (await toga.getCurrentPICInfo(superToken.address)).bond.gte(
                BOND_AMOUNT_1E18.add(toBN(1e12))
            )
        );
        // can't rely on that bcs the block timestamp is sometimes 1s higher
        /*
        assert.equal(
            (await toga.getCurrentPICInfo(superToken.address)).bond.toString(),
            (BOND_AMOUNT_1E12 + 1e12).toString()
        );
         */
    });

    // bond is consumed
    it("#14 bond is consumed by exit flow", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        const maxRate = shouldMaxExitRate(BOND_AMOUNT_1E18);
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, maxRate);
        await assertNetFlow(superToken, alice, maxRate);

        // critical stream is liquidated - remaining bond goes to zero
        await timeTravelOnce(1e6);

        await liquidateExitStream(superToken); // a sentinel would do this

        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, toga.address, 0);

        // this assumes the flow deletion was not triggered by the PIC - otherwise rewards would be accrued
        assert.equal(
            (await toga.getCurrentPICInfo(superToken.address)).bond.toString(),
            "0"
        );

        const aliceSigner = await ethers.getSigner(alice);

        // alice tries to re-establish stream - fail because no bond left
        await expectRevertedWith(
            toga
                .connect(aliceSigner)
                .changeExitRate(superToken.address, EXIT_RATE_1),
            "TOGA: exitRate too high"
        );

        const flowRate = t.configs.MINIMUM_DEPOSIT.mul(toBN(2)).div(toBN(1e3));
        // after some more rewards being collected, alice can re-establish the exit stream
        await collectRewards(superToken, Number(flowRate.toString()), 1e3);
        assert.isTrue(
            (await toga.getCurrentPICInfo(superToken.address)).bond.gte(
                toBN(1e12)
            )
        );

        await toga
            .connect(aliceSigner)
            .changeExitRate(superToken.address, EXIT_RATE_1E3);
        await assertNetFlow(superToken, alice, EXIT_RATE_1E3);

        const alicePreBal = await superToken.balanceOf(alice);
        const aliceBondLeft = (await toga.getCurrentPICInfo(superToken.address))
            .bond;

        // bob outbids
        await expectRevertedWith(
            sendPICBid(bob, superToken, 1, 0),
            "TOGA: bid too low"
        );
        await sendPICBid(bob, superToken, BOND_AMOUNT_2E18, EXIT_RATE_1E6);
        await assertNetFlow(superToken, alice, 0);
        await assertNetFlow(superToken, bob, EXIT_RATE_1E6);

        assert.equal(
            alicePreBal.add(aliceBondLeft).toString(),
            (await superToken.balanceOf(alice)).toString()
        );
    });

    it("#15 multiple PICs (one per token) in parallel", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        let superToken2 = (
            await t.deployNewToken("TEST2", {
                doUpgrade: true,
                isTruffle: true,
            })
        ).superToken;
        superToken2 = await ethers.getContractAt(
            "SuperToken",
            superToken2.address
        );

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);
        await sendPICBid(bob, superToken2, BOND_AMOUNT_2E18, EXIT_RATE_1);

        assert.equal(await toga.getCurrentPIC(superToken.address), alice);
        assert.equal(await toga.getCurrentPIC(superToken2.address), bob);

        await expectRevertedWith(
            toga
                .connect(await ethers.getSigner(alice))
                .changeExitRate(superToken2.address, 0),
            "TOGA: only PIC allowed"
        );
        await expectRevertedWith(
            toga
                .connect(await ethers.getSigner(bob))
                .changeExitRate(superToken.address, 0),
            "TOGA: only PIC allowed"
        );

        // let this run for a while...
        await timeTravelOnce(1e6);

        // alice takes over superToken2
        const bobPreBal = await superToken2.balanceOf(bob);
        const bobBondLeft = (await toga.getCurrentPICInfo(superToken2.address))
            .bond;

        await expectRevertedWith(
            sendPICBid(alice, superToken2, BOND_AMOUNT_1E18, EXIT_RATE_1),
            "TOGA: bid too low"
        );

        await sendPICBid(alice, superToken2, BOND_AMOUNT_10E18, EXIT_RATE_1E3);

        assert.equal(
            bobPreBal.add(bobBondLeft).toString(),
            (await superToken2.balanceOf(bob)).toString()
        );
    });

    it("#16 reverting send() hook can't prevent a successful bid", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        const aliceRecipientHookFactory = await ethers.getContractFactory(
            "ERC777RecipientReverting"
        );
        const aliceRecipientHook = await aliceRecipientHookFactory.deploy();
        await erc1820
            .connect(await ethers.getSigner(alice))
            .setInterfaceImplementer(
                alice,
                web3.utils.soliditySha3("ERC777TokensRecipient")!,
                aliceRecipientHook.address
            );
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);

        await sendPICBid(bob, superToken, BOND_AMOUNT_2E18, EXIT_RATE_1E3);
        assert.equal(await toga.getCurrentPIC(superToken.address), bob);
    });

    // this ensures an OOG in fallback transfer can't succeed
    it("#17 reverting transfer to custodian reverts whole tx", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        await t.upgradeBalance("carol", t.configs.INIT_BALANCE);

        // override to a toga which uses a reverting custodian contract
        const revertingRecipientFactory = await ethers.getContractFactory(
            "ERC777RecipientReverting"
        );
        const revertingRecipient = await revertingRecipientFactory.deploy();
        const TOGAFactory = await ethers.getContractFactory("TOGA");
        toga = await TOGAFactory.deploy(
            superfluid.address,
            MIN_BOND_DURATION,
            revertingRecipient.address
        );

        // alice becomes first PIC
        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);
        assert.equal(await toga.getCurrentPIC(superToken.address), alice);

        // bob can outbid
        await sendPICBid(bob, superToken, BOND_AMOUNT_2E18, EXIT_RATE_1E3);
        assert.equal(await toga.getCurrentPIC(superToken.address), bob);

        await erc1820
            .connect(await ethers.getSigner(bob))
            .setInterfaceImplementer(
                bob,
                web3.utils.soliditySha3("ERC777TokensRecipient")!,
                revertingRecipient.address
            );

        // now both the send() in the try and the send() in the catch fail
        await expectRevertedWith(
            sendPICBid(carol, superToken, BOND_AMOUNT_3E18, EXIT_RATE_1E3),
            "they shall not pass"
        );
    });

    it("#18 previous PIC can't grief new bidder with gas draining send() hook [ @skip-on-coverage ]", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);

        // alice becomes malicious and tries to prevent others from outbidding her
        const ERC777RecipientDrainingGasFactory =
            await ethers.getContractFactory("ERC777RecipientDrainingGas");
        const aliceRecipientHook =
            await ERC777RecipientDrainingGasFactory.deploy();
        await erc1820
            .connect(await ethers.getSigner(alice))
            .setInterfaceImplementer(
                alice,
                web3.utils.soliditySha3("ERC777TokensRecipient")!,
                aliceRecipientHook.address
            );

        // send hook has higher allowance than gas limit, causes the tx to fail
        await expectRevertedWith(
            sendPICBid(bob, superToken, BOND_AMOUNT_2E18, EXIT_RATE_1E3),
            "CallUtils: target revert()"
        );

        // tx gets high enough gas limit for the send allowance not to make it fail
        await expect(
            sendPICBid(
                bob,
                superToken,
                BOND_AMOUNT_2E18,
                EXIT_RATE_1E3,
                4000000
            )
        ).to.emit(aliceRecipientHook, "DrainedGas");
        // @note we need to figure out a way of getting the receipt
        // console.log(`gas used by tx: ${r1.receipt.gasUsed}`);
    });

    it("#19 funds in custody can be withdrawn by legitimate owner", async () => {
        await t.upgradeBalance("bob", t.configs.INIT_BALANCE);
        await t.upgradeBalance("carol", t.configs.INIT_BALANCE);

        const RevertingRecipientHookFactory = await ethers.getContractFactory(
            "ERC777RecipientReverting"
        );
        const revertingRecipientHook =
            await RevertingRecipientHookFactory.deploy();
        await erc1820
            .connect(await ethers.getSigner(alice))
            .setInterfaceImplementer(
                alice,
                web3.utils.soliditySha3("ERC777TokensRecipient")!,
                revertingRecipientHook.address
            );
        await erc1820
            .connect(await ethers.getSigner(bob))
            .setInterfaceImplementer(
                bob,
                web3.utils.soliditySha3("ERC777TokensRecipient")!,
                revertingRecipientHook.address
            );

        await sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 0);
        const alicePreOutbid1Bal = await superToken.balanceOf(alice);
        const alicePreOutbid1Bond = (
            await toga.getCurrentPICInfo(superToken.address)
        ).bond;

        await expect(sendPICBid(bob, superToken, BOND_AMOUNT_2E18, 0))
            .to.emit(custodian, "CustodianDeposit")
            .withArgs(superToken.address, alice, BOND_AMOUNT_1E18.toString());
        assert.equal(
            (await superToken.balanceOf(custodian.address)).toString(),
            alicePreOutbid1Bond.toString()
        );

        const bobPreOutbid1Bal = await superToken.balanceOf(bob);
        const bobPreOutbid1Bond = (
            await toga.getCurrentPICInfo(superToken.address)
        ).bond;

        await expect(sendPICBid(carol, superToken, BOND_AMOUNT_3E18, 0))
            .to.emit(custodian, "CustodianDeposit")
            .withArgs(superToken.address, bob, BOND_AMOUNT_2E18.toString());
        assert.equal(
            (await superToken.balanceOf(custodian.address)).toString(),
            alicePreOutbid1Bond.add(bobPreOutbid1Bond).toString()
        );

        // remove the reverting hook from both alice and bob,
        // otherwise withdrawal from the custodian fails too
        await erc1820
            .connect(await ethers.getSigner(alice))
            .setInterfaceImplementer(
                alice,
                web3.utils.soliditySha3("ERC777TokensRecipient")!,
                ZERO_ADDRESS
            );
        await erc1820
            .connect(await ethers.getSigner(bob))
            .setInterfaceImplementer(
                bob,
                web3.utils.soliditySha3("ERC777TokensRecipient")!,
                ZERO_ADDRESS
            );

        await expect(
            toga
                .connect(await ethers.getSigner(alice))
                .withdrawFundsInCustody(superToken.address)
        )
            .to.emit(custodian, "CustodianWithdrawal")
            .withArgs(superToken.address, alice, BOND_AMOUNT_1E18.toString());
        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            alicePreOutbid1Bal.add(alicePreOutbid1Bond).toString()
        );
        // withdrawing again shall have no effect
        await toga
            .connect(await ethers.getSigner(alice))
            .withdrawFundsInCustody(superToken.address);
        assert.equal(
            (await superToken.balanceOf(alice)).toString(),
            alicePreOutbid1Bal.add(alicePreOutbid1Bond).toString()
        );

        await expect(
            toga
                .connect(await ethers.getSigner(bob))
                .withdrawFundsInCustody(superToken.address)
        )
            .to.emit(custodian, "CustodianWithdrawal")
            .withArgs(superToken.address, bob, BOND_AMOUNT_2E18.toString());
        assert.equal(
            (await superToken.balanceOf(bob)).toString(),
            bobPreOutbid1Bal.add(bobPreOutbid1Bond).toString()
        );

        // no funds left as all was withdrawn
        assert.equal(
            (await superToken.balanceOf(custodian.address)).toString(),
            "0"
        );
    });
});
