import {assert} from "chai";
import {artifacts, ethers, web3} from "hardhat";

import {ISETH, SuperToken__factory} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";
import {expectCustomError} from "../../utils/expectRevert";

const {web3tx, toBN, toWad} = require("@decentral.ee/web3-helpers");
const {expectEvent} = require("@openzeppelin/test-helpers");
const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const ISETH = artifacts.require("ISETH");
const SETHProxy = artifacts.require("SETHProxy");

describe("Super ETH (SETH) Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let alice: string, bob: string;
    let seth: ISETH;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 3,
        });

        const superTokenFactory = await ISuperTokenFactory.at(
            await t.contracts.superfluid.getSuperTokenFactory()
        );
        const sethProxy = await SETHProxy.new();
        seth = await ISETH.at(sethProxy.address);
        await web3tx(
            superTokenFactory.initializeCustomSuperToken,
            "initializeCustomSuperToken"
        )(seth.address);
        await web3tx(seth.initialize, "seth.initialize")(
            t.constants.ZERO_ADDRESS,
            18,
            "Super ETH",
            "SETH"
        );

        await t.pushEvmSnapshot();

        ({alice, bob} = t.aliases);
    });

    after(async () => {
        await t.popEvmSnapshot();
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
        t.beforeEachTestCaseBenchmark(this);
    });

    afterEach(async () => {
        t.afterEachTestCaseBenchmark();
    });

    it("#1.1 upgradeByETH", async () => {
        const tx = await web3tx(
            seth.upgradeByETH,
            "seth.upgradeByETH by alice"
        )({
            from: alice,
            value: toWad(1),
        });
        await expectEvent.inTransaction(
            tx.tx,
            t.sf.contracts.ISuperToken, // see if it's compatible with ISuperToken events
            "TokenUpgraded",
            {
                account: alice,
                amount: toWad(1).toString(),
            }
        );
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            toWad(1).toString()
        );
    });

    it("#1.2 upgradeByETHTo", async () => {
        const tx = await web3tx(
            seth.upgradeByETHTo,
            "seth.upgradeByETHTo bob by alice"
        )(alice, {
            from: bob,
            value: toWad(1),
        });
        await expectEvent.inTransaction(
            tx.tx,
            t.sf.contracts.ISuperToken, // see if it's compatible with ISuperToken events
            "TokenUpgraded",
            {
                account: alice,
                amount: toWad(1).toString(),
            }
        );
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            toWad(1).toString()
        );
    });

    it("#1.4 downgradeToETH", async () => {
        const ethersSETH = await ethers.getContractAt("ISETH", seth.address);
        const aliceSigner = await ethers.getSigner(alice);
        await web3tx(
            ethersSETH.connect(aliceSigner).upgradeByETH,
            "seth.upgradeByETH by alice"
        )({
            value: toWad(1).toString(),
        });
        const superTokenContract = new ethers.Contract(
            "SuperToken",
            SuperToken__factory.abi,
            aliceSigner
        );
        await expectCustomError(
            ethersSETH
                .connect(aliceSigner)
                .downgradeToETH(toWad(1).addn(1).toString()),
            superTokenContract,
            "SF_TOKEN_BURN_INSUFFICIENT_BALANCE"
        );

        const aliceBalance1 = await web3.eth.getBalance(alice);
        const tx = await web3tx(
            seth.downgradeToETH,
            "seth.downgradeToETH by alice"
        )(toWad(1).toString(), {from: alice});
        const aliceBalance2 = await web3.eth.getBalance(alice);
        await expectEvent.inTransaction(
            tx.tx,
            t.sf.contracts.ISuperToken, // see if it's compatible with ISuperToken events
            "TokenDowngraded",
            {
                account: alice,
                amount: toWad(1).toString(),
            }
        );
        assert.equal(
            toBN(aliceBalance2)
                .sub(toBN(aliceBalance1))
                .add(tx.txCost)
                .toString(),
            toWad(1).toString()
        );
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            toWad(0).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            toWad(0).toString()
        );
    });

    it("#1.5 - Direct send Ether", async () => {
        await web3.eth.sendTransaction({
            to: seth.address,
            from: alice,
            value: toWad(1),
        });
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            toWad(1).toString()
        );
    });
});
