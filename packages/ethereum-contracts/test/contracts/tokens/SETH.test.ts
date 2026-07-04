import {assert} from "chai";
import {ethers} from "hardhat";

import {ISETH, SuperToken__factory} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";
import {web3} from "../../lib/web3-shim";
import {expectCustomError} from "../../utils/expectRevert";

const {toBN, toWad} = require("@decentral.ee/web3-helpers");

const artifacts = require("../../lib/artifacts");
const {callAsAccount} = require("../../lib/as-account");
const expectEvent = require("../../lib/expect-emit");
const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const ISETH = artifacts.require("ISETH");
const SETHProxy = artifacts.require("SETHProxy");

function ethValue(amount: string | number) {
    const raw = toWad(amount).toString();
    return ethers.BigNumber.from(raw.startsWith("0x") ? raw : `0x${raw}`);
}

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
        await callAsAccount(
            superTokenFactory,
            t.aliases.admin,
            "initializeCustomSuperToken",
            seth.address
        );
        await callAsAccount(
            seth,
            t.aliases.admin,
            "initialize",
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
        const aliceSigner = await ethers.getSigner(alice);
        const tx = await (
            await seth.connect(aliceSigner)
        ).upgradeByETH({value: ethValue(1)});
        const receipt = await tx.wait();
        await expectEvent.inTransaction(
            receipt.transactionHash,
            t.sf.contracts.ISuperToken,
            "TokenUpgraded",
            {
                account: alice,
                amount: ethValue(1).toString(),
            }
        );
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            ethValue(1).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            ethValue(1).toString()
        );
    });

    it("#1.2 upgradeByETHTo", async () => {
        const bobSigner = await ethers.getSigner(bob);
        const tx = await (
            await seth.connect(bobSigner)
        ).upgradeByETHTo(alice, {value: ethValue(1)});
        const receipt = await tx.wait();
        await expectEvent.inTransaction(
            receipt.transactionHash,
            t.sf.contracts.ISuperToken,
            "TokenUpgraded",
            {
                account: alice,
                amount: ethValue(1).toString(),
            }
        );
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            ethValue(1).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            ethValue(1).toString()
        );
    });

    it("#1.4 downgradeToETH", async () => {
        const aliceSigner = await ethers.getSigner(alice);
        const ethersSETH = await seth.connect(aliceSigner);
        await ethersSETH.upgradeByETH({value: ethValue(1)});
        const superTokenContract = new ethers.Contract(
            "SuperToken",
            SuperToken__factory.abi,
            aliceSigner
        );
        await expectCustomError(
            ethersSETH.downgradeToETH(ethValue(1).add(1)),
            superTokenContract,
            "SF_TOKEN_BURN_INSUFFICIENT_BALANCE"
        );

        const aliceBalance1 = await web3.eth.getBalance(alice);
        const tx = await callAsAccount(
            seth,
            alice,
            "downgradeToETH",
            ethValue(1)
        );
        const aliceBalance2 = await web3.eth.getBalance(alice);
        await expectEvent.inTransaction(
            tx.tx,
            t.sf.contracts.ISuperToken,
            "TokenDowngraded",
            {
                account: alice,
                amount: ethValue(1).toString(),
            }
        );
        assert.equal(
            toBN(aliceBalance2)
                .sub(toBN(aliceBalance1))
                .add(tx.txCost)
                .toString(),
            ethValue(1).toString()
        );
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            ethValue(0).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            ethValue(0).toString()
        );
    });

    it("#1.5 - Direct send Ether", async () => {
        await web3.eth.sendTransaction({
            to: seth.address,
            from: alice,
            value: ethValue(1),
        });
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            ethValue(1).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            ethValue(1).toString()
        );
    });
});
