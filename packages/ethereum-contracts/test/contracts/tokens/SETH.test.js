const {expectEvent} = require("@openzeppelin/test-helpers");
const {expectRevertedWith} = require("../../utils/expectRevert");

const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const TestEnvironment = require("../../TestEnvironment");
const ISETH = artifacts.require("ISETH");
const SETHProxy = artifacts.require("SETHProxy");

const {web3tx, toBN, toWad} = require("@decentral.ee/web3-helpers");

describe("Super ETH (SETH) Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let alice, bob;
    let seth;

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
        await web3tx(
            seth.upgradeByETH,
            "seth.upgradeByETH by alice"
        )({
            value: toWad(1),
            from: alice,
        });

        await expectRevertedWith(
            seth.downgradeToETH(toWad(1).addn(1), {from: alice}),
            "SuperfluidToken: burn amount exceeds balance"
        );

        const aliceBalance1 = await web3.eth.getBalance(alice);
        const tx = await web3tx(
            seth.downgradeToETH,
            "seth.downgradeToETH by alice"
        )(toWad(1), {
            from: alice,
        });
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
