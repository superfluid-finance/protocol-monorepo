const { expectRevert, expectEvent } = require("@openzeppelin/test-helpers");

const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const TestEnvironment = require("../../TestEnvironment");
const WETH9Mock = artifacts.require("WETH9Mock");
const ISETH = artifacts.require("ISETH");
const SETHProxy = artifacts.require("SETHProxy");

const { web3tx, toBN, toWad } = require("@decentral.ee/web3-helpers");

contract("Super ETH (SETH) Contract", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 3), {
        isTruffle: true,
        useMocks: true,
    });
    const { alice, bob } = t.aliases;

    let superTokenFactory;
    let weth;
    let seth;

    before(async () => {
        await t.reset();
        superTokenFactory = await ISuperTokenFactory.at(
            await t.contracts.superfluid.getSuperTokenFactory()
        );
    });

    beforeEach(async () => {
        weth = await WETH9Mock.new();
        const sethProxy = await SETHProxy.new(weth.address);
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

    it("#1.3 upgradeByWETH", async () => {
        await web3tx(
            weth.deposit,
            "weth.deposit 1wad"
        )({
            from: alice,
            value: toWad(1),
        });
        assert.equal(
            (await weth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );
        await web3tx(weth.approve, "weth.approve by alice to seth")(
            seth.address,
            toWad(1),
            {
                from: alice,
            }
        );

        const tx = await web3tx(seth.upgradeByWETH, "seth.upgrade by alice")(
            toWad(1),
            {
                from: alice,
            }
        );
        await expectEvent.inTransaction(
            tx.tx,
            t.sf.contracts.ISuperToken, // see if it's compatible with ISuperToken events
            "TokenUpgraded",
            {
                account: alice,
                amount: toWad(1).toString(),
            }
        );
        assert.equal((await weth.balanceOf(alice)).toString(), "0");
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

        await expectRevert(
            seth.downgradeToETH(toWad(1).addn(1), { from: alice }),
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

    it("#1.5 downgradeToWETH", async () => {
        await web3tx(
            seth.upgradeByETH,
            "seth.upgradeByETH by alice"
        )({
            value: toWad(1),
            from: alice,
        });

        await expectRevert(
            seth.downgradeToWETH(toWad(1).addn(1), {
                from: alice,
            }),
            "SuperfluidToken: burn amount exceeds balance"
        );

        const tx = await web3tx(
            seth.downgradeToWETH,
            "seth.downgradeToWETH by alice"
        )(toWad(1), {
            from: alice,
        });
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
            (await seth.balanceOf(alice)).toString(),
            toWad(0).toString()
        );
        assert.equal(
            (await weth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            toWad(0).toString()
        );
    });

    it("#1.6 downgrade to both", async () => {
        await web3tx(
            seth.upgradeByETH,
            "seth.upgradeByETH by alice"
        )({
            value: toWad(1),
            from: alice,
        });

        await web3tx(seth.downgradeToETH, "seth.downgradeToETH by alice")(
            toWad("0.5"),
            {
                from: alice,
            }
        );
        assert.equal(
            (await weth.balanceOf(alice)).toString(),
            toWad("0").toString()
        );
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            toWad("0.5").toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            toWad("0.5").toString()
        );
        assert.equal((await weth.balanceOf(alice)).toString(), "0");

        await web3tx(seth.downgradeToWETH, "seth.downgradeToWETH by alice")(
            toWad("0.5"),
            {
                from: alice,
            }
        );
        assert.equal(
            (await weth.balanceOf(alice)).toString(),
            toWad("0.5").toString()
        );
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            toWad("0").toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            toWad("0").toString()
        );
    });

    it("#1.7 - Direct send Ether", async () => {
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
