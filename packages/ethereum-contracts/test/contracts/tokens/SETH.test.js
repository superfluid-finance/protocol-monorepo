const { expectRevert } = require("@openzeppelin/test-helpers");

const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const TestEnvironment = require("../../TestEnvironment");
const WETH9Mock = artifacts.require("WETH9Mock");
const SETH = artifacts.require("SETH");
const SETHProxy = artifacts.require("SETHProxy");

const { web3tx, toWad } = require("@decentral.ee/web3-helpers");

contract("Super ETH (SETH) Contract", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 2), {
        isTruffle: true,
        useMocks: true
    });
    const { alice } = t.aliases;

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
        seth = await SETH.at((await SETHProxy.new(weth.address)).address);
        await web3tx(
            superTokenFactory.initializeCustomSuperToken,
            "initializeCustomSuperToken"
        )(seth.address);
        await web3tx(seth.initialize, "seth.initialize")(
            weth.address,
            18,
            "Super ETH",
            "SETH"
        );
    });

    it("#1.1 upgrade by WETH", async () => {
        await web3tx(
            weth.deposit,
            "weth.deposit 1wad"
        )({
            from: alice,
            value: toWad(1)
        });
        assert.equal(
            (await weth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );
        await web3tx(weth.approve, "weth.approve by alice to seth")(
            seth.address,
            toWad(1),
            {
                from: alice
            }
        );

        await web3tx(seth.upgrade, "seth.upgrade by alice")(toWad(1), {
            from: alice
        });
        assert.equal((await weth.balanceOf(alice)).toString(), "0");
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );

        await expectRevert(
            seth.downgrade(toWad(1).addn(1), { from: alice }),
            "SuperfluidToken: burn amount exceeds balance"
        );

        await web3tx(seth.downgrade, "seth.downgrade by alice")(toWad(1), {
            from: alice
        });
        assert.equal(
            (await weth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );
        assert.equal((await seth.balanceOf(alice)).toString(), "0");
    });

    it("#1.2 upgrade by ETH", async () => {
        await web3tx(
            seth.upgradeByETH,
            "seth.upgradeByETH by alice"
        )({
            from: alice,
            value: toWad(1)
        });
        assert.equal(
            (await seth.balanceOf(alice)).toString(),
            toWad(1).toString()
        );
        assert.equal(
            (await web3.eth.getBalance(seth.address)).toString(),
            toWad(1).toString()
        );

        await expectRevert(
            seth.downgradeToETH(toWad(1).addn(1), { from: alice }),
            "SuperfluidToken: burn amount exceeds balance"
        );

        await web3tx(seth.downgradeToETH, "seth.downgradeToETH by alice")(
            toWad(1),
            {
                from: alice
            }
        );
        assert.equal((await seth.balanceOf(alice)).toString(), "0");
        assert.equal((await web3.eth.getBalance(seth.address)).toString(), "0");
    });
});
