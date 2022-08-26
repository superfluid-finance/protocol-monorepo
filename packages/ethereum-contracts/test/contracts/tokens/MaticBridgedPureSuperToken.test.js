const {expectEvent} = require("@openzeppelin/test-helpers");
const {expectRevertedWith} = require("../../utils/expectRevert");

const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const TestEnvironment = require("../../TestEnvironment");
const MaticBridgedPureSuperTokenProxy = artifacts.require(
    "MaticBridgedPureSuperTokenProxy"
);
const IMaticBridgedPureSuperToken = artifacts.require(
    "IMaticBridgedPureSuperToken"
);

const {web3tx, toWad} = require("@decentral.ee/web3-helpers");

describe("MaticBridgedPureSuperTokenProxy Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let admin, bob, chainMgr;
    let superTokenFactory;
    let tokenProxy, token;

    const AMOUNT_1 = toWad(3);
    const AMOUNT_2 = toWad(5000);

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
            tokens: [],
        });

        ({admin, alice: chainMgr, bob} = t.aliases);
        console.log(`aliases: ${JSON.stringify(t.aliases, null, 2)}`);
        console.log(`chainMgr: ${chainMgr}`);
        superTokenFactory = await ISuperTokenFactory.at(
            await t.contracts.superfluid.getSuperTokenFactory()
        );
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
        tokenProxy = await MaticBridgedPureSuperTokenProxy.new(chainMgr);

        await web3tx(
            superTokenFactory.initializeCustomSuperToken,
            "superTokenFactory.initializeCustomSuperToken"
        )(tokenProxy.address);

        token = await IMaticBridgedPureSuperToken.at(tokenProxy.address);
    });

    it("#1 can initialize only once", async () => {
        await web3tx(token.initialize, "token.initialize")(
            t.constants.ZERO_ADDRESS,
            18,
            "Matic Bridged Token",
            "MBT"
        );

        assert.equal(
            (await token.balanceOf.call(admin)).toString(),
            toWad(0).toString()
        );
        await expectRevertedWith(
            token.initialize(
                t.constants.ZERO_ADDRESS,
                18,
                "Hacked Matic Bridged Token",
                "HMBT"
            ),
            "Initializable: contract is already initialized"
        );
    });

    it("#2 bridge interface permissions", async () => {
        await web3tx(token.initialize, "token.initialize")(
            t.constants.ZERO_ADDRESS,
            18,
            "MBT",
            "MBT"
        );

        await expectRevertedWith(
            token.deposit(
                bob,
                web3.eth.abi.encodeParameter("uint256", AMOUNT_1)
            ),
            "MBPSuperToken: no permission to deposit"
        );

        await token.deposit(
            bob,
            web3.eth.abi.encodeParameter("uint256", AMOUNT_1),
            {from: chainMgr}
        );

        await expectRevertedWith(
            token.withdraw(AMOUNT_1),
            "SuperfluidToken: burn amount exceeds balance"
        );

        await token.withdraw(AMOUNT_1, {from: bob});

        await expectRevertedWith(
            token.updateChildChainManager(bob),
            "MBPSuperToken: only governance allowed"
        );
    });

    it("#3 bridge interface correct balance changes", async () => {
        await web3tx(token.initialize, "token.initialize")(
            t.constants.ZERO_ADDRESS,
            18,
            "MBT",
            "MBT"
        );

        assert.equal(
            (await token.balanceOf(bob)).toString(),
            toWad(0).toString()
        );
        const r1 = await token.deposit(
            bob,
            web3.eth.abi.encodeParameter("uint256", AMOUNT_1),
            {from: chainMgr}
        );
        await expectEvent(r1, "Transfer", {
            from: t.constants.ZERO_ADDRESS,
            to: bob,
            value: AMOUNT_1,
        });
        assert.equal(
            (await token.balanceOf(bob)).toString(),
            AMOUNT_1.toString()
        );

        await token.deposit(
            bob,
            web3.eth.abi.encodeParameter("uint256", AMOUNT_2),
            {from: chainMgr}
        );
        assert.equal(
            (await token.balanceOf(bob)).toString(),
            AMOUNT_1.add(AMOUNT_2).toString()
        );
        assert.equal(
            (await token.balanceOf(bob)).toString(),
            AMOUNT_1.add(AMOUNT_2).toString()
        );

        await token.withdraw(AMOUNT_1, {from: bob});
        assert.equal(
            (await token.balanceOf(bob)).toString(),
            AMOUNT_2.toString()
        );
        assert.equal(
            (await token.totalSupply()).toString(),
            AMOUNT_2.toString()
        );
    });
});
