const {expectRevertedWith} = require("../../utils/expectRevert");

const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const TestEnvironment = require("../../TestEnvironment");
const PureSuperToken = artifacts.require("PureSuperToken");

const {web3tx, toWad} = require("@decentral.ee/web3-helpers");

describe("PureSuperToken Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let admin;
    let superTokenFactory;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 1,
        });

        ({admin} = t.aliases);
        superTokenFactory = await ISuperTokenFactory.at(
            await t.contracts.superfluid.getSuperTokenFactory()
        );
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    it("#1 initialization", async () => {
        const tokenProxy = await PureSuperToken.new();
        await web3tx(
            superTokenFactory.initializeCustomSuperToken,
            "superTokenFactory.initializeCustomSuperToken"
        )(tokenProxy.address);
        await web3tx(tokenProxy.initialize, "tokenProxy.initialize")(
            "Didi Token",
            "DD",
            toWad(42)
        );
        const token = await t.sf.contracts.ISuperToken.at(tokenProxy.address);
        assert.equal(
            (await token.balanceOf.call(admin)).toString(),
            toWad(42).toString()
        );
        await expectRevertedWith(
            tokenProxy.initialize("Hacker", "HH", toWad(0)),
            "Initializable: contract is already initialized"
        );
    });
});
