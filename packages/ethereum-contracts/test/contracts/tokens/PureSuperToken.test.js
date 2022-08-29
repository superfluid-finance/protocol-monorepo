const {expectRevertedWith} = require("../../utils/expectRevert");

const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const TestEnvironment = require("../../TestEnvironment");

const {web3tx} = require("@decentral.ee/web3-helpers");
const {ethers} = require("hardhat");
const {toWad} = require("../utils/helpers");

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
        let tokenProxy = await ethers.getContractFactory("PureSuperToken");
        tokenProxy = await tokenProxy.deploy();
        await web3tx(
            superTokenFactory.initializeCustomSuperToken,
            "superTokenFactory.initializeCustomSuperToken"
        )(tokenProxy.address);
        await web3tx(tokenProxy.initialize, "tokenProxy.initialize")(
            "Didi Token",
            "DD",
            toWad(42).toString()
        );
        const token = await t.sf.contracts.ISuperToken.at(tokenProxy.address);
        assert.equal(
            (await token.balanceOf.call(admin)).toString(),
            toWad(42).toString()
        );
        await expectRevertedWith(
            tokenProxy.initialize("Hacker", "HH", toWad(0).toString()),
            "Initializable: contract is already initialized"
        );
    });
});
