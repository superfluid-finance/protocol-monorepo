const { expectRevert } = require("@openzeppelin/test-helpers");

const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
const TestEnvironment = require("../../TestEnvironment");
const NativeSuperTokenProxy = artifacts.require("NativeSuperTokenProxy");

const { web3tx, toWad } = require("@decentral.ee/web3-helpers");

contract("NativeSuperTokenProxy Contract", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 3), {
        isTruffle: true,
        useMocks: true,
    });

    let superTokenFactory;

    before(async () => {
        await t.reset();
        superTokenFactory = await ISuperTokenFactory.at(
            await t.contracts.superfluid.getSuperTokenFactory()
        );
    });

    it("#1 create token", async () => {
        const tokenProxy = await NativeSuperTokenProxy.new();
        await web3tx(
            superTokenFactory.initializeCustomSuperToken,
            "superTokenFactory.initializeCustomSuperToken"
        )(tokenProxy.address);
        await web3tx(tokenProxy.initialize, "tokenProxy.initialize")(
            "Didi Token",
            "DD",
            toWad(42)
        );
        await expectRevert(
            tokenProxy.initialize("Hacker", "HH", toWad(0)),
            "Initializable: contract is already initialized"
        );
    });
});
