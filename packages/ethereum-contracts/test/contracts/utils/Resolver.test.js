const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");

const Resolver = artifacts.require("Resolver");

const DEFAULT_ADMIN_ROLE =
    "0x0000000000000000000000000000000000000000000000000000000000000000";

describe("Resolver", function () {
    const t = TestEnvironment.getSingleton();

    let admin, alice;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({admin, alice} = t.aliases);
    });

    it("Resolver.set should only be called by admin", async () => {
        const resolver = await Resolver.new({from: admin});
        await expectRevertedWith(
            resolver.set("alice", alice, {from: alice}),
            "Caller is not an admin"
        );
        await resolver.grantRole(DEFAULT_ADMIN_ROLE, alice);
        await resolver.set("alice", alice, {from: alice});
        assert.equal(await resolver.get("alice"), alice);
    });
});
