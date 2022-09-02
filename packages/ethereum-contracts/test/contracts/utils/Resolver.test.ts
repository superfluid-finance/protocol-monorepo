import {assert} from "chai";
const {ethers} = require("hardhat");
const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");

const DEFAULT_ADMIN_ROLE =
    "0x0000000000000000000000000000000000000000000000000000000000000000";

describe("Resolver", function () {
    const t = TestEnvironment.getSingleton();

    let admin: string, alice: string;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({admin, alice} = t.aliases);
    });

    it("Resolver.set should only be called by admin", async () => {
        const adminSigner = await ethers.getSigner(admin);
        const aliceSigner = await ethers.getSigner(alice);
        let resolver = await ethers.getContractFactory("Resolver");
        resolver = await resolver.connect(adminSigner).deploy();
        await expectRevertedWith(
            resolver.connect(aliceSigner).set("alice", alice),
            "Caller is not an admin"
        );
        await resolver.grantRole(DEFAULT_ADMIN_ROLE, alice);
        await resolver.connect(aliceSigner).set("alice", alice);
        assert.equal(await resolver.get("alice"), alice);
    });
});
