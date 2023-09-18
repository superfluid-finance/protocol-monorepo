import {artifacts, assert, ethers} from "hardhat";

import {ISuperTokenFactory} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";
import {expectRevertedWith} from "../../utils/expectRevert";
import {toWad} from "../utils/helpers";

const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");

const {web3tx} = require("@decentral.ee/web3-helpers");

describe("PureSuperToken Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let admin: string;
    let superTokenFactory: ISuperTokenFactory;

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
        t.beforeEachTestCaseBenchmark(this);
    });

    afterEach(async () => {
        t.afterEachTestCaseBenchmark();
    });

    it("#1 initialization", async () => {
        const PureSuperTokenFactory =
            await ethers.getContractFactory("PureSuperToken");
        const PureSuperToken = await PureSuperTokenFactory.deploy();
        await web3tx(
            superTokenFactory.initializeCustomSuperToken,
            "superTokenFactory.initializeCustomSuperToken"
        )(PureSuperToken.address);
        await web3tx(PureSuperToken.initialize, "PureSuperToken.initialize")(
            "Didi Token",
            "DD",
            toWad(42).toString()
        );
        const token = await ethers.getContractAt(
            "ISuperToken",
            PureSuperToken.address
        );
        assert.equal(
            (await token.balanceOf(admin)).toString(),
            toWad(42).toString()
        );
        await expectRevertedWith(
            PureSuperToken.initialize("Hacker", "HH", toWad(0).toString()),
            "Initializable: contract is already initialized"
        );
    });
});
