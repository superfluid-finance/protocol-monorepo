const { expectEvent } = require("@openzeppelin/test-helpers");
const {
    toWad,
    wad4human
} = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("../../TestEnvironment");
const traveler = require("ganache-time-traveler");

const CLOWNS = artifacts.require("CLOWNS");

contract("CLOWNS", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 4), {
        isTruffle: true,
        useMocks: true,
    });
    const { admin, alice, bob, carol } = t.aliases;

    let superfluid;
    let clowns;
    let erc1820;
    let superToken;
    const MIN_BOND_DURATION = 3600 * 24 * 7;

    before(async () => {
        await t.reset();
        ({ superfluid, erc1820 } = t.contracts);
        clowns = await CLOWNS.new(superfluid.address, MIN_BOND_DURATION);

        await t.createNewToken({ doUpgrade: true });
        ({ superToken } = t.contracts);
        console.log(`superToken addr: ${superToken.address}`);
    });

    // copied over from ConstantFlowAgreementV1.test.js - should probably be a shared fn
    async function timeTravelOnce(time = MIN_BOND_DURATION) {
        const block1 = await web3.eth.getBlock("latest");
        console.log("current block time", block1.timestamp);
        console.log(`time traveler going to the future +${time}...`);
        await traveler.advanceTimeAndBlock(time);
        const block2 = await web3.eth.getBlock("latest");
        console.log("new block time", block2.timestamp);
    }

    it("#1 alice becomes CLO", async () => {
        const aliceBal = await superToken.balanceOf(alice);
        console.log(`alice balance: ${aliceBal}`);
        const bondAmount = toWad(3);
        const exitRate = web3.eth.abi.encodeParameter("int96", 1);
        const r1 = await superToken.send(clowns.address, bondAmount, exitRate, {
            from: alice,
        });

        expectEvent.inTransaction(r1.tx, clowns.contract, "NewCLO", {
            token: superToken.address,
            clo: alice,
            bond: bondAmount,
            exitRate: "1",
        });

        const curCLO = await clowns.getCurrentCLO(superToken.address);
        assert.equal(curCLO, alice);

        const { remainingBond } = await clowns.getCurrentCLOBond(
            superToken.address
        );
        console.log(
            `remaining bond: ${wad4human(
                remainingBond
            )} (${remainingBond.toString()})`
        );

        // exit stream
    });

    // check erc1820 registration

    // enforce exitRate limits

    // interpret missing exitRate as default (min) exitRate

    // enforce min bid limit

    // handle initial bid with non-zero contract balance

    // changeFlowrate

    // CLO closes stream: nothing breaks - can reopen with changeFlowrate

    // rewards are added

    // bond goes to zero
});
