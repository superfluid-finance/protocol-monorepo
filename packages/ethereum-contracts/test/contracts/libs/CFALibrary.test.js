const TestEnvironment = require("../../TestEnvironment");
// const {BN, expectRevert} = require("@openzeppelin/test-helpers");
// const {web3tx, toWad, toBN} = require("@decentral.ee/web3-helpers");
const {web3tx, toBN} = require("@decentral.ee/web3-helpers");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const initialSupply = toBN(100);

// const traveler = require("ganache-time-traveler");

describe("CFA Library testing", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let superToken, host, cfa;
    let alice, bob;
    let CFALibraryMock;

    before(async () => {
        host = t.contracts.superfluid;
        cfa = t.contracts.cfa;

        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
        });

        ({alice, bob} = t.aliases);

        superToken = await SuperTokenMock.at(t.sf.tokens.TESTx.address);
        await web3tx(
            superToken.upgrade,
            `Upgrade initialSupply amount of token for ${alice}`
        )(initialSupply, {
            from: alice,
        });
    });

    beforeEach(async () => {
        //deploy a contract we'll use for testing the library
        let cfaLibraryMock = artifacts.require("CFALibraryMock");
        CFALibraryMock = await cfaLibraryMock.new(host.address, cfa.address);
        superToken.transfer(
            CFALibraryMock.address,
            web3.utils.toWei("100", "ether")
        );
    });

    describe("1 - Create flow", async function () {
        it("1.1 - create flow with no user data or extra cfa ctx", async () => {
            await CFALibraryMock.createFlowTest(
                bob.address,
                superToken.address,
                "3858024691358" //10 per month
            );
            let {timestamp, bal} = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                bob.address
            );
            console.log(timestamp);
            console.log(bal);

            //NOTE - still WIP
        });
    });
});
