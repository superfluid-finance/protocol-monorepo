const TestEnvironment = require("../../TestEnvironment");
// const {BN, expectRevert} = require("@openzeppelin/test-helpers");
// const {web3tx, toWad, toBN} = require("@decentral.ee/web3-helpers");
const {web3tx, toBN} = require("@decentral.ee/web3-helpers");
const { assertion } = require("@openzeppelin/test-helpers/src/expectRevert");
const expectEvent = require("@openzeppelin/test-helpers/src/expectEvent");
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
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
        });

        cfa = t.contracts.cfa;
        host = t.contracts.superfluid;


        ({alice, bob} = t.aliases);

        superToken = await SuperTokenMock.at(t.sf.tokens.TESTx.address);
        await superToken.mintInternal(alice, web3.utils.toWei("100000", "ether"), "0x", "0x")
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
        await superToken.transfer(
            CFALibraryMock.address,
            web3.utils.toWei("100", "ether"),
            {from: alice}
        );
    });

    describe("1 - Create, update, delete flow with no user data or extra ctx", async function () {
        it("1.1 - create flow with no user data or extra cfa ctx", async () => {
            await CFALibraryMock.createFlowTest(
                superToken.address,
                bob,
                "3858024691358", //10 per month
                {from: alice}
            );
            let flow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                bob
            );
            assert.equal(flow.flowRate, "3858024691358");
        });

        it("1.2 - update flow with no user data or extra cfa ctx", async () => {
            await CFALibraryMock.createFlowTest(
                superToken.address,
                bob,
                "3858024691358", //10 per month
                {from: alice}
            );
            await CFALibraryMock.updateFlowTest(
                superToken.address,
                bob,
                "1958024691358", //~5 per month
                {from: alice}
            );
            let flow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                bob
            );
            assert.equal(flow.flowRate, "1958024691358");
        });

        it("1.3 - delete flow with no user data or extra cfa ctx", async () => {
            await CFALibraryMock.createFlowTest(
                superToken.address,
                bob,
                "3858024691358", //10 per month
                {from: alice}
            );
            await CFALibraryMock.deleteFlowTest(
                superToken.address,
                // alice,
                bob,
                {from: alice}
            );
            let flow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                bob
            );
            assert.equal(flow.flowRate, "0");
        });
    });

});
