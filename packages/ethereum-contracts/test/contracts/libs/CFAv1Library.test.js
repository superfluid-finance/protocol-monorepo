const TestEnvironment = require("../../TestEnvironment");

// const {BN, expectRevert} = require("@openzeppelin/test-helpers");
// const {web3tx, toWad, toBN} = require("@decentral.ee/web3-helpers");
const {web3tx, toBN} = require("@decentral.ee/web3-helpers");
const {expectRevert} = require("@openzeppelin/test-helpers");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const initialSupply = toBN(100);

// const traveler = require("ganache-time-traveler");

describe("CFAv1 Library testing", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let superToken, host, cfa;
    let alice, bob;
    let CFALibraryMock;
    let TradeableCashflowMock;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
        });

        cfa = t.contracts.cfa;
        host = t.contracts.superfluid;

        ({alice, bob} = t.aliases);

        superToken = await SuperTokenMock.at(t.sf.tokens.TESTx.address);
        await superToken.mintInternal(
            alice,
            web3.utils.toWei("100000", "ether"),
            "0x",
            "0x"
        );
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
        CFALibraryMock = await cfaLibraryMock.new(host.address);
        await superToken.transfer(
            CFALibraryMock.address,
            web3.utils.toWei("1000", "ether"),
            {from: alice}
        );

        let tradeableCashflowMock = artifacts.require("TradeableCashflowMock");

        TradeableCashflowMock = await tradeableCashflowMock.new(
            bob,
            "Tradeable Cashflow",
            "TCF",
            host.address,
            superToken.address
        );
    });

    describe("1 - Create, update, delete flow with no user data or extra ctx", async function () {
        it("1.1 - create flow with no user data", async () => {
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

        it("1.2 - update flow with no user data", async () => {
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

        it("1.3 - delete flow with no user data", async () => {
            await CFALibraryMock.createFlowTest(
                superToken.address,
                bob,
                "3858024691358", //10 per month
                {from: alice}
            );
            await CFALibraryMock.deleteFlowTest(superToken.address, bob, {
                from: alice,
            });
            let flow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                bob
            );
            assert.equal(flow.flowRate, "0");
        });
    });

    describe("2 - Create, update, delete flow with user data", async function () {
        it("2.1 - create flow with user data", async () => {
            await CFALibraryMock.createFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                "3858024691358", //10 per month
                web3.eth.abi.encodeParameter("string", "HODL"),
                {from: alice}
            );

            let inFlow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                TradeableCashflowMock.address
            );

            let outFlow = await cfa.getFlow(
                superToken.address,
                TradeableCashflowMock.address,
                bob
            );

            assert.equal(inFlow.flowRate, "3858024691358");
            //this second assertion tests the callback inside of tradeable cashflow
            assert.equal(outFlow.flowRate, "3858024691358");
            assert.equal(await TradeableCashflowMock.userData(), "HODL");
        });

        it("2.2 - update flow with user data ", async () => {
            await CFALibraryMock.createFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                "3858024691358", //10 per month
                web3.eth.abi.encodeParameter("string", "HODL"),
                {from: alice}
            );

            await CFALibraryMock.updateFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                "1958024691358", //10 per month
                web3.eth.abi.encodeParameter("string", "WAGMI"),
                {from: alice}
            );

            let inFlow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                TradeableCashflowMock.address
            );

            let outFlow = await cfa.getFlow(
                superToken.address,
                TradeableCashflowMock.address,
                bob
            );

            assert.equal(inFlow.flowRate, "1958024691358");
            //this second assertion tests the callback inside of tradeable cashflow
            assert.equal(outFlow.flowRate, "1958024691358");
            assert.equal(await TradeableCashflowMock.userData(), "WAGMI");
        });

        it("2.3 - delete flow with user data", async () => {
            await CFALibraryMock.createFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                "3858024691358", //10 per month
                web3.eth.abi.encodeParameter("string", "HODL"),
                {from: alice}
            );

            await CFALibraryMock.deleteFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                web3.eth.abi.encodeParameter("string", "NGMI"),
                {from: alice}
            );

            let inFlow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                TradeableCashflowMock.address
            );

            let outFlow = await cfa.getFlow(
                superToken.address,
                TradeableCashflowMock.address,
                bob
            );

            assert.equal(inFlow.flowRate, "0");
            //this second assertion tests the callback inside of tradeable cashflow
            assert.equal(outFlow.flowRate, "0");
            assert.equal(await TradeableCashflowMock.userData(), "NGMI");
        });
    });

    describe("3 - Create, update, delete flow w user data, and check withCtx user data functions", async function () {
        it("3.1 - create flow with user data, run withCtx user data in callback", async () => {
            await CFALibraryMock.createFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                "5858024691358", //10 per month
                web3.eth.abi.encodeParameter("string", "HODL"),
                {from: alice}
            );

            let inFlow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                TradeableCashflowMock.address
            );

            let outFlow = await cfa.getFlow(
                superToken.address,
                TradeableCashflowMock.address,
                bob
            );

            assert.equal(inFlow.flowRate, "5858024691358");
            //this second assertion tests the callback inside of tradeable cashflow
            assert.equal(outFlow.flowRate, "5858024691358");
            assert.equal(await TradeableCashflowMock.userData(), "HODL");
        });

        it("3.2 - update flow with user data, run withCtx user data in callback", async () => {
            await CFALibraryMock.createFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                "5858024691358", //10 per month
                web3.eth.abi.encodeParameter("string", "HODL"),
                {from: alice}
            );

            await CFALibraryMock.updateFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                "6958024691358", //10 per month
                web3.eth.abi.encodeParameter("string", "WAGMI"),
                {from: alice}
            );

            let inFlow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                TradeableCashflowMock.address
            );

            let outFlow = await cfa.getFlow(
                superToken.address,
                TradeableCashflowMock.address,
                bob
            );

            assert.equal(inFlow.flowRate, "6958024691358");
            //this second assertion tests the callback inside of tradeable cashflow
            assert.equal(outFlow.flowRate, "6958024691358");
            assert.equal(await TradeableCashflowMock.userData(), "WAGMI");
        });

        it("3.3 - delete flow with user data, run withCtx user data in callback", async () => {
            await CFALibraryMock.createFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                "5858024691358", //10 per month
                web3.eth.abi.encodeParameter("string", "HODL"),
                {from: alice}
            );

            await CFALibraryMock.deleteFlowWithUserDataTest(
                superToken.address,
                TradeableCashflowMock.address,
                web3.eth.abi.encodeParameter("string", "NGMI"),
                {from: alice}
            );

            let inFlow = await cfa.getFlow(
                superToken.address,
                CFALibraryMock.address,
                TradeableCashflowMock.address
            );

            let outFlow = await cfa.getFlow(
                superToken.address,
                TradeableCashflowMock.address,
                bob
            );

            assert.equal(inFlow.flowRate, "0");
            //this second assertion tests the callback inside of tradeable cashflow
            assert.equal(outFlow.flowRate, "0");
            assert.equal(await TradeableCashflowMock.userData(), "NGMI");
        });
    });

    describe("4 - Expect revert cases", async () => {
        it("4.1 - Create should revert if flow exists", async () => {
            await CFALibraryMock.createFlowTest(
                superToken.address,
                bob,
                "3858024691358", //10 per month
                {from: alice}
            );
            await expectRevert(
                CFALibraryMock.createFlowTest(
                    superToken.address,
                    bob,
                    "2858024691358", //10 per month
                    {from: alice}
                ),
                "CFA: flow already exist"
            );
        });

        it("4.2 - Update should revert if flow does not exist", async () => {
            await expectRevert(
                CFALibraryMock.updateFlowTest(
                    superToken.address,
                    alice,
                    "2858024691358", //10 per month
                    {from: bob}
                ),
                "CFA: flow does not exist"
            );
        });

        it("4.3 - Delete should revert if flow does not exist", async () => {
            await expectRevert(
                CFALibraryMock.deleteFlowTest(superToken.address, alice, {
                    from: bob,
                }),
                "CFA: flow does not exist"
            );
        });

        it("4.4 - It should revert if given an invalid ctx", async () => {
            await expectRevert(
                CFALibraryMock.createFlowWithCtxTest(
                    "0x",
                    bob,
                    superToken.address,
                    "2858024691358"
                ),
                "SF: APP_RULE_CTX_IS_NOT_VALID"
            );
        });
    });
});
