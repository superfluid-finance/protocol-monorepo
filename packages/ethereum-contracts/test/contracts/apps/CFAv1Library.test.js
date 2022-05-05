const TestEnvironment = require("../../TestEnvironment");

const {web3tx, toBN} = require("@decentral.ee/web3-helpers");
const {expectRevertedWith} = require("../../utils/expectRevert");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const initialSupply = toBN(100);

describe("CFAv1 Library testing", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let superToken, host, cfa;
    let alice, bob;
    let CFALibraryMock;
    let TradeableCashflowMock;
    let SuperAppFlowOperatorMock;

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
        // deploy a contract we'll use for testing the library
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

        SuperAppFlowOperatorMock = await artifacts
            .require("SuperAppFlowOperatorMock")
            .new(
                host.address,
                alice, // sender
                bob, // receiver
                alice // operator
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
            await expectRevertedWith(
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
            await expectRevertedWith(
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
            await expectRevertedWith(
                CFALibraryMock.deleteFlowTest(superToken.address, alice, {
                    from: bob,
                }),
                "CFA: flow does not exist"
            );
        });

        it("4.4 - It should revert if given an invalid ctx", async () => {
            await expectRevertedWith(
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

    describe("5 - Operator Cases", async () => {
        const callbackFunctionIndex = {
            CREATE_FLOW_BY_OPERATOR: 0,
            UPDATE_FLOW_BY_OPERATOR: 1,
            DELETE_FLOW_BY_OPERATOR: 2,
            UPDATE_FLOW_OPERATOR_PERMISSIONS: 3,
            AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL: 4,
            REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL: 5,
        };

        const flowRate = "100000000"; // hard coded in the contract

        // testing permissions first before testing operator functions
        it("5.1 - Can Update Flow Operator Permissions", async () => {
            await CFALibraryMock.updateFlowOperatorPermissionsTest(
                alice,
                superToken.address,
                "7",
                "1"
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        CFALibraryMock.address,
                        alice
                    )
                ).permissions.toString(),
                "7"
            );
        });

        it("5.2 - Can Authorize Flow Operator With full Control", async () => {
            await CFALibraryMock.authorizeFlowOperatorWithFullControlTest(
                alice,
                superToken.address
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        CFALibraryMock.address,
                        alice
                    )
                ).permissions.toString(),
                "7"
            );
        });

        it("5.3 - Can Revoke Flow Operator With Full Control", async () => {
            await CFALibraryMock.authorizeFlowOperatorWithFullControlTest(
                alice,
                superToken.address
            );

            await CFALibraryMock.revokeFlowOperatorWithFullControlTest(
                alice,
                superToken.address
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        CFALibraryMock.address,
                        alice
                    )
                ).permissions.toString(),
                "0"
            );
        });

        it("5.4 - Can Create Flow By Operator", async () => {
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        CFALibraryMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await CFALibraryMock.createFlowByOperatorTest(
                alice,
                bob,
                superToken.address,
                "1"
            );

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                "1"
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .deleteFlow(superToken.address, alice, bob, "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );
        });

        it("5.5 - Can Update Flow By Operator", async () => {
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        CFALibraryMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(superToken.address, bob, "1", "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await CFALibraryMock.updateFlowByOperatorTest(
                alice,
                bob,
                superToken.address,
                "2"
            );

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                "2"
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .deleteFlow(superToken.address, alice, bob, "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );
        });

        it("5.6 - Can Delete Flow By Operator", async () => {
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        CFALibraryMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(superToken.address, bob, "1", "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await CFALibraryMock.deleteFlowByOperator(
                alice,
                bob,
                superToken.address
            );

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("5.7 - Can Create Flow By Operator in Callback", async () => {
            // alice approves super app as operator, alice creates a flow to super app which creates
            // a flow from alice to bob on alice's behalf.
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "1",
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.CREATE_FLOW_BY_OPERATOR
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                "100000000"
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .deleteFlow(superToken.address, alice, bob, "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );
        });

        it("5.8 - Can Update Flow By Operator in Callback", async () => {
            // alice approves super app as operator, alice creates a flow to bob, alice creates a
            // flow to super app which updates the flow from alice to bob on alice's behalf.
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(superToken.address, bob, "1", "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "1",
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.UPDATE_FLOW_BY_OPERATOR
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowRate
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .deleteFlow(superToken.address, alice, bob, "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );
        });

        it("5.9 - Can Delete Flow By Operator in Callback", async () => {
            // alice approves super app as operator, alice creates a flow to bob, alice creates a
            // flow to super app which deletes the flow from alice to bob on alice's behalf.
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(superToken.address, bob, "1", "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "1",
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.DELETE_FLOW_BY_OPERATOR
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("5.10 - Can Update Flow Operator Permissions in Callback", async () => {
            // alice creates a flow to the super app which sets alice as the operator for it
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "1",
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.UPDATE_FLOW_OPERATOR_PERMISSIONS
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        alice
                    )
                ).permissions.toString(),
                "7"
            );
        });

        it("5.11 - Can Authorize Flow Operator With Full Control in Callback", async () => {
            // alice creates a flow to the super app which sets alice as the operator for it
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "1",
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        alice
                    )
                ).permissions.toString(),
                "7"
            );
        });

        it("5.12 - Can Revoke Flow Operator With Full Control in Callback", async () => {
            // alice sets theirself as the operator for the super app, alice creates a flow to
            // the super app which revokes alice's operator permissions
            await SuperAppFlowOperatorMock.authorizeFlowOperatorWithFullControl(
                superToken.address,
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        "1",
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        SuperAppFlowOperatorMock.address,
                        alice
                    )
                ).permissions.toString(),
                "0"
            );
        });
    });
});
