const TestEnvironment = require("../../TestEnvironment");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const CFALibraryMock = artifacts.require("CFALibraryMock");
const CFALibrarySuperAppMock = artifacts.require("CFALibrarySuperAppMock");

const mintAmount = "1000000000000000000000000000"; // a small loan of a billion dollars
const flowRate = "1000000000000";
const updatedFlowRate = "2000000000000";

// used to trigger different functions to test in the same callback
const callbackFunctionIndex = {
    CREATE_FLOW: 0,
    UPDATE_FLOW: 1,
    DELETE_FLOW: 2,
    CREATE_FLOW_BY_OPERATOR: 3,
    UPDATE_FLOW_BY_OPERATOR: 4,
    DELETE_FLOW_BY_OPERATOR: 5,
    UPDATE_FLOW_OPERATOR_PERMISSIONS: 6,
    AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL: 7,
    REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL: 8,
};

describe("CFAv1 Library testing", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let superToken, host, cfa;
    let alice, bob;
    let cfaLibraryMock;
    let cfaLibrarySuperAppMock;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 3,
        });

        cfa = t.contracts.cfa;
        host = t.contracts.superfluid;

        ({alice, bob} = t.aliases);
    });

    beforeEach(async () => {
        superToken = await SuperTokenMock.new(host.address, "69");
        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken.mintInternal(bob, mintAmount, "0x", "0x");

        // deploy a contract we'll use for testing the library
        cfaLibraryMock = await CFALibraryMock.new(host.address);

        cfaLibrarySuperAppMock = await CFALibrarySuperAppMock.new(
            host.address,
            alice, // sender
            bob, // receiver
            alice // operator
        );

        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken.transfer(cfaLibraryMock.address, mintAmount, {
            from: alice,
        });

        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken.transfer(cfaLibrarySuperAppMock.address, mintAmount, {
            from: alice,
        });
    });

    describe("1 - Flow Ops", async function () {
        it("1.1 - Create Flow", async () => {
            await cfaLibraryMock.createFlowTest(
                superToken.address,
                bob,
                flowRate
            );

            assert.equal(
                (
                    await cfa.getFlow(
                        superToken.address,
                        cfaLibraryMock.address,
                        bob
                    )
                ).flowRate.toString(),
                flowRate
            );
        });

        it("1.2 - Update Flow", async () => {
            await cfaLibraryMock.createFlowTest(
                superToken.address,
                bob,
                flowRate
            );

            await cfaLibraryMock.updateFlowTest(
                superToken.address,
                bob,
                updatedFlowRate
            );

            assert.equal(
                (
                    await cfa.getFlow(
                        superToken.address,
                        cfaLibraryMock.address,
                        bob
                    )
                ).flowRate.toString(),
                updatedFlowRate
            );
        });

        it("1.3 - Delete Flow", async () => {
            await cfaLibraryMock.createFlowTest(
                superToken.address,
                bob,
                flowRate
            );

            await cfaLibraryMock.deleteFlowTest(superToken.address, bob);

            assert.equal(
                (
                    await cfa.getFlow(
                        superToken.address,
                        cfaLibraryMock.address,
                        bob
                    )
                ).flowRate.toString(),
                "0"
            );
        });

        it("1.4 - Create Flow in Callback", async () => {
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        flowRate,
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.CREATE_FLOW
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        bob
                    )
                ).flowRate.toString(),
                flowRate
            );
        });

        it("1.5 - Update Flow in Callback", async () => {
            await cfaLibrarySuperAppMock.createFlow(superToken.address);

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        flowRate,
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.UPDATE_FLOW
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        bob
                    )
                ).flowRate.toString(),
                updatedFlowRate
            );
        });

        it("1.6 - Delete Flow in Callback", async () => {
            await cfaLibrarySuperAppMock.createFlow(superToken.address);

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        flowRate,
                        "0x"
                    )
                    .encodeABI(),
                web3.eth.abi.encodeParameter(
                    "uint8",
                    callbackFunctionIndex.DELETE_FLOW
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await cfa.getFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        bob
                    )
                ).flowRate.toString(),
                "0"
            );
        });
    });

    describe("2 - Flow Operator Ops", async () => {
        // testing permissions first before testing operator functions
        it("2.1 - Can Update Flow Operator Permissions", async () => {
            await cfaLibraryMock.updateFlowOperatorPermissionsTest(
                alice,
                superToken.address,
                "7",
                "1"
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        cfaLibraryMock.address,
                        alice
                    )
                ).permissions.toString(),
                "7"
            );
        });

        it("2.2 - Can Authorize Flow Operator With full Control", async () => {
            await cfaLibraryMock.authorizeFlowOperatorWithFullControlTest(
                alice,
                superToken.address
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        cfaLibraryMock.address,
                        alice
                    )
                ).permissions.toString(),
                "7"
            );
        });

        it("2.3 - Can Revoke Flow Operator With Full Control", async () => {
            await cfaLibraryMock.authorizeFlowOperatorWithFullControlTest(
                alice,
                superToken.address
            );

            await cfaLibraryMock.revokeFlowOperatorWithFullControlTest(
                alice,
                superToken.address
            );

            assert.equal(
                (
                    await cfa.getFlowOperatorData(
                        superToken.address,
                        cfaLibraryMock.address,
                        alice
                    )
                ).permissions.toString(),
                "0"
            );
        });

        it("2.4 - Can Create Flow By Operator", async () => {
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        cfaLibraryMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await cfaLibraryMock.createFlowByOperatorTest(
                alice,
                bob,
                superToken.address,
                flowRate
            );

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowRate
            );
        });

        it("2.5 - Can Update Flow By Operator", async () => {
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        cfaLibraryMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(superToken.address, bob, flowRate, "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await cfaLibraryMock.updateFlowByOperatorTest(
                alice,
                bob,
                superToken.address,
                updatedFlowRate
            );

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                updatedFlowRate
            );
        });

        it("2.6 - Can Delete Flow By Operator", async () => {
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        cfaLibraryMock.address,
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

            await cfaLibraryMock.deleteFlowByOperator(
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

        it("2.7 - Can Create Flow By Operator in Callback", async () => {
            // alice approves super app as operator, alice creates a flow to super app which creates
            // a flow from alice to bob on alice's behalf.
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
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
                        cfaLibrarySuperAppMock.address,
                        flowRate,
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
                flowRate
            );
        });

        it("2.8 - Can Update Flow By Operator in Callback", async () => {
            // alice approves super app as operator, alice creates a flow to bob, alice creates a
            // flow to super app which updates the flow from alice to bob on alice's behalf.
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(superToken.address, bob, flowRate, "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        flowRate,
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
                updatedFlowRate
            );
        });

        it("2.9 - Can Delete Flow By Operator in Callback", async () => {
            // alice approves super app as operator, alice creates a flow to bob, alice creates a
            // flow to super app which deletes the flow from alice to bob on alice's behalf.
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .authorizeFlowOperatorWithFullControl(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(superToken.address, bob, flowRate, "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        flowRate,
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

        it("2.10 - Can Update Flow Operator Permissions in Callback", async () => {
            // alice creates a flow to the super app which sets alice as the operator for it
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        flowRate,
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
                        cfaLibrarySuperAppMock.address,
                        alice
                    )
                ).permissions.toString(),
                "7"
            );
        });

        it("2.11 - Can Authorize Flow Operator With Full Control in Callback", async () => {
            // alice creates a flow to the super app which sets alice as the operator for it
            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        flowRate,
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
                        cfaLibrarySuperAppMock.address,
                        alice
                    )
                ).permissions.toString(),
                "7"
            );
        });

        it("2.12 - Can Revoke Flow Operator With Full Control in Callback", async () => {
            // alice sets theirself as the operator for the super app, alice creates a flow to
            // the super app which revokes alice's operator permissions
            await cfaLibrarySuperAppMock.authorizeFlowOperatorWithFullControl(
                superToken.address,
                {from: alice}
            );

            await host.callAgreement(
                cfa.address,
                cfa.contract.methods
                    .createFlow(
                        superToken.address,
                        cfaLibrarySuperAppMock.address,
                        flowRate,
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
                        cfaLibrarySuperAppMock.address,
                        alice
                    )
                ).permissions.toString(),
                "0"
            );
        });
    });
});
