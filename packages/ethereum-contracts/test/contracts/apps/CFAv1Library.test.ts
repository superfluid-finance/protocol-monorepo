import {SignerWithAddress} from "@nomiclabs/hardhat-ethers/signers";
import {assert, ethers, web3} from "hardhat";

import {
    CFALibraryMock,
    CFALibrarySuperAppMock,
    ConstantFlowAgreementV1,
    SuperfluidMock,
    SuperTokenMock,
} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";

import {deploySuperTokenAndNFTContractsAndInitialize} from "./SuperTokenV1Library.CFA.test";

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

    let superToken: SuperTokenMock,
        host: SuperfluidMock,
        cfa: ConstantFlowAgreementV1;
    let alice: string, bob: string;
    let aliceSigner: SignerWithAddress;
    let cfaLibraryMock: CFALibraryMock;
    let cfaLibrarySuperAppMock: CFALibrarySuperAppMock;

    // the calldata used for a lot of the tests are
    // repeated, it makes sense to collapse them here
    let createFlowCalldata: string;
    let authorizeFullControlCalldata: string;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 3,
        });

        cfa = t.contracts.cfa;
        host = t.contracts.superfluid;

        ({alice, bob} = t.aliases);
        aliceSigner = await ethers.getSigner(alice);
    });

    beforeEach(async function () {
        superToken = await deploySuperTokenAndNFTContractsAndInitialize(t);
        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken.mintInternal(bob, mintAmount, "0x", "0x");

        // deploy a contract we'll use for testing the library
        const CFALibraryMockFactory =
            await ethers.getContractFactory("CFALibraryMock");
        cfaLibraryMock = await CFALibraryMockFactory.deploy(host.address);

        const CFALibrarySuperAppMockFactory = await ethers.getContractFactory(
            "CFALibrarySuperAppMock"
        );
        cfaLibrarySuperAppMock = await CFALibrarySuperAppMockFactory.deploy(
            host.address,
            alice, // sender
            bob, // receiver
            alice // operator
        );

        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken
            .connect(aliceSigner)
            .transfer(cfaLibraryMock.address, mintAmount);

        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken
            .connect(aliceSigner)
            .transfer(cfaLibrarySuperAppMock.address, mintAmount);

        createFlowCalldata = t.agreementHelper.cfaInterface.encodeFunctionData(
            "createFlow",
            [superToken.address, cfaLibrarySuperAppMock.address, flowRate, "0x"]
        );
        authorizeFullControlCalldata =
            t.agreementHelper.cfaInterface.encodeFunctionData(
                "authorizeFlowOperatorWithFullControl",
                [superToken.address, cfaLibraryMock.address, "0x"]
            );
        t.beforeEachTestCaseBenchmark(this);
    });

    afterEach(() => {
        t.afterEachTestCaseBenchmark();
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
            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    t.agreementHelper.cfaInterface.encodeFunctionData(
                        "createFlow",
                        [
                            superToken.address,
                            cfaLibrarySuperAppMock.address,
                            flowRate,
                            "0x",
                        ]
                    ),
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.CREATE_FLOW
                    )
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

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    createFlowCalldata,
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.UPDATE_FLOW
                    )
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

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    createFlowCalldata,
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.DELETE_FLOW
                    )
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
            await host
                .connect(aliceSigner)
                .callAgreement(cfa.address, authorizeFullControlCalldata, "0x");

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
            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    t.agreementHelper.cfaInterface.encodeFunctionData(
                        "authorizeFlowOperatorWithFullControl",
                        [superToken.address, cfaLibraryMock.address, "0x"]
                    ),
                    "0x"
                );

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    t.agreementHelper.cfaInterface.encodeFunctionData(
                        "createFlow",
                        [superToken.address, bob, flowRate, "0x"]
                    ),
                    "0x"
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
            await host
                .connect(aliceSigner)
                .callAgreement(cfa.address, authorizeFullControlCalldata, "0x");

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    t.agreementHelper.cfaInterface.encodeFunctionData(
                        "createFlow",
                        [superToken.address, bob, flowRate, "0x"]
                    ),
                    "0x"
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
            authorizeFullControlCalldata =
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "authorizeFlowOperatorWithFullControl",
                    [superToken.address, cfaLibrarySuperAppMock.address, "0x"]
                );
            await host
                .connect(aliceSigner)
                .callAgreement(cfa.address, authorizeFullControlCalldata, "0x");

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    createFlowCalldata,
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.CREATE_FLOW_BY_OPERATOR
                    )
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
            authorizeFullControlCalldata =
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "authorizeFlowOperatorWithFullControl",
                    [superToken.address, cfaLibrarySuperAppMock.address, "0x"]
                );
            await host
                .connect(aliceSigner)
                .callAgreement(cfa.address, authorizeFullControlCalldata, "0x");

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    t.agreementHelper.cfaInterface.encodeFunctionData(
                        "createFlow",
                        [superToken.address, bob, flowRate, "0x"]
                    ),
                    "0x"
                );

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    createFlowCalldata,
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.UPDATE_FLOW_BY_OPERATOR
                    )
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
            authorizeFullControlCalldata =
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "authorizeFlowOperatorWithFullControl",
                    [superToken.address, cfaLibrarySuperAppMock.address, "0x"]
                );
            await host
                .connect(aliceSigner)
                .callAgreement(cfa.address, authorizeFullControlCalldata, "0x");

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    t.agreementHelper.cfaInterface.encodeFunctionData(
                        "createFlow",
                        [superToken.address, bob, flowRate, "0x"]
                    ),
                    "0x"
                );

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    createFlowCalldata,
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.DELETE_FLOW_BY_OPERATOR
                    )
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
            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    createFlowCalldata,
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.UPDATE_FLOW_OPERATOR_PERMISSIONS
                    )
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
            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    createFlowCalldata,
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL
                    )
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
            await cfaLibrarySuperAppMock
                .connect(aliceSigner)
                .authorizeFlowOperatorWithFullControl(superToken.address);

            await host
                .connect(aliceSigner)
                .callAgreement(
                    cfa.address,
                    createFlowCalldata,
                    web3.eth.abi.encodeParameter(
                        "uint8",
                        callbackFunctionIndex.REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL
                    )
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
