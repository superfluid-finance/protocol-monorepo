import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { expect } from "chai";
import {
    AUTHORIZE_FLOW_OPERATOR_CREATE,
    AUTHORIZE_FULL_CONTROL,
    _addPermissions,
    _removePermissions,
    clipDepositNumber,
    getFlowOperatorId,
    getPerSecondFlowRateByMonth,
    toBN,
} from "../src";
import {
    makeSuite,
    TestEnvironment,
    validateOperationShouldUseCallAgreement,
} from "./TestEnvironment";

makeSuite("SuperToken-CFA-Operator Tests", (testEnv: TestEnvironment) => {
    describe("Revert cases", () => {
        let sender: SignerWithAddress;
        let flowOperator: SignerWithAddress;
        before(() => {
            sender = testEnv.bob;
            flowOperator = testEnv.charlie;
        });

        it("Should throw when passing in unclean permissions", async () => {
            const flowRateAllowance = getPerSecondFlowRateByMonth("100");
            try {
                const permissions = AUTHORIZE_FULL_CONTROL + 1;
                const operation =
                    testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                        flowRateAllowance,
                        flowOperator: flowOperator.address,
                        permissions,
                    });
                await operation.exec(sender);
            } catch (err: any) {
                expect(err.message).to.eql(
                    "Unclean Permissions Error: The desired permissions are unclean"
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should throw when attempting to update flow operator permissions with negative flow rate", async () => {
            const flowRateAllowance = "-1000";
            try {
                const permissions = AUTHORIZE_FULL_CONTROL;
                const operation =
                    testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                        flowRateAllowance,
                        flowOperator: flowOperator.address,
                        permissions,
                    });
                await operation.exec(sender);
            } catch (err: any) {
                expect(err.message).to.eql(
                    "Negative Flow Rate Allowance Error: No negative flow allowance allowed"
                );
                expect(err.cause).to.be.undefined;
            }
        });
    });

    describe("Happy Path Tests", () => {
        let sender: SignerWithAddress;
        let flowOperator: SignerWithAddress;
        let receiver: SignerWithAddress;

        before(() => {
            sender = testEnv.bob;
            flowOperator = testEnv.charlie;
            receiver = testEnv.users[5];
        });

        it("Should be able to increase flow rate allowance", async () => {
            const flowOperatorDataBefore =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            const flowRateAllowanceDelta = getPerSecondFlowRateByMonth("100");
            await testEnv.wrapperSuperToken
                .increaseFlowRateAllowance({
                    flowRateAllowanceDelta,
                    flowOperator: flowOperator.address,
                })
                .exec(sender);
            const flowOperatorDataAfter =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            expect(flowOperatorDataAfter.flowRateAllowance).to.equal(
                toBN(flowOperatorDataBefore.flowRateAllowance)
                    .add(toBN(flowRateAllowanceDelta))
                    .toString()
            );
        });

        it("Should be able to decrease flow rate allowance", async () => {
            const flowRateAllowanceDelta = getPerSecondFlowRateByMonth("100");
            await testEnv.wrapperSuperToken
                .increaseFlowRateAllowance({
                    flowRateAllowanceDelta,
                    flowOperator: flowOperator.address,
                })
                .exec(sender);
            const flowOperatorDataBefore =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            const decreaseFlowRateAllowanceDelta =
                getPerSecondFlowRateByMonth("31");
            await testEnv.wrapperSuperToken
                .decreaseFlowRateAllowance({
                    flowRateAllowanceDelta: decreaseFlowRateAllowanceDelta,
                    flowOperator: flowOperator.address,
                })
                .exec(sender);
            const flowOperatorDataAfter =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            expect(flowOperatorDataAfter.flowRateAllowance).to.equal(
                toBN(flowOperatorDataBefore.flowRateAllowance)
                    .sub(toBN(decreaseFlowRateAllowanceDelta))
                    .toString()
            );
        });

        it("Should be able to increase flow rate allowance with permissions", async () => {
            const flowRateAllowanceDelta = getPerSecondFlowRateByMonth("100");
            const permissions = AUTHORIZE_FULL_CONTROL;

            const flowOperatorDataBefore =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            await testEnv.wrapperSuperToken
                .increaseFlowRateAllowanceWithPermissions({
                    flowRateAllowanceDelta,
                    flowOperator: flowOperator.address,
                    permissionsDelta: permissions,
                })
                .exec(sender);
            const flowOperatorDataAfter =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            expect(flowOperatorDataAfter.flowRateAllowance).to.equal(
                flowRateAllowanceDelta
            );
            expect(flowOperatorDataAfter.permissions).to.equal(
                _addPermissions(
                    Number(flowOperatorDataBefore.permissions),
                    permissions
                ).toString()
            );
        });

        it("Should be able to decrease flow rate allowance with permissions", async () => {
            const flowRateAllowanceDelta = getPerSecondFlowRateByMonth("100");
            const permissions = AUTHORIZE_FULL_CONTROL;
            await testEnv.wrapperSuperToken
                .increaseFlowRateAllowanceWithPermissions({
                    flowRateAllowanceDelta,
                    flowOperator: flowOperator.address,
                    permissionsDelta: permissions,
                })
                .exec(sender);
            const flowOperatorDataBefore =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            const decreaseFlowRateAllowanceDelta =
                getPerSecondFlowRateByMonth("31");
            await testEnv.wrapperSuperToken
                .decreaseFlowRateAllowanceWithPermissions({
                    flowRateAllowanceDelta: decreaseFlowRateAllowanceDelta,
                    flowOperator: flowOperator.address,
                    permissionsDelta: permissions,
                })
                .exec(sender);
            const flowOperatorDataAfter =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            expect(flowOperatorDataAfter.flowRateAllowance).to.equal(
                toBN(flowOperatorDataBefore.flowRateAllowance)
                    .sub(toBN(decreaseFlowRateAllowanceDelta))
                    .toString()
            );
            expect(flowOperatorDataAfter.permissions).to.equal(
                _removePermissions(
                    Number(flowOperatorDataBefore.permissions),
                    permissions
                ).toString()
            );
        });

        context(
            "Should be able to update flow operator permissions",
            async () => {
                async function shouldUpdateFlowOperatorPermissions(
                    shouldUseCallAgreement: boolean
                ) {
                    const flowRateAllowance =
                        getPerSecondFlowRateByMonth("100");
                    let permissions = AUTHORIZE_FLOW_OPERATOR_CREATE; // ALLOW_CREATE

                    const flowOperatorId = getFlowOperatorId(
                        sender.address,
                        flowOperator.address
                    );

                    // Update Flow Operator Permissions
                    const updateFlowOperatorPermissionsOperation =
                        testEnv.wrapperSuperToken.updateFlowOperatorPermissions(
                            {
                                sender: sender.address,
                                flowRateAllowance,
                                flowOperator: flowOperator.address,
                                permissions,
                                shouldUseCallAgreement,
                            }
                        );
                    validateOperationShouldUseCallAgreement(
                        testEnv,
                        updateFlowOperatorPermissionsOperation,
                        shouldUseCallAgreement,
                        testEnv.sdkFramework.cfaV1.forwarder.address
                    );
                    await expect(
                        updateFlowOperatorPermissionsOperation.exec(sender)
                    )
                        .to.emit(testEnv.cfaV1, "FlowOperatorUpdated")
                        .withArgs(
                            testEnv.wrapperSuperToken.address,
                            sender.address,
                            flowOperator.address,
                            permissions,
                            Number(flowRateAllowance)
                        );

                    // getFlowOperatorData test
                    let flowOperatorData =
                        await testEnv.wrapperSuperToken.getFlowOperatorData({
                            sender: sender.address,
                            flowOperator: flowOperator.address,
                            providerOrSigner: sender,
                        });
                    expect(flowOperatorData.flowOperatorId).equals(
                        flowOperatorId
                    );
                    expect(flowOperatorData.flowRateAllowance).equals(
                        flowRateAllowance
                    );
                    expect(flowOperatorData.permissions).equals(
                        permissions.toString()
                    );

                    // Revoke Flow Operator With Full Control Permissions
                    permissions = 0; // no permissions
                    const revokeFlowOperatorWithFullControlOperation =
                        testEnv.wrapperSuperToken.revokeFlowOperatorWithFullControl(
                            {
                                flowOperator: flowOperator.address,
                                shouldUseCallAgreement,
                            }
                        );
                    validateOperationShouldUseCallAgreement(
                        testEnv,
                        revokeFlowOperatorWithFullControlOperation,
                        shouldUseCallAgreement,
                        testEnv.sdkFramework.cfaV1.forwarder.address
                    );
                    await expect(
                        revokeFlowOperatorWithFullControlOperation.exec(sender)
                    )
                        .to.emit(testEnv.cfaV1, "FlowOperatorUpdated")
                        .withArgs(
                            testEnv.wrapperSuperToken.address,
                            sender.address,
                            flowOperator.address,
                            permissions,
                            0
                        );
                    // getFlowOperatorDataByID test
                    flowOperatorData =
                        await testEnv.wrapperSuperToken.getFlowOperatorDataByID(
                            {
                                flowOperatorId,
                                providerOrSigner: sender,
                            }
                        );
                    expect(flowOperatorData.flowOperatorId).equals(
                        flowOperatorId
                    );
                    expect(flowOperatorData.flowRateAllowance).equals("0");
                    expect(flowOperatorData.permissions).equals(
                        permissions.toString()
                    );

                    // Authorize Flow Operator With Full Control
                    permissions = AUTHORIZE_FULL_CONTROL; // all permissions
                    const authorizeFlowOperatorWithFullControlOperation =
                        testEnv.wrapperSuperToken.authorizeFlowOperatorWithFullControl(
                            {
                                flowOperator: flowOperator.address,
                                shouldUseCallAgreement,
                            }
                        );
                    validateOperationShouldUseCallAgreement(
                        testEnv,
                        authorizeFlowOperatorWithFullControlOperation,
                        shouldUseCallAgreement,
                        testEnv.sdkFramework.cfaV1.forwarder.address
                    );
                    await expect(
                        authorizeFlowOperatorWithFullControlOperation.exec(
                            sender
                        )
                    )
                        .to.emit(testEnv.cfaV1, "FlowOperatorUpdated")
                        .withArgs(
                            testEnv.wrapperSuperToken.address,
                            sender.address,
                            flowOperator.address,
                            permissions,
                            testEnv.constants.MAX_FLOW_RATE.toString() // max flow rate ((2 ** 95) - 1)
                        );

                    // getFlowOperatorDataByID test
                    flowOperatorData =
                        await testEnv.wrapperSuperToken.getFlowOperatorDataByID(
                            {
                                flowOperatorId,
                                providerOrSigner: sender,
                            }
                        );
                    expect(flowOperatorData.flowOperatorId).equals(
                        flowOperatorId
                    );
                    expect(flowOperatorData.flowRateAllowance).equals(
                        testEnv.constants.MAX_FLOW_RATE.toString()
                    );
                    expect(flowOperatorData.permissions).equals(
                        permissions.toString()
                    );
                }

                it("With Call Agreement", async () => {
                    await shouldUpdateFlowOperatorPermissions(true);
                });

                it("With Forwarder", async () => {
                    await shouldUpdateFlowOperatorPermissions(false);
                });
            }
        );

        context("Should be able to create flow by operator", async () => {
            async function shouldCreateFlowByOperator(
                shouldUseCallAgreement: boolean
            ) {
                const flowRateAllowance = getPerSecondFlowRateByMonth("100");
                let permissions = AUTHORIZE_FLOW_OPERATOR_CREATE; // ALLOW_CREATE

                const updateFlowOperatorPermissionsOperation =
                    testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                        sender: sender.address,
                        flowRateAllowance,
                        flowOperator: flowOperator.address,
                        permissions,
                        shouldUseCallAgreement,
                    });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    updateFlowOperatorPermissionsOperation,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await updateFlowOperatorPermissionsOperation.exec(sender);

                const deposit = clipDepositNumber(
                    toBN(flowRateAllowance).mul(
                        testEnv.constants.LIQUIDATION_PERIOD
                    )
                );
                const createFlowByOperatorOp =
                    testEnv.wrapperSuperToken.createFlowByOperator({
                        flowRate: flowRateAllowance,
                        sender: sender.address,
                        receiver: receiver.address,
                        shouldUseCallAgreement,
                    });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    createFlowByOperatorOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await expect(createFlowByOperatorOp.exec(flowOperator))
                    .to.emit(testEnv.cfaV1, "FlowUpdatedExtension")
                    .withArgs(flowOperator.address, deposit.toString());
            }

            it("With Call Agreement", async () => {
                await shouldCreateFlowByOperator(true);
            });

            it("With Forwarder", async () => {
                await shouldCreateFlowByOperator(false);
            });
        });

        context("Should be able to update flow by operator", async () => {
            async function shouldUpdateFlowByOperator(
                shouldUseCallAgreement: boolean
            ) {
                const initialFlowRate = getPerSecondFlowRateByMonth("100");

                // grant permissions
                const updateFlowOperatorPermissionsOperation =
                    testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                        sender: sender.address,
                        flowRateAllowance: initialFlowRate,
                        flowOperator: flowOperator.address,
                        permissions: AUTHORIZE_FULL_CONTROL,
                    });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    updateFlowOperatorPermissionsOperation,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await updateFlowOperatorPermissionsOperation.exec(sender);

                // create flow by operator
                const createFlowByOperatorOp =
                    testEnv.wrapperSuperToken.createFlowByOperator({
                        flowRate: initialFlowRate,
                        sender: sender.address,
                        receiver: receiver.address,
                        shouldUseCallAgreement,
                    });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    createFlowByOperatorOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await createFlowByOperatorOp.exec(flowOperator);
                const flowRate = getPerSecondFlowRateByMonth("70");

                const deposit = clipDepositNumber(
                    toBN(flowRate).mul(testEnv.constants.LIQUIDATION_PERIOD)
                );
                const updateFlowByOperatorOp =
                    testEnv.wrapperSuperToken.updateFlowByOperator({
                        flowRate,
                        sender: sender.address,
                        receiver: receiver.address,
                    });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    updateFlowByOperatorOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                // update flow by operator
                await expect(updateFlowByOperatorOp.exec(flowOperator))
                    .to.emit(testEnv.cfaV1, "FlowUpdatedExtension")
                    .withArgs(flowOperator.address, deposit.toString());
            }

            it("With Call Agreement", async () => {
                await shouldUpdateFlowByOperator(true);
            });

            it("With Forwarder", async () => {
                await shouldUpdateFlowByOperator(false);
            });
        });

        context("Should be able to delete flow by operator", async () => {
            async function shouldDeleteFlowByOperator(
                shouldUseCallAgreement: boolean
            ) {
                const initialFlowRate = getPerSecondFlowRateByMonth("100");
                // grant permissions
                const updateFlowOperatorPermissionsOperation =
                    testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                        sender: sender.address,
                        flowRateAllowance: initialFlowRate,
                        flowOperator: flowOperator.address,
                        permissions: AUTHORIZE_FULL_CONTROL,
                        shouldUseCallAgreement,
                    });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    updateFlowOperatorPermissionsOperation,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await updateFlowOperatorPermissionsOperation.exec(sender);

                // create flow by operator
                const createFlowByOperatorOp =
                    testEnv.wrapperSuperToken.createFlowByOperator({
                        flowRate: initialFlowRate,
                        sender: sender.address,
                        receiver: receiver.address,
                        shouldUseCallAgreement,
                    });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    createFlowByOperatorOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await createFlowByOperatorOp.exec(flowOperator);

                // delete flow by operator
                const deleteFlowByOperatorOp =
                    testEnv.wrapperSuperToken.deleteFlowByOperator({
                        flowRate: initialFlowRate,
                        sender: sender.address,
                        receiver: receiver.address,
                        shouldUseCallAgreement,
                    });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    deleteFlowByOperatorOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await expect(deleteFlowByOperatorOp.exec(flowOperator))
                    .to.emit(testEnv.cfaV1, "FlowUpdatedExtension")
                    .withArgs(flowOperator.address, "0");
            }

            it("With Call Agreement", async () => {
                await shouldDeleteFlowByOperator(true);
            });

            it("With Forwarder", async () => {
                await shouldDeleteFlowByOperator(false);
            });
        });
    });
});
