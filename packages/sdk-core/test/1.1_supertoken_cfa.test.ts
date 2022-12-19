import { expect } from "chai";
import { getPerSecondFlowRateByMonth } from "../src";
import {
    validateOperationShouldUseCallAgreement,
    makeSuite,
    TestEnvironment,
} from "./TestEnvironment";

makeSuite("SuperToken-CFA Tests", (testEnv: TestEnvironment) => {
    describe("Revert cases", () => {
        it("Should throw an error if one of the input addresses is invalid", async () => {
            const flowRate = getPerSecondFlowRateByMonth("100");
            try {
                testEnv.wrapperSuperToken.createFlow({
                    flowRate,
                    receiver: testEnv.bob.address + "0",
                    sender: testEnv.alice.address,
                });
            } catch (err: any) {
                expect(err.message).to.eql(
                    "Invalid Address Error: The address you have entered is not a valid ethereum address"
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should throw an error on getFlow functions as expected", async () => {
            // NOTE: using casting to pass in wrong input to force error
            // get flow throw
            try {
                await testEnv.wrapperSuperToken.getFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "ConstantFlowAgreementV1 Read Error: There was an error getting the flow"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }

            // get account flow info throw
            try {
                await testEnv.wrapperSuperToken.getAccountFlowInfo({
                    account: testEnv.alice.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "ConstantFlowAgreementV1 Read Error: There was an error getting the account flow information"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }

            // get net flow throw
            try {
                await testEnv.wrapperSuperToken.getNetFlow({
                    account: testEnv.alice.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "ConstantFlowAgreementV1 Read Error: There was an error getting net flow"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }
        });

        it("Should throw an error on getFlowOperatorData functions as expected", async () => {
            // NOTE: using casting to pass in wrong input to force error
            // get flowOperatorData throw
            try {
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: testEnv.alice.address,
                    flowOperator: testEnv.bob.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "ConstantFlowAgreementV1 Read Error: There was an error getting flow operator data"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }
            try {
                await testEnv.wrapperSuperToken.getFlowOperatorDataByID({
                    flowOperatorId: "",
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "ConstantFlowAgreementV1 Read Error: There was an error getting flow operator data"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }
        });

        it("Should throw if delete flow by wrong person", async () => {
            let flowRate = getPerSecondFlowRateByMonth("1000");
            await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);
            try {
                await testEnv.wrapperSuperToken
                    .deleteFlow({
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                    })
                    .exec(testEnv.charlie);
            } catch (err: any) {
                expect(err.message).to.include("cannot estimate gas;");
            }
        });
    });

    describe("Happy Path Tests", () => {
        // CFA Functions
        it("Should have eip155 protection check", async () => {
            const flowRate = getPerSecondFlowRateByMonth("1000");
            const txnResponse = await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);
            expect(txnResponse.v).to.not.be.undefined;
        });

        context("Should be able to create flow", async () => {
            async function shouldCreateFlow(shouldUseCallAgreement: boolean) {
                const flowRate = getPerSecondFlowRateByMonth("1000");
                const baseParams = {
                    receiver: testEnv.bob.address,
                    flowRate,
                    shouldUseCallAgreement,
                };
                
                // should work without sender
                const params = shouldUseCallAgreement
                    ? baseParams
                    : { ...baseParams, sender: testEnv.alice.address };
                const createFlowOp =
                    testEnv.wrapperSuperToken.createFlow(params);
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    createFlowOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await expect(createFlowOp.exec(testEnv.alice))
                    .to.emit(testEnv.cfaV1, "FlowUpdated")
                    .withArgs(
                        testEnv.wrapperSuperToken.address,
                        testEnv.alice.address,
                        testEnv.bob.address,
                        Number(flowRate),
                        Number(flowRate) * -1,
                        Number(flowRate),
                        "0x"
                    );

                // get flow check
                const flow = await testEnv.wrapperSuperToken.getFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    providerOrSigner: testEnv.alice,
                });
                expect(flow.flowRate).to.equal(flowRate);

                // get account flow info check
                const deployerAccountFlowInfo =
                    await testEnv.wrapperSuperToken.getAccountFlowInfo({
                        account: testEnv.alice.address,
                        providerOrSigner: testEnv.alice,
                    });
                const alphaAccountFlowInfo =
                    await testEnv.wrapperSuperToken.getAccountFlowInfo({
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    });
                expect(Number(deployerAccountFlowInfo.flowRate)).to.equal(
                    Number(flowRate) * -1
                );
                expect(Number(alphaAccountFlowInfo.flowRate)).to.equal(
                    Number(flowRate)
                );

                // get net flow check
                const deployerNetFlow =
                    await testEnv.wrapperSuperToken.getNetFlow({
                        account: testEnv.alice.address,
                        providerOrSigner: testEnv.alice,
                    });
                const alphaNetFlow = await testEnv.wrapperSuperToken.getNetFlow(
                    {
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    }
                );
                expect(Number(deployerNetFlow)).to.equal(Number(flowRate) * -1);
                expect(Number(alphaNetFlow)).to.equal(Number(flowRate));
            }

            it("With Call Agreement", async () => {
                await shouldCreateFlow(true);
            });

            it("With Forwarder", async () => {
                await shouldCreateFlow(false);
            });
        });

        context("Should be able to update flow", async () => {
            async function shouldUpdateFlow(shouldUseCallAgreement: boolean) {
                let flowRate = getPerSecondFlowRateByMonth("1000");
                const baseParams = {
                    receiver: testEnv.bob.address,
                    flowRate,
                    shouldUseCallAgreement,
                };
                
                // should work without sender
                const params = shouldUseCallAgreement
                    ? baseParams
                    : { ...baseParams, sender: testEnv.alice.address };
                const createFlowOp = testEnv.wrapperSuperToken.createFlow(params);
                await createFlowOp.exec(testEnv.alice);
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    createFlowOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                flowRate = getPerSecondFlowRateByMonth("1200");

                const updateFlowOp = testEnv.wrapperSuperToken.updateFlow({...params, flowRate});
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    updateFlowOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await expect(updateFlowOp.exec(testEnv.alice))
                    .to.emit(testEnv.cfaV1, "FlowUpdated")
                    .withArgs(
                        testEnv.wrapperSuperToken.address,
                        testEnv.alice.address,
                        testEnv.bob.address,
                        Number(flowRate),
                        Number(flowRate) * -1,
                        Number(flowRate),
                        "0x"
                    );
            }

            it("With Call Agreement", async () => {
                await shouldUpdateFlow(true);
            });

            it("With Forwarder", async () => {
                await shouldUpdateFlow(false);
            });
        });

        context("Should be able to delete flow (by sender)", async () => {
            async function shouldDeleteFlowBySender(
                shouldUseCallAgreement: boolean
            ) {
                let flowRate = getPerSecondFlowRateByMonth("1000");
                const baseParams = {
                    receiver: testEnv.bob.address,
                    flowRate,
                    shouldUseCallAgreement,
                };
                
                // should work without sender
                const params = shouldUseCallAgreement
                    ? baseParams
                    : { ...baseParams, sender: testEnv.alice.address };
                const createFlowOp = testEnv.wrapperSuperToken.createFlow(params);
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    createFlowOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await createFlowOp.exec(testEnv.alice);

                const deleteFlowOp = testEnv.wrapperSuperToken.deleteFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    shouldUseCallAgreement,
                });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    deleteFlowOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await expect(deleteFlowOp.exec(testEnv.alice))
                    .to.emit(testEnv.cfaV1, "FlowUpdated")
                    .withArgs(
                        testEnv.wrapperSuperToken.address,
                        testEnv.alice.address,
                        testEnv.bob.address,
                        0,
                        0,
                        0,
                        "0x"
                    );
            }

            it("With Call Agreement", async () => {
                await shouldDeleteFlowBySender(true);
            });

            it("With Forwarder", async () => {
                await shouldDeleteFlowBySender(false);
            });
        });

        context("Should be able to delete flow (by receiver)", async () => {
            async function shouldDeleteFlowByReceiver(
                shouldUseCallAgreement: boolean
            ) {
                let flowRate = getPerSecondFlowRateByMonth("1000");
                const createFlowOp = testEnv.wrapperSuperToken.createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                    shouldUseCallAgreement,
                });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    createFlowOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await createFlowOp.exec(testEnv.alice);

                const deleteFlowOp = testEnv.wrapperSuperToken.deleteFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    shouldUseCallAgreement,
                });
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    deleteFlowOp,
                    shouldUseCallAgreement,
                    testEnv.sdkFramework.cfaV1.forwarder.address
                );
                await expect(deleteFlowOp.exec(testEnv.bob))
                    .to.emit(testEnv.cfaV1, "FlowUpdated")
                    .withArgs(
                        testEnv.wrapperSuperToken.address,
                        testEnv.alice.address,
                        testEnv.bob.address,
                        0,
                        0,
                        0,
                        "0x"
                    );
            }

            it("With Call Agreement", async () => {
                await shouldDeleteFlowByReceiver(true);
            });

            it("With Forwarder", async () => {
                await shouldDeleteFlowByReceiver(false);
            });
        });
    });
});
