import { expect } from "chai";
import { ethers } from "ethers";
import { makeSuite, TestEnvironment } from "./TestEnvironment";

makeSuite("SuperToken-IDA Tests", (testEnv: TestEnvironment) => {
    describe("Revert cases", () => {
        it("Should throw an error if one of the input addresses is invalid", async () => {
            try {
                testEnv.sdkFramework.idaV1.createIndex({
                    indexId: "0",
                    superToken: testEnv.wrapperSuperToken.address + "z",
                });
            } catch (err: any) {
                expect(err.message).to.eql(
                    "Invalid Address Error: The address you have entered is not a valid ethereum address"
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should throw an error on the reads as expected", async () => {
            // NOTE: using casting to pass in wrong input to force error
            try {
                await testEnv.sdkFramework.idaV1.getIndex({
                    superToken: testEnv.wrapperSuperToken.address,
                    publisher: testEnv.alice.address,
                    indexId: "0",
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "InstantDistributionAgreementV1 Read Error: There was an error getting the index"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }

            try {
                await testEnv.sdkFramework.idaV1.getSubscription({
                    superToken: testEnv.wrapperSuperToken.address,
                    publisher: testEnv.alice.address,
                    indexId: "0",
                    subscriber: testEnv.bob.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "InstantDistributionAgreementV1 Read Error: There was an error getting the subscription"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }
        });
    });

    describe("Happy Path Tests", () => {
        // IDA Functions
        it("Should be able to create an index and get the newly created index", async () => {
            await expect(
                testEnv.wrapperSuperToken
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexCreated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    "0x"
                );

            const index = await testEnv.wrapperSuperToken.getIndex({
                publisher: testEnv.bob.address,
                indexId: "0",
                providerOrSigner: testEnv.bob,
            });
            expect(index.exist).to.equal(true);
            expect(index.indexValue).to.equal("0");
            expect(index.totalUnitsApproved).to.equal("0");
            expect(index.totalUnitsPending).to.equal("0");
        });

        it("Should be able to update subscription units and get newly created subscriptions", async () => {
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            const units = ethers.utils.parseUnits("0.001").toString();
            await expect(
                testEnv.wrapperSuperToken
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: testEnv.alice.address,
                        units,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "SubscriptionUnitsUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    units,
                    "0x"
                );

            const deployerSubscription =
                await testEnv.wrapperSuperToken.getSubscription({
                    publisher: testEnv.bob.address,
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    providerOrSigner: testEnv.bob,
                });

            expect(deployerSubscription.exist).to.equal(true);
            expect(deployerSubscription.approved).to.equal(false);
            expect(deployerSubscription.units).to.equal(units);
            expect(deployerSubscription.pendingDistribution).to.equal("0");

            await expect(
                testEnv.wrapperSuperToken
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: testEnv.charlie.address,
                        units,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUnitsUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.charlie.address,
                    units,
                    "0x"
                );

            const bravoSubscription =
                await testEnv.wrapperSuperToken.getSubscription({
                    publisher: testEnv.bob.address,
                    indexId: "0",
                    subscriber: testEnv.charlie.address,
                    providerOrSigner: testEnv.charlie,
                });

            expect(bravoSubscription.exist).to.equal(true);
            expect(bravoSubscription.approved).to.equal(false);
            expect(bravoSubscription.units).to.equal(units);
            expect(bravoSubscription.pendingDistribution).to.equal("0");
        });

        it("Should be able to distribute (simple case)", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();

            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .distribute({
                        indexId: "0",
                        amount: ethers.utils.parseUnits("1").toString(),
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    "0",
                    "1000",
                    ethers.utils.parseUnits("0.001").toString(),
                    ethers.utils.parseUnits("0").toString(),
                    "0x"
                );
        });

        it("Should be able to distribute (multiple subs)", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();

            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.charlie.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .distribute({
                        indexId: "0",
                        amount: ethers.utils.parseUnits("1").toString(),
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    "0",
                    "500",
                    ethers.utils.parseUnits("0.002").toString(),
                    ethers.utils.parseUnits("0").toString(),
                    "0x"
                );
        });

        it("Should be able to approve subscription", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();

            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .approveSubscription({
                        indexId: "0",
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.idaV1, "SubscriptionApproved")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    "0x"
                );

            await expect(
                testEnv.wrapperSuperToken
                    .approveSubscription({
                        indexId: "0",
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.charlie)
            )
                .to.emit(testEnv.idaV1, "IndexSubscribed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.charlie.address,
                    "0x"
                );
        });

        it("Should be able to update index value", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();
            const updatedIndexValue = ethers.utils
                .parseUnits("0.000000002")
                .toString();
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .updateIndexValue({
                        indexId: "0",
                        indexValue: updatedIndexValue,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    "0",
                    updatedIndexValue,
                    units,
                    "0",
                    "0x"
                );
        });

        it("Should be able to revoke subscription", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.bob.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .approveSubscription({
                    indexId: "0",
                    publisher: testEnv.bob.address,
                })
                .exec(testEnv.alice);

            await testEnv.wrapperSuperToken
                .approveSubscription({
                    indexId: "0",
                    publisher: testEnv.bob.address,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .revokeSubscription({
                        indexId: "0",
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.idaV1, "SubscriptionRevoked")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    "0x"
                );

            await expect(
                testEnv.wrapperSuperToken
                    .revokeSubscription({
                        indexId: "0",
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUnsubscribed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.bob.address,
                    "0x"
                );
        });

        it("Should be able to claim pending units", async () => {
            const units = ethers.utils.parseUnits("0.002").toString();
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .distribute({
                    indexId: "0",
                    amount: units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .claim({
                        indexId: "0",
                        subscriber: testEnv.alice.address,
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.idaV1, "SubscriptionDistributionClaimed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    units
                )
                .and.to.emit(testEnv.idaV1, "IndexDistributionClaimed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.alice.address,
                    units
                );
        });

        it("Should be able to delete subscription", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.charlie.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .deleteSubscription({
                        indexId: "0",
                        subscriber: testEnv.alice.address,
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "SubscriptionRevoked")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    "0x"
                );

            await expect(
                testEnv.wrapperSuperToken
                    .deleteSubscription({
                        indexId: "0",
                        subscriber: testEnv.charlie.address,
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUnsubscribed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.charlie.address,
                    "0x"
                );
        });
    });
});
