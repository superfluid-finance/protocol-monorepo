const { expectRevert } = require("@openzeppelin/test-helpers");
const { wad4human, toWad } = require("@decentral.ee/web3-helpers");
const {
    shouldCreateIndex,
    shouldDistribute,
    shouldApproveSubscription,
    shouldUpdateSubscription,
    shouldDeleteSubscription,
    shouldClaimPendingDistribution,
} = require("./InstantDistributionAgreementV1.behaviour.js");

const TestEnvironment = require("../../TestEnvironment");

const DEFAULT_INDEX_ID = 42;

contract.only("Using InstanceDistributionAgreement v1", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 5), {
        isTruffle: true,
        useMocks: true,
    });
    const { alice, bob, carol, dan } = t.aliases;
    const { INIT_BALANCE } = t.configs;

    let superToken;

    before(async () => {
        await t.reset();
    });

    beforeEach(async function () {
        await t.createNewToken({ doUpgrade: false });
        ({ superToken } = t.contracts);
    });

    async function testExpectedBalances(expectedBalances) {
        for (let i = 0; i < expectedBalances.length; ++i) {
            const account = expectedBalances[i][0];
            const expectedBalance = expectedBalances[i][1];
            //const expectedDeposit = expectedBalances[i][2] || "0";
            const balance = await superToken.balanceOf.call(account);
            console.log(
                `${t.toAlias(account)}'s current balance: `,
                wad4human(balance)
            );
            assert.equal(balance.toString(), expectedBalance.toString());
        }
    }

    async function verifyAll() {
        await t.validateSystemInvariance();
    }

    context("#1 without callbacks", () => {
        describe("#1.1 index operations", async () => {
            it("#1.1.1 publisher can create a new index", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await verifyAll();
            });

            it("#1.1.2 publisher should fail to create the same index", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await expectRevert(
                    shouldCreateIndex({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                    }),
                    "IDA: E_INDEX_EXISTS"
                );
            });

            it("#1.1.3 publisher should fail to query non-existant index", async () => {
                const idata = await t.sf.ida.getIndex({
                    superToken: superToken.address,
                    publisher: alice,
                    indexId: DEFAULT_INDEX_ID,
                });
                assert.isFalse(idata.exist);
            });

            it("#1.1.4 publisher can update update the index", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "1984",
                });

                await verifyAll();
            });

            it("#1.1.5 publisher should fail to update non-existent index", async () => {
                await expectRevert(
                    shouldDistribute({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        indexValue: "1984",
                    }),
                    "IDA: E_NO_INDEX"
                );
            });

            it("#1.1.6 publisher should fail to update index with smaller value", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "1984",
                });

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "1984",
                });

                await expectRevert(
                    shouldDistribute({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        indexValue: "1983",
                    }),
                    "IDA: E_INDEX_GROW"
                );
            });
        });

        describe("#1.2 subscription operations", async () => {
            it("#1.2.1 subscriber can approve a subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
            });

            it("#1.2.2 subscriber should fail to approve a subscription twice", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                await expectRevert(
                    shouldApproveSubscription({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                    }),
                    "IDA: E_SUBS_APPROVED"
                );
            });

            it("#1.2.3 subscriber can delete its approved subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });

                await shouldDeleteSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await verifyAll();
            });

            it("#1.2.4 subscriber can delete its pending subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });

                await shouldDeleteSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await verifyAll();
            });

            it("#1.2.5 publisher can delete a subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });

                await shouldDeleteSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "alice",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await verifyAll();
            });

            it("#1.2.6 subscriber should fail to delete a non-existen subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await expectRevert(
                    shouldDeleteSubscription({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "bob",
                    }),
                    "IDA: E_NO_SUBS"
                );
            });

            it("#1.2.7 subscriber should fail to delete a subscription of a non-existent index", async () => {
                await expectRevert(
                    shouldDeleteSubscription({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "bob",
                    }),
                    "IDA: E_NO_INDEX"
                );
            });

            it("#1.2.8 one should fail to delete other's subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                await expectRevert(
                    shouldDeleteSubscription({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "dan",
                    }),
                    "IDA: E_NOT_ALLOWED"
                );
            });

            it("#1.2.9 subscriber can delete then resubscribe to subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);

                await shouldDeleteSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);
            });
        });

        describe("#1.3 distribution operations", () => {
            it("#1.3.1 approveSubscription -> updateSubscription -> updateIndex", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });

                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);
                assert.equal(subs.publishers[0], alice);
                assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
                assert.equal(subs.unitsList[0], "0");

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);
                assert.equal(subs.publishers[0], alice);
                assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
                assert.equal(subs.unitsList[0], toWad("0.001").toString());

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    indexValue: "100",
                });

                await verifyAll();
            });

            it("#1.3.2 updateSubscription -> updateIndex -> approveSubscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.003").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await verifyAll();

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);
                assert.equal(subs.publishers[0], alice);
                assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
                assert.equal(
                    subs.unitsList[0].toString(),
                    toWad("0.003").toString()
                );

                await verifyAll();
            });

            it("#1.3.3 updateSubscription -> approveSubscription -> updateIndex", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);
                assert.equal(subs.publishers[0], alice);
                assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
                assert.equal(
                    subs.unitsList[0].toString(),
                    toWad("0.001").toString()
                );

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);
                assert.equal(subs.publishers[0], alice);
                assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
                assert.equal(
                    subs.unitsList[0].toString(),
                    toWad("0.001").toString()
                );

                await verifyAll();
            });

            it("#1.3.4 2x(updateSubscription -> shouldDistribute) ->  approveSubscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.003").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.005").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 1);
                assert.equal(subs.publishers[0], alice);
                assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
                assert.equal(
                    subs.unitsList[0].toString(),
                    toWad("0.005").toString()
                );

                await verifyAll();
            });
        });

        describe("#1.4 claim operation", () => {
            it("#1.4.1 subscriber can claim distribution from its pending subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.003").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.publishers.length, 0);

                await shouldClaimPendingDistribution({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "bob",
                });

                // claim by third party
                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });
                await shouldClaimPendingDistribution({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "dan",
                });

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "300",
                });

                await verifyAll();
            });

            it("#1.4.2 anyone can claim distribution on behalf of other", async () => {
                await t.upgradeBalance("alice", INIT_BALANCE);
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.003").toString(),
                });
                await shouldDistribute({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                await shouldClaimPendingDistribution({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "dan",
                });
            });

            it("#1.4.3 one should not claim from a non-existent subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await expectRevert(
                    shouldClaimPendingDistribution({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "bob",
                    }),
                    "IDA: E_NO_SUBS"
                );
            });

            it("#1.4.4 one should not claim from a subscription of a non-existent index", async () => {
                await expectRevert(
                    shouldClaimPendingDistribution({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "bob",
                    }),
                    "IDA: E_NO_INDEX"
                );
            });
        });
    });

    context("#3 misc", async () => {
        it("#4.1 only authorized host can access token", async () => {
            const FakeSuperfluidMock = artifacts.require("FakeSuperfluidMock");
            const fakeHost = await FakeSuperfluidMock.new();
            const ida = t.sf.agreements.ida;
            await expectRevert(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .createIndex(superToken.address, 42, "0x")
                        .encodeABI(),
                    { from: alice }
                ),
                "AgreementLibrary: unauthroized host"
            );
            await expectRevert(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .updateIndex(superToken.address, 42, 9000, "0x")
                        .encodeABI(),
                    { from: alice }
                ),
                "AgreementLibrary: unauthroized host"
            );
            await expectRevert(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .distribute(superToken.address, 42, 9000, "0x")
                        .encodeABI(),
                    { from: alice }
                ),
                "AgreementLibrary: unauthroized host"
            );
            await expectRevert(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .approveSubscription(superToken.address, bob, 42, "0x")
                        .encodeABI(),
                    { from: alice }
                ),
                "AgreementLibrary: unauthroized host"
            );
            await expectRevert(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .updateSubscription(
                            superToken.address,
                            42,
                            alice,
                            1000,
                            "0x"
                        )
                        .encodeABI(),
                    { from: alice }
                ),
                "AgreementLibrary: unauthroized host"
            );
            await expectRevert(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .deleteSubscription(
                            superToken.address,
                            bob,
                            42,
                            alice,
                            "0x"
                        )
                        .encodeABI(),
                    { from: alice }
                ),
                "AgreementLibrary: unauthroized host"
            );
        });
    });

    context.only("#10 scenarios", async () => {
        it("#10.1 1to3 distribution scenario", async () => {
            await t.upgradeBalance("alice", INIT_BALANCE);

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            const subscribers = [
                [bob, toWad("0.0001"), true],
                [carol, toWad("0.0002"), true],
                [dan, toWad("0.0003"), false],
            ];
            for (let i = 0; i < subscribers.length; ++i) {
                const subscriberAddr = subscribers[i][0];
                const subscriptionUnits = subscribers[i][1];
                const doApprove = subscribers[i][2];
                const subscriberName = t.toAlias(subscriberAddr);

                if (doApprove) {
                    await shouldApproveSubscription({
                        testenv: t,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: subscriberName,
                    });
                }

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: subscriberName,
                    units: subscriptionUnits.toString(),
                });

                const subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: subscriberAddr,
                });
                if (doApprove) {
                    assert.equal(subs.publishers.length, 1);
                    assert.equal(subs.publishers[0], alice);
                    assert.equal(
                        subs.indexIds[0].toString(),
                        DEFAULT_INDEX_ID.toString()
                    );
                    assert.equal(
                        subs.unitsList[0].toString(),
                        subscriptionUnits.toString()
                    );
                } else {
                    assert.equal(subs.publishers.length, 0);
                }
            }

            await shouldDistribute({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "100",
            });
            await testExpectedBalances([
                [alice, toWad("99.94")],
                [bob, toWad("0.01")],
                [carol, toWad("0.02")],
                [dan, toWad("0.00")],
            ]);

            await shouldDistribute({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "300",
            });
            await testExpectedBalances([
                [alice, toWad("99.82")],
                [bob, toWad("0.03")],
                [carol, toWad("0.06")],
                [dan, toWad("0.00")],
            ]);

            await shouldDeleteSubscription({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "dan",
                senderName: "dan",
            });
            await testExpectedBalances([
                [alice, toWad("99.82")],
                [bob, toWad("0.03")],
                [carol, toWad("0.06")],
                [dan, toWad("0.09")],
            ]);

            await shouldDistribute({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "400",
            });
            await testExpectedBalances([
                [alice, toWad("99.79")],
                [bob, toWad("0.04")],
                [carol, toWad("0.08")],
                [dan, toWad("0.09")],
            ]);

            await verifyAll();
        });

        it("#10.2 2to1 distribution scenario", async () => {
            await t.upgradeBalance("alice", INIT_BALANCE);
            await t.upgradeBalance("bob", INIT_BALANCE);

            // alice and bob create indeces and dan subscribes to them
            const publishers = [
                [alice, toWad("0.0001"), true],
                [bob, toWad("0.0002"), false],
            ];
            for (let i = 0; i < publishers.length; ++i) {
                let publisherAddr = publishers[i][0];
                let subscriptionUnits = publishers[i][1];
                let doApprove = publishers[i][2];
                const publisherName = t.toAlias(publisherAddr);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName,
                    indexId: DEFAULT_INDEX_ID,
                });

                if (doApprove) {
                    await shouldApproveSubscription({
                        testenv: t,
                        publisherName,
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "dan",
                    });
                }

                await shouldUpdateSubscription({
                    testenv: t,
                    publisherName,
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "dan",
                    units: subscriptionUnits.toString(),
                });
            }

            const subs = await t.sf.ida.listSubscriptions({
                superToken: superToken.address,
                subscriber: dan,
            });
            assert.equal(subs.publishers.length, 1);
            assert.equal(subs.publishers[0], alice);
            assert.equal(
                subs.indexIds[0].toString(),
                DEFAULT_INDEX_ID.toString()
            );
            assert.equal(wad4human(subs.unitsList[0]), "0.00010");

            // Alice distributes tokens (100 * 0.0001 = 0.01)
            await shouldDistribute({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "100",
            });
            await testExpectedBalances([
                [alice, toWad("99.99")],
                [bob, toWad("100.00")],
                [dan, toWad("0.01")],
            ]);

            // Bob distributes tokens (200 * 0.0002 = 0.04)
            await shouldDistribute({
                testenv: t,
                publisherName: "bob",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "200",
            });
            await testExpectedBalances([
                [alice, toWad("99.99")],
                [bob, toWad("99.96")],
                [dan, toWad("0.01")],
            ]);

            // Alice update Dan's subscription with more units
            await shouldUpdateSubscription({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "dan",
                units: toWad("0.0003").toString(),
            });

            // Alice distributes tokens again (100 * 0.0003 = 0.03)
            await shouldDistribute({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "200",
            });
            await testExpectedBalances([
                [alice, toWad("99.96")],
                [bob, toWad("99.96")],
                [dan, toWad("0.04")],
            ]);

            await shouldApproveSubscription({
                testenv: t,
                publisherName: "bob",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "dan",
            });
            await testExpectedBalances([
                [alice, toWad("99.96")],
                [bob, toWad("99.96")],
                [dan, toWad("0.08")],
            ]);

            await verifyAll();
        });
    });
});
