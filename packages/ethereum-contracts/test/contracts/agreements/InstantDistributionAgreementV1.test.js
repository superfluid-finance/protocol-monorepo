const {expectEvent} = require("@openzeppelin/test-helpers");
const {web3tx, wad4human, toWad} = require("@decentral.ee/web3-helpers");

const {
    shouldCreateIndex,
    shouldDistribute,
    shouldApproveSubscription,
    shouldUpdateSubscription,
    shouldRevokeSubscription,
    shouldDeleteSubscription,
    shouldClaimPendingDistribution,
} = require("./InstantDistributionAgreementV1.behaviour.js");

const {expectRevertedWith} = require("../../utils/expectRevert");

const IDASuperAppTester = artifacts.require("IDASuperAppTester");
const TestEnvironment = require("../../TestEnvironment");
const {ZERO_ADDRESS} = require("@openzeppelin/test-helpers/src/constants");

const DEFAULT_INDEX_ID = "42";

describe("Using InstantDistributionAgreement v1", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const {INIT_BALANCE} = t.configs;

    let alice, bob, carol;
    let superToken;

    before(async function () {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({alice, bob, carol} = t.aliases);

        superToken = t.sf.tokens.TESTx;
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    async function testExpectedBalances(expectedBalances) {
        for (let i = 0; i < expectedBalances.length; ++i) {
            const account = expectedBalances[i][0];
            const expectedBalance = expectedBalances[i][1];
            //const expectedDeposit = expectedBalances[i][2] || "0";
            const balance = await superToken.balanceOf(account);
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
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await verifyAll();
            });

            it("#1.1.2 publisher should fail to create the same index", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await expectRevertedWith(
                    shouldCreateIndex({
                        testenv: t,
                        superToken,
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

            it("#1.1.4 publisher can update the index", async () => {
                await t.upgradeBalance("alice", INIT_BALANCE);
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });
                await testExpectedBalances([
                    [alice, toWad("99.80")],
                    [bob, toWad("0.00")],
                ]);

                await verifyAll();
            });

            it("#1.1.5 publisher should fail to update non-existent index", async () => {
                await expectRevertedWith(
                    t.sf.ida.updateIndex({
                        superToken: superToken.address,
                        publisher: alice,
                        indexId: DEFAULT_INDEX_ID,
                        indexValue: "42",
                    }),
                    "IDA: E_NO_INDEX"
                );
                await expectRevertedWith(
                    t.sf.ida.distribute({
                        superToken: superToken.address,
                        publisher: alice,
                        indexId: DEFAULT_INDEX_ID,
                        amount: "42",
                    }),
                    "IDA: E_NO_INDEX"
                );
                await expectRevertedWith(
                    t.sf.agreements.ida.calculateDistribution(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "42"
                    ),
                    "IDA: E_NO_INDEX"
                );
            });

            it("#1.1.6 publisher should fail to update index with smaller value", async () => {
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });
                await testExpectedBalances([
                    [alice, toWad("99.80")],
                    [bob, toWad("0.00")],
                ]);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });
                await testExpectedBalances([
                    [alice, toWad("99.80")],
                    [bob, toWad("0.00")],
                ]);

                await expectRevertedWith(
                    shouldDistribute({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        indexValue: "199",
                    }),
                    "IDA: E_INDEX_GROW"
                );
            });

            it("#1.1.7 publisher can distribute by specifiying amount", async () => {
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    amount: toWad(1).toString(),
                });
                await testExpectedBalances([
                    [alice, toWad("99.00")],
                    [bob, toWad("0.00")],
                ]);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    amount: toWad(1).toString(),
                });
                await testExpectedBalances([
                    [alice, toWad("98.00")],
                    [bob, toWad("0.00")],
                ]);
            });

            it("#1.1.8 publisher cannot distribute with insufficient balance", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await expectRevertedWith(
                    shouldDistribute({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        amount: toWad(1).toString(),
                    }),
                    "IDA: E_LOW_BALANCE"
                );
            });

            it("#1.1.9 publisher should not be able to update subscription to zero address", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await expectRevertedWith(
                    t.sf.ida.updateSubscription({
                        superToken: superToken.address,
                        publisher: t.getAddress("alice"),
                        indexId: DEFAULT_INDEX_ID,
                        subscriber: ZERO_ADDRESS,
                        units: toWad("0.001").toString(),
                    }),
                    "IDA: E_NO_ZERO_SUBS"
                );
            });

            it("#1.1.10 publisher should not be able to delete zero address subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await expectRevertedWith(
                    t.sf.ida.deleteSubscription({
                        superToken: superToken.address,
                        indexId: DEFAULT_INDEX_ID,
                        publisher: t.getAddress("alice"),
                        subscriber: ZERO_ADDRESS,
                        sender: t.getAddress("alice"),
                    }),
                    "IDA: E_NO_ZERO_SUBS"
                );
            });

            it("#1.1.11 calling distribute when total units = 0 doesn't revert", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    amount: toWad(1).toString(),
                });
            });
        });

        describe("#1.2 subscription operations", async () => {
            it("#1.2.1 subscriber can approve a subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });

                await verifyAll();
            });

            it("#1.2.2 subscriber should fail to approve a subscription twice", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                await expectRevertedWith(
                    shouldApproveSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                    }),
                    "IDA: E_SUBS_APPROVED"
                );
            });

            it("#1.2.3 subscriber can revoke its approved subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });

                await shouldRevokeSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await verifyAll();
            });

            it("#1.2.4 publisher can delete a subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });

                await shouldDeleteSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "alice",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await verifyAll();
            });

            it("#1.2.5 publisher should fail to delete a non-existen subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await expectRevertedWith(
                    shouldDeleteSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "alice",
                    }),
                    "IDA: E_NO_SUBS"
                );
            });

            it("#1.2.6 one should fail to delete other's subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                await expectRevertedWith(
                    shouldDeleteSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "dan",
                    }),
                    "IDA: E_NOT_ALLOWED"
                );
            });

            it("#1.2.7 subscriber can revoke and resubscribe multiple times to subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);

                await verifyAll();

                await shouldRevokeSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await verifyAll();

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);

                await verifyAll();

                await shouldRevokeSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await verifyAll();
            });

            it("#1.2.8 subscriber can have multiple subscription and then with subId 0 revoked", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "carol",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "carol",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.002").toString(),
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "carol",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                let subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 2);
                assert.equal(subs[0].publisher, alice);
                assert.equal(subs[1].publisher, carol);
                await verifyAll();

                await shouldRevokeSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);
                assert.equal(subs[0].publisher, carol);
                await verifyAll();
            });

            it("#1.2.10 one should fail to use a subscription of a non-existent index", async () => {
                await expectRevertedWith(
                    shouldApproveSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                    }),
                    "IDA: E_NO_INDEX"
                );
                await expectRevertedWith(
                    shouldUpdateSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        units: "42",
                    }),
                    "IDA: E_NO_INDEX"
                );
                await expectRevertedWith(
                    t.sf.ida.getSubscription({
                        superToken: superToken.address,
                        publisher: alice,
                        indexId: DEFAULT_INDEX_ID,
                        subscriber: bob,
                    }),
                    "IDA: E_NO_INDEX"
                );
            });

            it("#1.2.11 subscriber can revoke its subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });
                await testExpectedBalances([
                    [alice, toWad("99.8")],
                    [bob, toWad("0.2")],
                ]);

                await shouldRevokeSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "500",
                });
                await testExpectedBalances([
                    [alice, toWad("99.5")],
                    [bob, toWad("0.2")],
                ]);

                await verifyAll();
            });

            it("#1.2.12 subscriber should fail to revoke an pending subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });
                await expectRevertedWith(
                    shouldRevokeSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                    }),
                    "IDA: E_SUBS_NOT_APPROVED"
                );
            });

            it("#1.2.13 subscriber should fail to revoke a non-existen subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await expectRevertedWith(
                    shouldRevokeSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                    }),
                    "IDA: E_NO_SUBS"
                );
            });

            it("#1.2.14 subscriber should fail to revoke a subscription of a non-existent index", async () => {
                await expectRevertedWith(
                    shouldRevokeSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "bob",
                    }),
                    "IDA: E_NO_INDEX"
                );
            });

            it("#1.2.15 publisher should be able to delete an approved subscription", async () => {
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "500",
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });

                await shouldDeleteSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "alice",
                });
            });

            it("#1.2.16 Max number of subscriptions a subscriber can have", async () => {
                const maxNumberOfSubs = 256;

                for (let i = 0; i < maxNumberOfSubs; i++) {
                    console.log(`Creating subscription ${i}`);
                    await t.sf.ida.createIndex({
                        superToken: superToken.address,
                        publisher: t.getAddress("alice"),
                        indexId: i,
                    });

                    await t.sf.ida.updateSubscription({
                        superToken: superToken.address,
                        publisher: t.getAddress("alice"),
                        indexId: i,
                        subscriber: t.getAddress("bob"),
                        units: toWad(0.01).toString(),
                    });

                    await t.sf.ida.approveSubscription({
                        superToken: superToken.address,
                        publisher: t.getAddress("alice"),
                        indexId: i,
                        subscriber: t.getAddress("bob"),
                    });
                }

                const subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });

                assert.equal(subs.length, maxNumberOfSubs);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: maxNumberOfSubs,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: maxNumberOfSubs,
                    subscriberName: "bob",
                    units: toWad(0.01).toString(),
                });

                expectRevertedWith(
                    shouldApproveSubscription({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: maxNumberOfSubs,
                        subscriberName: "bob",
                    }),
                    "SlotBitmap out of bound"
                );
            });
        });

        describe("#1.3 distribution workflows", () => {
            it("#1.3.1 approveSubscription -> updateSubscription -> updateIndex", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });

                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);
                assert.equal(subs[0].publisher, alice);
                assert.equal(subs[0].indexId, DEFAULT_INDEX_ID);
                assert.equal(subs[0].units, "0");

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });

                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);
                assert.equal(subs[0].publisher, alice);
                assert.equal(subs[0].indexId, DEFAULT_INDEX_ID);
                assert.equal(subs[0].units, toWad("0.001").toString());

                await shouldDistribute({
                    testenv: t,
                    superToken,
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
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.003").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await verifyAll();

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);
                assert.equal(subs[0].publisher, alice);
                assert.equal(subs[0].indexId, DEFAULT_INDEX_ID);
                assert.equal(
                    subs[0].units.toString(),
                    toWad("0.003").toString()
                );

                await verifyAll();
            });

            it("#1.3.3 updateSubscription -> approveSubscription -> updateIndex", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.001").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);
                assert.equal(subs[0].publisher, alice);
                assert.equal(subs[0].indexId, DEFAULT_INDEX_ID);
                assert.equal(
                    subs[0].units.toString(),
                    toWad("0.001").toString()
                );

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);
                assert.equal(subs[0].publisher, alice);
                assert.equal(subs[0].indexId, DEFAULT_INDEX_ID);
                assert.equal(
                    subs[0].units.toString(),
                    toWad("0.001").toString()
                );

                await verifyAll();
            });

            it("#1.3.4 2x(updateSubscription -> shouldDistribute) ->  approveSubscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.003").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.005").toString(),
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "200",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 1);
                assert.equal(subs[0].publisher, alice);
                assert.equal(subs[0].indexId, DEFAULT_INDEX_ID);
                assert.equal(
                    subs[0].units.toString(),
                    toWad("0.005").toString()
                );

                await verifyAll();
            });
        });

        describe("#1.4 claim workflows", () => {
            it("#1.4.1 subscriber can claim distribution from its pending subscription", async () => {
                let subs;
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.003").toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                await testExpectedBalances([
                    [alice, toWad("99.7")],
                    [bob, toWad("0.0")],
                ]);

                subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldClaimPendingDistribution({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "bob",
                });
                await testExpectedBalances([
                    [alice, toWad("99.7")],
                    [bob, toWad("0.3")],
                ]);

                await shouldClaimPendingDistribution({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "bob",
                });
                await testExpectedBalances([
                    [alice, toWad("99.7")],
                    [bob, toWad("0.3")],
                ]);

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                await shouldDistribute({
                    testenv: t,
                    superToken,
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
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });
                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad("0.003").toString(),
                });
                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "100",
                });
                await shouldClaimPendingDistribution({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "dan",
                });
            });

            it("#1.4.3 one should not claim from a non-existent subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await expectRevertedWith(
                    shouldClaimPendingDistribution({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "bob",
                    }),
                    "IDA: E_NO_SUBS"
                );
            });

            it("#1.4.4 one should not claim from a subscription of a non-existent index", async () => {
                await expectRevertedWith(
                    shouldClaimPendingDistribution({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "bob",
                    }),
                    "IDA: E_NO_INDEX"
                );
            });

            it("#1.4.5 subscriber should not claim from a already approved subscription", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });

                await expectRevertedWith(
                    shouldClaimPendingDistribution({
                        testenv: t,
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "bob",
                        senderName: "bob",
                    }),
                    "IDA: E_SUBS_APPROVED"
                );
            });

            it("#1.4.6 cannot claim from zero address", async () => {
                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await expectRevertedWith(
                    t.sf.ida.claim({
                        superToken: superToken.address,
                        indexId: DEFAULT_INDEX_ID,
                        publisher: t.getAddress("alice"),
                        subscriber: ZERO_ADDRESS,
                        sender: t.getAddress("bob"),
                    }),
                    "IDA: E_NO_ZERO_SUBS"
                );
            });
        });

        describe("#1.5 complex sequences", () => {
            it("#1.5.1 distributions using the correct token", async () => {
                await t.upgradeBalance("alice", INIT_BALANCE);
                const {superToken: superToken2} = await t.deployNewToken(
                    "TEST2",
                    {
                        doUpgrade: true,
                        isTruffle: true,
                    }
                );

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad(0.01).toString(),
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    amount: toWad(50).toString(),
                });

                assert.equal(
                    await superToken.balanceOf(
                        t.getAddress("alice").toString()
                    ),
                    toWad("50").toString()
                );
                assert.equal(
                    await superToken2.balanceOf(
                        t.getAddress("alice").toString()
                    ),
                    INIT_BALANCE.toString()
                );
            });

            it("#1.5.2 context should not be exploited", async () => {
                const {superfluid, ida} = t.contracts;

                await expectRevertedWith(
                    superfluid.callAgreement(
                        ida.address,
                        ida.contract.methods
                            .createIndex(
                                superToken.address,
                                DEFAULT_INDEX_ID,
                                web3.eth.abi.encodeParameters(
                                    ["bytes", "bytes"],
                                    ["0xdeadbeef", "0x"]
                                )
                            )
                            .encodeABI(),
                        "0x",
                        {
                            from: alice,
                        }
                    ),
                    "invalid ctx"
                );
            });

            it("#1.5.3 publisher subscribing to their own index and receiving a distribution", async () => {
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "alice",
                    units: toWad(0.01).toString(),
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "alice",
                });

                await t.sf.ida.distribute({
                    superToken: superToken.address,
                    publisher: alice,
                    indexId: DEFAULT_INDEX_ID,
                    amount: toWad(30).toString(),
                });

                await testExpectedBalances([[alice, INIT_BALANCE]]);
            });

            it("#1.5.4 subscribe -> distribute -> unsubscribe -> distribute -> subscribe -> distribute", async () => {
                await t.upgradeBalance("alice", INIT_BALANCE);

                await shouldCreateIndex({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    units: toWad(0.01).toString(),
                });

                await shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    amount: toWad(50).toString(),
                });

                await testExpectedBalances([
                    [alice, toWad("50.00")],
                    [bob, toWad("50.00")],
                ]);
                await shouldRevokeSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                });
                let subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: bob,
                });
                assert.equal(subs.length, 0);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    amount: toWad(25).toString(),
                });

                await testExpectedBalances([
                    [alice, toWad("25.00")],
                    [bob, toWad("50.00")],
                ]);

                await shouldClaimPendingDistribution({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "alice",
                });

                await testExpectedBalances([
                    [alice, toWad("25.00")],
                    [bob, toWad("75.00")],
                ]);

                await shouldDistribute({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    amount: toWad(25).toString(),
                });

                await shouldClaimPendingDistribution({
                    testenv: t,
                    superToken,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                    senderName: "alice",
                });

                await testExpectedBalances([
                    [alice, toWad("0")],
                    [bob, toWad("100.00")],
                ]);
            });
        });
    });

    context("#2 callbacks", () => {
        let app;

        function idaSelector(functionName) {
            return t.sf.agreements.ida.abi.filter(
                (i) => i.name === functionName
            )[0].signature;
        }

        beforeEach(async () => {
            app = await IDASuperAppTester.new(
                t.sf.host.address,
                1 /* APP_TYPE_FINAL_LEVEL */,
                t.sf.agreements.ida.address,
                superToken.address,
                DEFAULT_INDEX_ID
            );
            t.addAlias("app", app.address);
        });

        afterEach(async () => {
            assert.isFalse(
                await t.sf.host.isAppJailed(app.address),
                "App got jailed"
            );
        });

        it("#2.1 approveSubscription AgreementCreated callbacks", async () => {
            const tx = await shouldApproveSubscription({
                testenv: t,
                superToken,
                publisherName: "app",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "alice",
                userData: web3.eth.abi.encodeParameters(
                    ["bytes32", "bytes4", "bytes"],
                    [
                        web3.utils.sha3("created"),
                        idaSelector("approveSubscription"),
                        "0x",
                    ]
                ),
            });
            await expectEvent.notEmitted.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataBefore"
            );
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataAfter",
                {
                    publisher: app.address,
                    indexId: DEFAULT_INDEX_ID,
                    approved: true,
                    units: "0",
                    pendingDistribution: "0",
                }
            );
        });

        it("#2.2 approveSubscription AgreementUpdated callbacks", async () => {
            const units = toWad("0.003").toString();
            await shouldUpdateSubscription({
                testenv: t,
                superToken,
                publisherName: "app",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "alice",
                units,
                fn: async () => {
                    return await web3tx(
                        app.updateSubscription,
                        "app.updateSubscription alice"
                    )(alice, units);
                },
            });
            const tx = await shouldApproveSubscription({
                testenv: t,
                superToken,
                publisherName: "app",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "alice",
                userData: web3.eth.abi.encodeParameters(
                    ["bytes32", "bytes4", "bytes"],
                    [
                        web3.utils.sha3("updated"),
                        idaSelector("approveSubscription"),
                        "0x",
                    ]
                ),
            });
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataBefore",
                {
                    publisher: app.address,
                    indexId: DEFAULT_INDEX_ID,
                    approved: false,
                    units,
                    pendingDistribution: "0",
                }
            );
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataAfter",
                {
                    publisher: app.address,
                    indexId: DEFAULT_INDEX_ID,
                    approved: true,
                    units,
                    pendingDistribution: "0",
                }
            );
        });

        it("#2.3 updateSubscription AgreementCreated callbacks", async () => {
            const units = toWad("0.003").toString();
            await shouldCreateIndex({
                testenv: t,
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });
            const tx = await shouldUpdateSubscription({
                testenv: t,
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "app",
                units,
                userData: web3.eth.abi.encodeParameters(
                    ["bytes32", "bytes4", "bytes"],
                    [
                        web3.utils.sha3("created"),
                        idaSelector("updateSubscription"),
                        "0x",
                    ]
                ),
            });
            await expectEvent.notEmitted.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataBefore"
            );
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataAfter",
                {
                    publisher: alice,
                    indexId: DEFAULT_INDEX_ID,
                    approved: false,
                    units,
                    pendingDistribution: "0",
                }
            );
        });

        it("#2.4 updateSubscription AgreementUpdated callbacks", async () => {
            const units1 = toWad("0.003").toString();
            const units2 = toWad("0.004").toString();
            await shouldCreateIndex({
                testenv: t,
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });
            await shouldUpdateSubscription({
                testenv: t,
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "app",
                units: units1,
                userData: web3.eth.abi.encodeParameters(
                    ["bytes32", "bytes4", "bytes"],
                    [
                        web3.utils.sha3("created"),
                        idaSelector("updateSubscription"),
                        "0x",
                    ]
                ),
            });
            const tx = await shouldUpdateSubscription({
                testenv: t,
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "app",
                units: units2,
                userData: web3.eth.abi.encodeParameters(
                    ["bytes32", "bytes4", "bytes"],
                    [
                        web3.utils.sha3("updated"),
                        idaSelector("updateSubscription"),
                        "0x",
                    ]
                ),
            });
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataBefore",
                {
                    publisher: alice,
                    indexId: DEFAULT_INDEX_ID,
                    approved: false,
                    units: units1,
                    pendingDistribution: "0",
                }
            );
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataAfter",
                {
                    publisher: alice,
                    indexId: DEFAULT_INDEX_ID,
                    approved: false,
                    units: units2,
                    pendingDistribution: "0",
                }
            );
        });

        it("#2.6 publisher deleteSubscription callbacks", async () => {
            const units = toWad("0.003").toString();
            await shouldCreateIndex({
                testenv: t,
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });
            await shouldUpdateSubscription({
                testenv: t,
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "app",
                units,
                userData: web3.eth.abi.encodeParameters(
                    ["bytes32", "bytes4", "bytes"],
                    [
                        web3.utils.sha3("created"),
                        idaSelector("updateSubscription"),
                        "0x",
                    ]
                ),
            });
            const tx = await shouldDeleteSubscription({
                testenv: t,
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "app",
                senderName: "alice",
                userData: web3.eth.abi.encodeParameters(
                    ["bytes32", "bytes4", "bytes"],
                    [
                        web3.utils.sha3("deleted"),
                        idaSelector("deleteSubscription"),
                        "0x",
                    ]
                ),
            });
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataBefore",
                {
                    publisher: alice,
                    indexId: DEFAULT_INDEX_ID,
                    approved: false,
                    units,
                    pendingDistribution: "0",
                }
            );
            await expectEvent.notEmitted.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataAfter"
            );
        });

        it("#2.7 claim callbacks", async () => {
            await t.upgradeBalance("alice", INIT_BALANCE);
            await t.transferBalance("alice", "app", INIT_BALANCE);

            const units = toWad("0.005").toString();
            const distributionAmount = toWad(1).toString();
            await shouldUpdateSubscription({
                testenv: t,
                superToken,
                publisherName: "app",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "alice",
                units,
                fn: async () => {
                    return await web3tx(
                        app.updateSubscription,
                        "app.updateSubscription alice"
                    )(alice, units);
                },
            });
            await shouldDistribute({
                testenv: t,
                superToken,
                publisherName: "app",
                indexId: DEFAULT_INDEX_ID,
                amount: distributionAmount,
                fn: async () => {
                    return await web3tx(
                        app.distribute,
                        "app.distribute"
                    )(distributionAmount);
                },
            });
            const tx = await shouldClaimPendingDistribution({
                testenv: t,
                superToken,
                publisherName: "app",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "alice",
                senderName: "dan",
                userData: web3.eth.abi.encodeParameters(
                    ["bytes32", "bytes4", "bytes"],
                    [web3.utils.sha3("updated"), idaSelector("claim"), "0x"]
                ),
            });
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataBefore",
                {
                    publisher: app.address,
                    indexId: DEFAULT_INDEX_ID,
                    approved: false,
                    units,
                    pendingDistribution: distributionAmount,
                }
            );
            await expectEvent.inTransaction(
                tx.tx,
                IDASuperAppTester,
                "SubscriptionDataAfter",
                {
                    publisher: app.address,
                    indexId: DEFAULT_INDEX_ID,
                    approved: false,
                    units,
                    pendingDistribution: "0",
                }
            );
        });

        it("#2.8 getSubscriptionByID revert with E_NO_SUBS", async () => {
            await app.setForceGetSubscriptionByID();
            await expectRevertedWith(
                shouldApproveSubscription({
                    testenv: t,
                    superToken,
                    publisherName: "app",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "alice",
                    userData: web3.eth.abi.encodeParameters(
                        ["bytes32", "bytes4", "bytes"],
                        [
                            web3.utils.sha3("created"),
                            idaSelector("approveSubscription"),
                            "0x",
                        ]
                    ),
                }),
                "IDA: E_NO_SUBS"
            );
        });
    });

    context("#3 misc", async () => {
        it("#4.1 only authorized host can access token", async () => {
            const FakeSuperfluidMock = artifacts.require("FakeSuperfluidMock");
            const fakeHost = await FakeSuperfluidMock.new();
            const ida = t.sf.agreements.ida;
            await expectRevertedWith(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .createIndex(superToken.address, 42, "0x")
                        .encodeABI(),
                    {from: alice}
                ),
                "unauthorized host"
            );
            await expectRevertedWith(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .updateIndex(superToken.address, 42, 9000, "0x")
                        .encodeABI(),
                    {from: alice}
                ),
                "unauthorized host"
            );
            await expectRevertedWith(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .distribute(superToken.address, 42, 9000, "0x")
                        .encodeABI(),
                    {from: alice}
                ),
                "unauthorized host"
            );
            await expectRevertedWith(
                fakeHost.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .approveSubscription(superToken.address, bob, 42, "0x")
                        .encodeABI(),
                    {from: alice}
                ),
                "unauthorized host"
            );
            await expectRevertedWith(
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
                    {from: alice}
                ),
                "unauthorized host"
            );
        });
    });
});
