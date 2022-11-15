const {toWad} = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");

describe("InstantDistributionAgreementV1Helper class", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let alice, bob, carol;
    let sf;
    let superToken;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: false,
            web3,
            nAccounts: 4,
        });

        ({alice, bob, carol} = t.aliases);
        sf = t.sf;

        ({superToken} = await t.deployNewToken("TEST2", {
            isTruffle: false,
            web3,
            doUpgrade: true,
        }));
        await t.pushEvmSnapshot();
    });

    after(async () => {
        await t.popEvmSnapshot();
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    describe("index", async () => {
        it("createIndex", async () => {
            const indexId = 1;
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId,
            });

            const index = await sf.ida.getIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId,
            });
            assert.equal(index.exist, true);
        });

        it("updateIndex", async () => {
            const indexId = 1;
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId,
            });

            const indexValue = 100;
            await sf.ida.updateIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId,
                indexValue,
            });

            const index = await sf.ida.getIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId,
            });
            assert.equal(index.indexValue, indexValue);
        });

        it("claim", async () => {
            const indexId = 1;
            const subscriber = bob;
            const publisher = alice;

            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher,
                indexId,
            });

            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
                units: toWad("0.001").toString(),
            });

            await sf.ida.updateIndex({
                superToken: superToken.address,
                publisher,
                indexId,
                indexValue: "1000",
            });

            const balanceBefore = await superToken.balanceOf(subscriber);
            assert.equal(balanceBefore.toString(), toWad("100").toString());

            await sf.ida.claim({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
                sender: subscriber,
            });

            const balanceAfter = await superToken.balanceOf(subscriber);
            assert.equal(balanceAfter.toString(), toWad("101").toString());
        });
    });

    describe("subscriptions", () => {
        const indexId = 1;
        let subscriber, publisher;

        before(() => {
            subscriber = bob;
            publisher = alice;
        });

        beforeEach(async () => {
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher,
                indexId,
            });
        });

        it("updateSubscription", async () => {
            const units = 100;
            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
                units,
            });

            const subscription = await sf.ida.getSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
            });
            assert.equal(subscription.exist, true);
            assert.equal(subscription.units, units);
            assert.equal(subscription.approved, false);
        });

        it("approveSubscription", async () => {
            const units = 100;
            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
                units,
            });

            await sf.ida.approveSubscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber,
            });

            const subscription = await sf.ida.getSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
            });
            assert.equal(subscription.exist, true);
            assert.equal(subscription.approved, true);
        });

        it("revokeSubscription", async () => {
            const units = 100;
            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
                units,
            });

            await sf.ida.approveSubscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber,
            });

            await sf.ida.revokeSubscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber,
            });

            const subscription = await sf.ida.getSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
            });
            assert.equal(subscription.exist, true);
            assert.equal(subscription.approved, false);
        });

        it("deleteSubscription from publisher", async () => {
            const units = 100;
            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
                units,
            });

            await sf.ida.deleteSubscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber,
                sender: publisher,
            });

            const subscription = await sf.ida.getSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
            });
            assert.equal(subscription.exist, false);
        });
    });

    describe("distribution", () => {
        const indexId = 1;
        let publisher, subscriber1, subscriber2;
        const subscriber1Units = toWad("9");
        const subscriber2Units = toWad("1");

        before(() => {
            publisher = alice;
            subscriber1 = bob;
            subscriber2 = carol;
        });

        beforeEach(async () => {
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher,
                indexId,
            });
            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber: subscriber1,
                units: subscriber1Units.toString(),
            });
            await sf.ida.approveSubscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber: subscriber1,
            });
        });

        it("distribute - simple case", async () => {
            const amount = toWad(100);
            await sf.ida.distribute({
                superToken: superToken.address,
                publisher,
                indexId,
                amount: amount.toString(),
            });
            const balance = await superToken.balanceOf(subscriber2);
            assert.equal(balance.toString(), toWad(100).toString());
        });

        it("distribute - multiple subscribers", async () => {
            const amount = toWad(100);
            // Add the second additional subscriber
            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber: subscriber2,
                units: subscriber2Units.toString(),
            });

            await sf.ida.approveSubscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber: subscriber2,
            });

            await sf.ida.distribute({
                superToken: superToken.address,
                publisher,
                indexId,
                amount: amount.toString(),
            });
            const balance1 = await superToken.balanceOf(subscriber1);
            const balance2 = await superToken.balanceOf(subscriber2);
            assert.equal(balance1.toString(), toWad(190).toString());
            assert.equal(balance2.toString(), toWad(110).toString());
        });
    });

    describe("details", () => {
        it("getIndex", async () => {
            const indexId = 1;
            const halfUnits = 50;
            const publisher = alice;

            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher,
                indexId,
            });

            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber: bob,
                units: halfUnits,
            });

            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber: carol,
                units: halfUnits,
            });

            await sf.ida.approveSubscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber: bob,
            });

            // Carol doesn't approve, so her units are still "pending"

            const index = await sf.ida.getIndex({
                superToken: superToken.address,
                publisher,
                indexId,
            });
            const indexDetails = {
                exist: true,
                indexValue: "0",
                totalUnitsApproved: halfUnits.toString(),
                totalUnitsPending: halfUnits.toString(),
            };
            assert.deepEqual(index, indexDetails);
        });

        it("listSubscriptions", async () => {
            const indexId = 1;
            const units = toWad(100);
            const publisher = alice;
            const subscriber = bob;
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher,
                indexId,
            });

            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId,
                subscriber,
                units: units.toString(),
            });

            await sf.ida.approveSubscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber: subscriber,
            });

            assert.deepEqual(
                await sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber,
                }),
                [
                    {
                        publisher,
                        indexId,
                        units: units.toString(),
                    },
                ]
            );
        });

        it("listSubscribers", async () => {
            const units = toWad(100).toString();
            const publisher = alice;
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher,
                indexId: 1,
            });
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher,
                indexId: 2,
            });

            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId: 1,
                subscriber: bob,
                units,
            });
            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId: 1,
                subscriber: carol,
                units,
            });

            assert.deepEqual(
                await sf.ida.listSubscribers({
                    superToken: superToken.address,
                    publisher,
                    indexId: 1,
                }),
                [
                    {
                        subscriber: bob,
                        units,
                    },
                    {
                        subscriber: carol,
                        units,
                    },
                ]
            );

            await sf.ida.updateSubscription({
                superToken: superToken.address,
                publisher,
                indexId: 1,
                subscriber: carol,
                units: "0",
            });

            assert.deepEqual(
                await sf.ida.listSubscribers({
                    superToken: superToken.address,
                    publisher,
                    indexId: 1,
                }),
                [
                    {
                        subscriber: bob,
                        units,
                    },
                ]
            );

            assert.deepEqual(
                await sf.ida.listSubscribers({
                    superToken: superToken.address,
                    publisher,
                    indexId: 2,
                }),
                []
            );
        });

        it("listIndices", async () => {
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId: 1,
            });
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId: 2,
            });
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher: bob,
                indexId: 2,
            });

            assert.deepEqual(
                await sf.ida.listIndices({
                    superToken: superToken.address,
                    publisher: alice,
                }),
                [1, 2]
            );

            assert.deepEqual(
                await sf.ida.listIndices({
                    superToken: superToken.address,
                    publisher: bob,
                }),
                [2]
            );
        });
    });
});
