const { toWad } = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");

contract("InstantDistributionAgreementV1Helper helper class", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 4), { isTruffle: true });
    const { alice, bob, carol } = t.aliases;

    let sf;
    let superToken;

    before(async () => {
        await t.reset();
        sf = t.sf;
    });

    beforeEach(async () => {
        await t.createNewToken({ doUpgrade: true });
        ({ superToken } = t.contracts);
    });

    describe("index", async () => {
        it("createIndex", async () => {
            const indexId = 1;
            await sf.ida.createIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId,
            });

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
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

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
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
        const subscriber = bob;
        const publisher = alice;
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

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
            assert.equal(index.totalUnitsPending, units);
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

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
            assert.equal(index.totalUnitsApproved, units);
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

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
            assert.equal(index.totalUnitsApproved, 0);
        });
    });

    describe("distribution", () => {
        const indexId = 1;
        const publisher = alice;
        const subscriber1 = bob;
        const subscriber1Units = toWad("9");
        const subscriber2 = carol;
        const subscriber2Units = toWad("1");
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
            const indexId = "1";
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
    });
});
