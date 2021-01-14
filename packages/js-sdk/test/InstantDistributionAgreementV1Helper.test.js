const { toBN, toWad } = require("@decentral.ee/web3-helpers");
const { expectRevert } = require("@openzeppelin/test-helpers");
const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");

contract("InstantDistributionAgreementV1Helper helper class", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 4), { isTruffle: true });
    const { admin, alice, bob, carol } = t.aliases;

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
                indexId,
                sender: alice
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
                indexId,
                sender: alice
            });

            const indexValue = 100;
            await sf.ida.updateIndex({
                superToken: superToken.address,
                indexId,
                indexValue,
                sender: alice
            });

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
            assert.equal(index.indexValue, indexValue);
        });

        // it("claim", async () => {
        //     const indexId = 1;
        //     const subscriber = bob;
        //     const publisher = alice;
        //
        //     await sf.ida.createIndex({
        //         superToken: superToken.address,
        //         indexId,
        //         sender: publisher
        //     });
        //
        //     await sf.ida.updateSupscription({
        //         superToken: superToken.address,
        //         indexId,
        //         subscriber,
        //         units: toWad("0.001").toString(),
        //         sender: publisher
        //     });
        //
        //     await sf.ida.updateIndex({
        //         superToken: superToken.address,
        //         indexId,
        //         indexValue: toWad("0.001").toString(),
        //         sender: publisher
        //     });
        //
        //     const balanceBefore = await superToken.balanceOf(subscriber);
        //     assert.equal(balanceBefore.toString(), toWad("100.1").toString());
        //
        //     await sf.ida.claim({
        //         superToken: superToken.address,
        //         publisher,
        //         indexId,
        //         subscriber,
        //         sender: subscriber
        //     });
        //
        //     const balanceAfter = await superToken.balanceOf(subscriber);
        //     assert.equal(balanceAfter.toString(), toWad(110).toString());
        // });
    });

    describe("subscriptions", () => {
        const indexId = 1;
        const subscriber = bob;
        const publisher = alice;
        beforeEach(async () => {
            await sf.ida.createIndex({
                superToken: superToken.address,
                indexId,
                sender: publisher
            });
        });

        it("updateSupscription", async () => {
            const units = 100;
            await sf.ida.updateSupscription({
                superToken: superToken.address,
                indexId,
                subscriber,
                units,
                sender: publisher
            });

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
            assert.equal(index.totalUnitsPending, units);
        });

        it("approveSupscription", async () => {
            const units = 100;
            await sf.ida.updateSupscription({
                superToken: superToken.address,
                indexId,
                subscriber,
                units,
                sender: publisher
            });

            await sf.ida.approveSupscription({
                superToken: superToken.address,
                indexId,
                publisher,
                sender: subscriber
            });

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
            assert.equal(index.totalUnitsApproved, units);
        });

        it("deleteSupscription from publisher", async () => {
            const units = 100;
            await sf.ida.updateSupscription({
                superToken: superToken.address,
                indexId,
                subscriber,
                units,
                sender: publisher
            });

            await sf.ida.deleteSupscription({
                superToken: superToken.address,
                indexId,
                publisher,
                subscriber,
                sender: publisher
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
                indexId,
                sender: publisher
            });
            await sf.ida.updateSupscription({
                superToken: superToken.address,
                indexId,
                subscriber: subscriber1,
                units: subscriber1Units.toString(),
                sender: publisher
            });
            await sf.ida.approveSupscription({
                superToken: superToken.address,
                indexId,
                publisher,
                sender: subscriber1
            });
        });

        it("distribute - simple case", async () => {
            const amount = toWad(100);
            await sf.ida.distribute({
                superToken: superToken.address,
                indexId,
                amount: amount.toString(),
                sender: publisher
            });
            const balance = await superToken.balanceOf(subscriber2);
            assert.equal(balance.toString(), toWad(100).toString());
        });

        it("distribute - multiple subscribers", async () => {
            const amount = toWad(100);
            // Add the second additional subscriber
            await sf.ida.updateSupscription({
                superToken: superToken.address,
                indexId,
                subscriber: subscriber2,
                units: subscriber2Units.toString(),
                sender: publisher
            });

            await sf.ida.approveSupscription({
                superToken: superToken.address,
                indexId,
                publisher,
                sender: subscriber2
            });

            await sf.ida.distribute({
                superToken: superToken.address,
                indexId,
                amount: amount.toString(),
                sender: publisher
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
            const units = 100;
            const halfUnits = 50;
            const publisher = alice;

            await sf.ida.createIndex({
                superToken: superToken.address,
                indexId,
                sender: publisher
            });

            await sf.ida.updateSupscription({
                superToken: superToken.address,
                indexId,
                subscriber: bob,
                units: halfUnits,
                sender: publisher
            });

            await sf.ida.updateSupscription({
                superToken: superToken.address,
                indexId,
                subscriber: carol,
                units: halfUnits,
                sender: publisher
            });

            await sf.ida.approveSupscription({
                superToken: superToken.address,
                indexId,
                publisher,
                sender: bob
            });

            // Carol doesn't approve, so her units are still "pending"

            const index = await sf.ida.getIndex({
                superToken: superToken.address,
                publisher,
                indexId
            });
            const indexDetails = {
                exist: true,
                indexValue: "0",
                totalUnitsApproved: halfUnits.toString(),
                totalUnitsPending: halfUnits.toString()
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
                indexId,
                sender: publisher
            });

            await sf.ida.updateSupscription({
                superToken: superToken.address,
                indexId,
                subscriber,
                units: units.toString(),
                sender: publisher
            });

            await sf.ida.approveSupscription({
                superToken: superToken.address,
                indexId,
                publisher,
                sender: subscriber
            });

            const {
                publishers,
                indexIds,
                unitsList
            } = await sf.ida.listSubscriptions({
                superToken: superToken.address,
                subscriber
            });

            assert.deepEqual(publishers, [publisher]);
            assert.deepEqual(indexIds, [indexId]);
            assert.deepEqual(unitsList, [units.toString()]);
        });
    });
});
