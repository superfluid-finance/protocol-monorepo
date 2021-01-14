const { toBN } = require("@decentral.ee/web3-helpers");
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
            console.log(index.totalUnitsApproved === units);
            assert.equal(index.totalUnitsApproved, units);
        });

        it("deleteSupscription", async () => {
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
                sender: subscriber
            });

            const index = await sf.agreements.ida.getIndex(
                superToken.address,
                alice,
                indexId
            );
            console.log(JSON.stringify(index));
            assert.equal(index.totalUnitsApproved, units);
        });
    });
});
