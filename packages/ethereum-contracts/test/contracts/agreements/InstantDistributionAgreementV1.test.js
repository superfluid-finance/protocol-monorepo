const { expectRevert } = require("@openzeppelin/test-helpers");
const { web3tx, wad4human, toWad } = require("@decentral.ee/web3-helpers");
const {
    shouldCreateIndex,
    shouldApproveSubscription,
    shouldUpdateSubscription,
    shouldUpdateIndex,
} = require("./InstantDistributionAgreementV1.behaviour.js");

const TestEnvironment = require("../../TestEnvironment");

const DEFAULT_INDEX_ID = 42;

contract("Using InstanceDistributionAgreement v1", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 5), {
        isTruffle: true,
        useMocks: true,
    });
    const { alice, bob, carol, dan } = t.aliases;
    const { INIT_BALANCE } = t.configs;

    let superToken;
    let ida;
    let superfluid;

    before(async () => {
        await t.reset();
        ({ superfluid, ida } = t.contracts);
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

    describe("#2 index operations", async () => {
        it("#2.1 create a new index", async () => {
            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await t.validateSystemInvariance();
        });

        it("#2.2 should fail to create the same index", async () => {
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

        it("#2.3 should fail to query non-existant index", async () => {
            const idata = await t.sf.ida.getIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId: DEFAULT_INDEX_ID,
            });
            assert.isFalse(idata.exist);
        });

        it("#2.4 update index", async () => {
            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            const idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "1984",
            });
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(idata.totalUnitsPending, "0");

            await t.validateSystemInvariance();
        });

        it("#2.5 should fail to update non-existent index", async () => {
            await expectRevert(
                shouldUpdateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "1984",
                }),
                "IDA: E_NO_INDEX"
            );
        });

        it("#2.5 should fail to update index with smaller value", async () => {
            let idata;

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "1984",
            });
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(idata.totalUnitsPending, "0");

            idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "1984",
            });
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(idata.totalUnitsPending, "0");

            await expectRevert(
                shouldUpdateIndex({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    indexValue: "1983",
                }),
                "IDA: E_INDEX_GROW"
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
        });
    });

    describe("#3 subscription operations", async () => {
        it("#3.1 update an approved subcription then distribute", async () => {
            let idata;
            let sdata;
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            sdata = await shouldApproveSubscription({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "bob",
            });
            assert.equal(sdata.units.toString(), "0");
            assert.equal(sdata.pendingDistribution.toString(), "0");

            idata = await t.sf.ida.getIndex({
                superToken: superToken.address,
                publisher: alice,
                indexId: DEFAULT_INDEX_ID,
            });
            assert.equal(idata.indexValue, "0");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(idata.totalUnitsPending, "0");

            await testExpectedBalances([
                [alice, toWad("100")],
                [bob, toWad("0")],
            ]);

            await expectRevert(
                shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "bob",
                }),
                "IDA: E_SUBS_APPROVED"
            );

            subs = await t.sf.ida.listSubscriptions({
                superToken: superToken.address,
                subscriber: bob,
            });
            assert.equal(subs.publishers.length, 1);
            assert.equal(subs.publishers[0], alice);
            assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
            assert.equal(subs.unitsList[0], "0");

            sdata = await shouldUpdateSubscription({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "bob",
                units: toWad("0.001").toString(),
            });
            assert.equal(sdata.pendingDistribution.toString(), "0");

            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 1);
            assert.equal(subs.publishers[0], alice);
            assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
            assert.equal(subs.unitsList[0], toWad("0.001").toString());

            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "0");
            assert.equal(idata.totalUnitsApproved, toWad("0.001").toString());
            assert.equal(idata.totalUnitsPending, "0");
            await testExpectedBalances([
                [alice, toWad("100")],
                [bob, toWad("0")],
            ]);

            idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "bob",
                indexValue: "100",
            });
            await web3tx(superfluid.callAgreement, "Alice update the index")(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        "100",
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "100");
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.001").toString()
            );
            assert.equal(idata.totalUnitsPending, "0");
            await testExpectedBalances([
                [alice, toWad("99.9")],
                [bob, toWad("0.1")],
            ]);

            await t.validateSystemInvariance();
        });

        it("#3.2 distribute to a pending subcription then approve", async () => {
            let idata;
            let sdata;
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.001").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "0");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.001").toString()
            );
            await testExpectedBalances([
                [alice, toWad("100")],
                [bob, toWad("0")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.001").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);

            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription again"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.003").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "0");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("100")],
                [bob, toWad("0")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.003").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);

            await web3tx(superfluid.callAgreement, "Alice update the index")(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        "100",
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "100");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.7")], // FIXME check deposit
                [bob, toWad("0")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.003").toString());
            assert.equal(
                sdata.pendingDistribution.toString(),
                toWad("0.3").toString()
            );
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);
            await t.validateSystemInvariance();

            await web3tx(
                superfluid.callAgreement,
                "Bob approve the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .approveSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            await expectRevert(
                web3tx(
                    superfluid.callAgreement,
                    "Bob approve the subscription"
                )(
                    ida.address,
                    ida.contract.methods
                        .approveSubscription(
                            superToken.address,
                            alice,
                            DEFAULT_INDEX_ID,
                            "0x"
                        )
                        .encodeABI(),
                    "0x",
                    {
                        from: bob,
                    }
                ),
                "IDA: E_SUBS_APPROVED"
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "100");
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.003").toString()
            );
            assert.equal(idata.totalUnitsPending, "0");
            await testExpectedBalances([
                [alice, toWad("99.7")],
                [bob, toWad("0.3")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isTrue(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.003").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 1);
            assert.equal(subs.publishers[0], alice);
            assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
            assert.equal(
                subs.unitsList[0].toString(),
                toWad("0.003").toString()
            );

            await t.validateSystemInvariance();
        });

        it("#3.3 approve a pending subcription before distribution", async () => {
            let idata;
            let sdata;
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.001").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "0");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.001").toString()
            );
            await testExpectedBalances([
                [alice, toWad("100")],
                [bob, toWad("0")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.001").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);

            await web3tx(
                superfluid.callAgreement,
                "Bob approve the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .approveSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            await expectRevert(
                web3tx(
                    superfluid.callAgreement,
                    "Bob approve the subscription"
                )(
                    ida.address,
                    ida.contract.methods
                        .approveSubscription(
                            superToken.address,
                            alice,
                            DEFAULT_INDEX_ID,
                            "0x"
                        )
                        .encodeABI(),
                    "0x",
                    {
                        from: bob,
                    }
                ),
                "IDA: E_SUBS_APPROVED"
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "0");
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.001").toString()
            );
            assert.equal(idata.totalUnitsPending, "0");
            await testExpectedBalances([
                [alice, toWad("100")],
                [bob, toWad("0")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isTrue(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.001").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 1);
            assert.equal(subs.publishers[0], alice);
            assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
            assert.equal(
                subs.unitsList[0].toString(),
                toWad("0.001").toString()
            );

            await web3tx(superfluid.callAgreement, "Alice update the index")(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        "100",
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "100");
            assert.equal(sdata.units.toString(), toWad("0.001").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            await testExpectedBalances([
                [alice, toWad("99.9")],
                [bob, toWad("0.1")],
            ]);
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 1);
            assert.equal(subs.publishers[0], alice);
            assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
            assert.equal(
                subs.unitsList[0].toString(),
                toWad("0.001").toString()
            );

            await t.validateSystemInvariance();
        });

        it("#3.4 distribute to a pending subcription, update it, distribute again and approve", async () => {
            let idata;
            let sdata;
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.003").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "0");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("100")],
                [bob, toWad("0")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.003").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);

            await web3tx(superfluid.callAgreement, "Alice update the index")(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        "100",
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "100");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.7")], // FIXME check deposit
                [bob, toWad("0")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.003").toString());
            assert.equal(
                sdata.pendingDistribution.toString(),
                toWad("0.3").toString()
            );
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);
            await t.validateSystemInvariance();

            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription again"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.005").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue.toString(), "100");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.005").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.7")], // FIXME check deposit
                [bob, toWad("0.3")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.005").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);
            await t.validateSystemInvariance();

            await web3tx(
                superfluid.callAgreement,
                "Alice update the index again"
            )(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        "200",
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue.toString(), "200");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.005").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.2")], // FIXME check deposit
                [bob, toWad("0.3")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.005").toString());
            assert.equal(
                sdata.pendingDistribution.toString(),
                toWad("0.5").toString()
            );
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);
            await t.validateSystemInvariance();

            await web3tx(
                superfluid.callAgreement,
                "Bob approve the subscription finally"
            )(
                ida.address,
                ida.contract.methods
                    .approveSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isTrue(sdata.exist);
            assert.isTrue(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.005").toString());
            assert.equal(sdata.pendingDistribution.toString(), "0");
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 1);
            assert.equal(subs.publishers[0], alice);
            assert.equal(subs.indexIds[0], DEFAULT_INDEX_ID);
            assert.equal(
                subs.unitsList[0].toString(),
                toWad("0.005").toString()
            );

            await t.validateSystemInvariance();
        });

        it("#3.5 subscriber delete its approved subscription", async () => {
            let idata;
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await web3tx(
                superfluid.callAgreement,
                "Bob approve the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .approveSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 1);
            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.001").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            await web3tx(
                superfluid.callAgreement,
                "Alice distribute tokens again with index -> 200"
            )(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        200,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            await expectRevert(
                web3tx(
                    superfluid.callAgreement,
                    "Dan try to delete the subscription"
                )(
                    ida.address,
                    ida.contract.methods
                        .deleteSubscription(
                            superToken.address,
                            alice,
                            DEFAULT_INDEX_ID,
                            bob,
                            "0x"
                        )
                        .encodeABI(),
                    "0x",
                    {
                        from: dan,
                    }
                ),
                "IDA: E_NOT_ALLOWED"
            );
            await web3tx(
                superfluid.callAgreement,
                "Bob delete the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .deleteSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        bob,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            await expectRevert(
                web3tx(
                    superfluid.callAgreement,
                    "Bob try to delete the subscription again"
                )(
                    ida.address,
                    ida.contract.methods
                        .deleteSubscription(
                            superToken.address,
                            alice,
                            DEFAULT_INDEX_ID,
                            bob,
                            "0x"
                        )
                        .encodeABI(),
                    "0x",
                    {
                        from: bob,
                    }
                ),
                "IDA: E_NO_SUBS"
            );
            await testExpectedBalances([
                [alice, toWad("99.8")], // FIXME check deposit
                [bob, toWad("0.2")],
            ]);
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "200");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(idata.totalUnitsPending.toString(), "0");
            const sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isFalse(sdata.exist);
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);

            await t.validateSystemInvariance();
        });

        it("#3.6 subscriber delete its pending subscription", async () => {
            let idata;
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.001").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            await web3tx(
                superfluid.callAgreement,
                "Alice distribute tokens again with index -> 200"
            )(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        200,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            await web3tx(
                superfluid.callAgreement,
                "Bob delete the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .deleteSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        bob,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            await testExpectedBalances([
                [alice, toWad("99.8")], // FIXME check deposit
                [bob, toWad("0.2")],
            ]);
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "200");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(idata.totalUnitsPending.toString(), "0");
            const sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isFalse(sdata.exist);
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);

            await t.validateSystemInvariance();
        });

        it("#3.7 publisher delete a subscription", async () => {
            let idata;
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.001").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            await web3tx(
                superfluid.callAgreement,
                "Alice distribute tokens again with index -> 200"
            )(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        200,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            await web3tx(
                superfluid.callAgreement,
                "Alice delete the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .deleteSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        bob,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            await testExpectedBalances([
                [alice, toWad("99.8")], // FIXME check deposit
                [bob, toWad("0.2")],
            ]);
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "200");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(idata.totalUnitsPending.toString(), "0");
            const sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isFalse(sdata.exist);
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);

            await t.validateSystemInvariance();
        });

        it("#3.8 subscriber delete then resubscribe a subscription", async () => {
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await web3tx(
                superfluid.callAgreement,
                "Bob approve the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .approveSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 1);
            await web3tx(
                superfluid.callAgreement,
                "Bob delete the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .deleteSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        bob,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);
            await web3tx(
                superfluid.callAgreement,
                "Bob approve the subscription again"
            )(
                ida.address,
                ida.contract.methods
                    .approveSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 1);
        });

        it("#3.9 claim distribution from pending subscriptions", async () => {
            let idata;
            let sdata;
            let subs;
            await superToken.upgrade(INIT_BALANCE, { from: alice });

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            await expectRevert(
                superfluid.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .claim(
                            superToken.address,
                            alice,
                            DEFAULT_INDEX_ID,
                            bob,
                            "0x"
                        )
                        .encodeABI(),
                    "0x",
                    {
                        from: bob,
                    }
                ),
                "IDA: E_NO_SUBS"
            );

            await web3tx(
                superfluid.callAgreement,
                "Alice update the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        bob,
                        toWad("0.003").toString(),
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );

            await web3tx(superfluid.callAgreement, "Alice update the index")(
                ida.address,
                ida.contract.methods
                    .updateIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        "100",
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "100");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.7")], // FIXME check deposit
                [bob, toWad("0")],
            ]);
            sdata = await ida.getSubscription.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID,
                bob
            );
            assert.isFalse(sdata.approved);
            assert.equal(sdata.units.toString(), toWad("0.003").toString());
            assert.equal(
                sdata.pendingDistribution.toString(),
                toWad("0.3").toString()
            );
            subs = await ida.listSubscriptions.call(superToken.address, bob);
            assert.equal(subs.publishers.length, 0);
            await t.validateSystemInvariance();

            await web3tx(
                superfluid.callAgreement,
                "Bob claims his pending distribution"
            )(
                ida.address,
                ida.contract.methods
                    .claim(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        bob,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "100");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.7")], // FIXME check deposit
                [bob, toWad("0.3")],
            ]);

            await web3tx(
                superfluid.callAgreement,
                "Alice claims bob's pending distribution"
            )(
                ida.address,
                ida.contract.methods
                    .claim(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        bob,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: alice,
                }
            );
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue, "100");
            assert.equal(idata.totalUnitsApproved, "0");
            assert.equal(
                idata.totalUnitsPending.toString(),
                toWad("0.003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.7")], // FIXME check deposit
                [bob, toWad("0.3")],
            ]);

            await web3tx(
                superfluid.callAgreement,
                "Bob approve the subscription"
            )(
                ida.address,
                ida.contract.methods
                    .approveSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {
                    from: bob,
                }
            );
            await expectRevert(
                superfluid.callAgreement(
                    ida.address,
                    ida.contract.methods
                        .claim(
                            superToken.address,
                            alice,
                            DEFAULT_INDEX_ID,
                            bob,
                            "0x"
                        )
                        .encodeABI(),
                    "0x",
                    {
                        from: bob,
                    }
                ),
                "IDA: E_SUBS_APPROVED"
            );

            await t.validateSystemInvariance();
        });
    });

    describe("#4 misc", async () => {
        it("#4.1 only authorized host can access token", async () => {
            const FakeSuperfluidMock = artifacts.require("FakeSuperfluidMock");
            const fakeHost = await FakeSuperfluidMock.new();
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

    describe("#10 end to end scenarios", async () => {
        it("#1.1 1to3 distribution scenario", async () => {
            await superToken.upgrade(INIT_BALANCE, { from: alice });
            let idata;
            let sdata;

            await shouldCreateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
            });

            const subscribers = [
                [bob, toWad("0.0001")],
                [carol, toWad("0.0002")],
                [dan, toWad("0.0003")],
            ];
            for (let i = 0; i < subscribers.length; ++i) {
                const subscriberAddr = subscribers[i][0];
                const subscriptionUnits = subscribers[i][1];
                const subscriberName = t.toAlias(subscriberAddr);
                sdata = await shouldApproveSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: subscriberName,
                });
                assert.equal(sdata.units.toString(), "0");
                assert.equal(sdata.pendingDistribution.toString(), "0");

                sdata = await shouldUpdateSubscription({
                    testenv: t,
                    publisherName: "alice",
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: subscriberName,
                    units: subscriptionUnits.toString(),
                });
                assert.isTrue(sdata.approved);
                assert.equal(sdata.pendingDistribution.toString(), "0");

                const subs = await t.sf.ida.listSubscriptions({
                    superToken: superToken.address,
                    subscriber: subscriberAddr,
                });
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
            }

            idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "100",
            });
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.0006").toString()
            );

            await testExpectedBalances([
                [bob, toWad("0.01")],
                [carol, toWad("0.02")],
                [dan, toWad("0.03")],
                [alice, toWad("99.94")],
            ]);

            idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "300",
            });
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.0006").toString()
            );

            await testExpectedBalances([
                [bob, toWad("0.03")],
                [carol, toWad("0.06")],
                [dan, toWad("0.09")],
                [alice, toWad("99.82")],
            ]);

            await t.validateSystemInvariance();
        });

        it("#1.2 2to1 distribution scenario", async () => {
            await superToken.upgrade(INIT_BALANCE, { from: alice });
            await superToken.upgrade(INIT_BALANCE, { from: bob });
            let idata;
            let sdata;

            // alice and bob create indeces and dan subscribes to them
            const publishers = [
                [alice, toWad("0.0001")],
                [bob, toWad("0.0002")],
            ];
            for (let i = 0; i < publishers.length; ++i) {
                let publisherAddr = publishers[i][0];
                let subscriptionUnits = publishers[i][1];
                const publisherName = t.toAlias(publisherAddr);

                await shouldCreateIndex({
                    testenv: t,
                    publisherName,
                    indexId: DEFAULT_INDEX_ID,
                });

                await shouldApproveSubscription({
                    testenv: t,
                    publisherName,
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "dan",
                });

                sdata = await shouldUpdateSubscription({
                    testenv: t,
                    publisherName,
                    indexId: DEFAULT_INDEX_ID,
                    subscriberName: "dan",
                    units: subscriptionUnits.toString(),
                });
                assert.isTrue(sdata.approved);
                assert.equal(sdata.pendingDistribution.toString(), "0");
            }

            const subs = await t.sf.ida.listSubscriptions({
                superToken: superToken.address,
                subscriber: dan,
            });
            assert.equal(subs.publishers.length, 2);
            assert.equal(subs.publishers[0], alice);
            assert.equal(
                subs.indexIds[0].toString(),
                DEFAULT_INDEX_ID.toString()
            );
            assert.equal(wad4human(subs.unitsList[0]), "0.00010");
            assert.equal(subs.publishers[1], bob);
            assert.equal(
                subs.indexIds[1].toString(),
                DEFAULT_INDEX_ID.toString()
            );
            assert.equal(wad4human(subs.unitsList[1]), "0.00020");
            await testExpectedBalances([
                [alice, toWad("100")],
                [bob, toWad("100")],
                [dan, toWad("0")],
            ]);

            // Alice distributes tokens (100 * 0.0001 = 0.01)
            idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "100",
            });
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.0001").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.99")],
                [bob, toWad("100")],
                [dan, toWad("0.01")],
            ]);

            // Bob distributes tokens (200 * 0.0002 = 0.04)
            idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "bob",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "200",
            });
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.0002").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.99")],
                [bob, toWad("99.96")],
                [dan, toWad("0.05")],
            ]);

            // Alice update Dan's subscription with more units
            sdata = await shouldUpdateSubscription({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "dan",
                units: toWad("0.0003").toString(),
            });
            assert.isTrue(sdata.approved);
            assert.equal(sdata.pendingDistribution.toString(), "0");
            idata = await ida.getIndex.call(
                superToken.address,
                alice,
                DEFAULT_INDEX_ID
            );
            assert.equal(idata.indexValue.toString(), "100");
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.0003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.99")],
                [bob, toWad("99.96")],
                [dan, toWad("0.05")],
            ]);

            // Alice distributes tokens again (100 * 0.0003 = 0.03)
            idata = await shouldUpdateIndex({
                testenv: t,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                indexValue: "200",
            });
            assert.equal(
                idata.totalUnitsApproved.toString(),
                toWad("0.0003").toString()
            );
            await testExpectedBalances([
                [alice, toWad("99.96")],
                [bob, toWad("99.96")],
                [dan, toWad("0.08")],
            ]);

            await t.validateSystemInvariance();
        });
    });
});
