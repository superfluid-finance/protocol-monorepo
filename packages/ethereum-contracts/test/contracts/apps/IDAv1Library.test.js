const TestEnvironment = require("../../TestEnvironment");

const {web3tx} = require("@decentral.ee/web3-helpers");
const {
    constants: {ZERO_ADDRESS},
} = require("@openzeppelin/test-helpers");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const IDAv1LibraryMock = artifacts.require("IDAv1LibraryMock");
const IDAv1LibrarySuperAppMock = artifacts.require("IDAv1LibrarySuperAppMock");

describe("IDAv1Library testing", function () {
    this.timeout(300e3);

    // HELPERS

    // enum simulatinator.
    // This is used in the IDAv1LibrarySuperAppMock for checking all functions.
    const FunctionIndex = {
        CREATE_INDEX: 0,
        CREATE_INDEX_USER_DATA: 1,
        UPDATE_INDEX: 2,
        UPDATE_INDEX_USER_DATA: 3,
        DISTRIBUTE: 4,
        DISTRIBUTE_USER_DATA: 5,
        APROVE_SUBSCRIPTION: 6,
        APROVE_SUBSCRIPTION_USER_DATA: 7,
        REVOKE_SUBSCRIPTION: 8,
        REVOKE_SUBSCRIPTION_USER_DATA: 9,
        UPDATE_SUBSCRIPTION: 10,
        UPDATE_SUBSCRIPTION_USER_DATA: 11,
        DELETE_SUBSCRIPTION: 12,
        DELETE_SUBSCRIPTION_USER_DATA: 13,
        CLAIM: 14,
        CLAIM_USER_DATA: 15,
    };

    const toBytes = (text) => web3.eth.abi.encodeParameter("string", text);

    const userData = (
        functionIndex,
        indexId,
        publisher = ZERO_ADDRESS,
        subscriber = ZERO_ADDRESS,
        units = 0
    ) =>
        web3.eth.abi.encodeParameters(
            ["uint8", "uint32", "address", "address", "uint128"],
            [functionIndex, indexId, publisher, subscriber, units]
        );

    // TEST SET UP
    const env = TestEnvironment.getSingleton();

    const INDEX_ID = 0;

    let superToken, host, ida, alice, bob, idav1LibMock, idav1LibSuperAppMock;

    before(async () => {
        await env.beforeTestSuite({isTruffle: true, nAccounts: 4});

        ida = env.contracts.ida;
        host = env.contracts.superfluid;
        ({alice, bob} = env.aliases);
        superToken = await SuperTokenMock.at(env.sf.tokens.TESTx.address);

        await superToken.mintInternal(
            alice,
            web3.utils.toWei("100000", "ether"),
            "0x",
            "0x"
        );
    });

    beforeEach(async () => {
        idav1LibMock = await IDAv1LibraryMock.new(host.address);
        idav1LibSuperAppMock = await IDAv1LibrarySuperAppMock.new(host.address);
        await superToken.transfer(
            idav1LibMock.address,
            web3.utils.toWei("10", "ether"),
            {from: alice}
        );
        await superToken.transfer(
            idav1LibSuperAppMock.address,
            web3.utils.toWei("10", "ether"),
            {from: alice}
        );
    });

    describe("#1 - Non-Callback Index Operations", async function () {
        it("#1.1 - create index", async () => {
            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID
                    )
                ).exist,
                true
            );
        });

        it("#1.2 - create index with user data", async () => {
            await web3tx(
                idav1LibMock.createIndexWithUserDataTest,
                "Alice create index with user data"
            )(superToken.address, INDEX_ID, toBytes("oh hello"), {from: alice});

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID
                    )
                ).exist,
                true
            );
        });

        it("#1.3 - update index value", async () => {
            const indexValue = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateIndexValueTest,
                "Alice updates index value"
            )(superToken.address, INDEX_ID, indexValue, {from: alice});

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                indexValue
            );
        });

        it("#1.4 - update index value with user data", async () => {
            const indexValue = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateIndexValueWithUserDataTest,
                "Alice updates index value with user data"
            )(superToken.address, INDEX_ID, indexValue, toBytes("oh hello"), {
                from: alice,
            });

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                indexValue
            );
        });

        it("#1.5 - distribute", async () => {
            const distribution = 1;
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice issues units to Bob"
            )(superToken.address, INDEX_ID, bob, units);

            await web3tx(idav1LibMock.distributeTest, "Alice distributes")(
                superToken.address,
                INDEX_ID,
                distribution,
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                distribution
            );
        });

        it("#1.6 - distribute with user data", async () => {
            const distribution = 1;
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice issues units to Bob"
            )(superToken.address, INDEX_ID, bob, units);

            await web3tx(
                idav1LibMock.distributeWithUserDataTest,
                "Alice distributes with user data"
            )(superToken.address, INDEX_ID, distribution, toBytes("oh hello"), {
                from: alice,
            });

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                distribution
            );
        });
    });

    describe("#2 - Non-Callback Subscription Operations", async function () {
        it("#2.1 - approve subscription", async () => {
            // must create externally to test againt mock contract
            await web3tx(host.callAgreement, "Alice creates index")(
                ida.address,
                ida.contract.methods
                    .createIndex(superToken.address, INDEX_ID, "0x")
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await web3tx(
                idav1LibMock.approveSubscriptionTest,
                "Bob approves subscription"
            )(superToken.address, alice, INDEX_ID, {from: bob});

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        alice,
                        INDEX_ID,
                        idav1LibMock.address
                    )
                ).approved,
                true
            );
        });

        it("#2.2 - approve subscription with user data", async () => {
            await web3tx(
                idav1LibMock.approveSubscriptionWithUserDataTest,
                "Bob approves subscription with user data"
            )(superToken.address, alice, INDEX_ID, toBytes("oh hello"), {
                from: bob,
            });

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        alice,
                        INDEX_ID,
                        idav1LibMock.address
                    )
                ).approved,
                true
            );
        });

        it("#2.3 - revoke subscription", async () => {
            await web3tx(
                idav1LibMock.approveSubscriptionTest,
                "Bob approves subscription"
            )(superToken.address, alice, INDEX_ID, {from: bob});

            await web3tx(
                idav1LibMock.revokeSubscriptionTest,
                "Bob revokes subcription"
            )(superToken.address, alice, INDEX_ID, {from: bob});

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        alice,
                        INDEX_ID,
                        idav1LibMock.address
                    )
                ).approved,
                false
            );
        });

        it("#2.4 - revoke subscription with user data", async () => {
            await web3tx(
                idav1LibMock.approveSubscriptionTest,
                "Bob approves subscription"
            )(superToken.address, alice, INDEX_ID, {from: bob});

            await web3tx(
                idav1LibMock.revokeSubscriptionWithUserDataTest,
                "Bob revokes subcription with user data"
            )(superToken.address, alice, INDEX_ID, toBytes("oh hello"), {
                from: bob,
            });

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        alice,
                        INDEX_ID,
                        idav1LibMock.address
                    )
                ).approved,
                false
            );
        });

        it("#2.5 - update subscription units", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice updates Bob's subscription"
            )(superToken.address, INDEX_ID, bob, units, {from: alice});

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                units
            );
        });

        it("#2.6 - update subscription with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "Alice updates Bob's subscription with user data"
            )(superToken.address, INDEX_ID, bob, units, toBytes("oh hello"), {
                from: alice,
            });

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                units
            );
        });

        it("#2.7 - delete subscription", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice updates Bob's subscription"
            )(superToken.address, INDEX_ID, bob, units, {from: alice});

            await web3tx(
                idav1LibMock.deleteSubscriptionTest,
                "Alice deletes Bob's subscription"
            )(superToken.address, idav1LibMock.address, INDEX_ID, bob, {
                from: bob,
            });

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                0
            );
        });

        it("#2.8 - delete subscription with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice updates Bob's subscription"
            )(superToken.address, INDEX_ID, bob, units, {from: alice});

            await web3tx(
                idav1LibMock.deleteSubscriptionWithUserDataTest,
                "Alice deletes Bob's subscription"
            )(
                superToken.address,
                idav1LibMock.address,
                INDEX_ID,
                bob,
                toBytes("oh hello"),
                {
                    from: bob,
                }
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                0
            );
        });

        it("#2.9 - claim", async () => {
            const units = 1;

            await web3tx(
                host.callAgreement,
                "Alice updates subscription units"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        INDEX_ID,
                        idav1LibMock.address,
                        units,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await web3tx(idav1LibMock.claimTest, "Bob claims pending units")(
                superToken.address,
                alice,
                INDEX_ID,
                idav1LibMock.address,
                {from: bob}
            );

            const subscription = await ida.getSubscription(
                superToken.address,
                alice,
                INDEX_ID,
                idav1LibMock.address
            );

            assert.equal(subscription.units.toNumber(), units);
            assert.equal(subscription.pendingDistribution.toNumber(), 0);
        });

        it("#2.10 - claim with user data", async () => {
            const units = 1;

            await web3tx(
                host.callAgreement,
                "Alice updates subscription units"
            )(
                ida.address,
                ida.contract.methods
                    .updateSubscription(
                        superToken.address,
                        INDEX_ID,
                        idav1LibMock.address,
                        units,
                        "0x"
                    )
                    .encodeABI(),
                "0x",
                {from: alice}
            );

            await web3tx(
                idav1LibMock.claimWithUserDataTest,
                "Bob claims pending units"
            )(
                superToken.address,
                alice,
                INDEX_ID,
                idav1LibMock.address,
                toBytes("oh hello"),
                {from: bob}
            );

            const subscription = await ida.getSubscription(
                superToken.address,
                alice,
                INDEX_ID,
                idav1LibMock.address
            );

            assert.equal(subscription.units.toNumber(), units);
            assert.equal(subscription.pendingDistribution.toNumber(), 0);
        });
    });

    describe("#3 - View Operations", async function () {
        it("#3.1 - get index", async () => {
            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            const index = await ida.getIndex(
                superToken.address,
                idav1LibMock.address,
                INDEX_ID
            );

            const libIndex = await idav1LibMock.getIndexTest(
                superToken.address,
                idav1LibMock.address,
                INDEX_ID
            );

            assert.equal(index.exist, libIndex.exist);
            assert.equal(
                index.indexValue.toString(),
                libIndex.indexValue.toString()
            );
            assert.equal(
                index.totalUnitsApproved.toString(),
                libIndex.totalUnitsApproved.toString()
            );
            assert.equal(
                index.totalUnitsPending.toString(),
                libIndex.totalUnitsPending.toString()
            );
        });

        it("#3.2 - calculate distribution", async () => {
            const amount = 1;
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice updates subscription units"
            )(superToken.address, INDEX_ID, bob, units);

            const distribution = await ida.calculateDistribution(
                superToken.address,
                idav1LibMock.address,
                INDEX_ID,
                amount
            );
            const distributionLib =
                await idav1LibMock.calculateDistributionTest(
                    superToken.address,
                    idav1LibMock.address,
                    INDEX_ID,
                    amount
                );

            assert.equal(
                distribution.actualAmount.toString(),
                distributionLib.actualAmount.toString()
            );
            assert.equal(
                distribution.newIndexValue.toString(),
                distributionLib.newIndexValue.toString()
            );
        });

        it("#3.3 - list subscriptions", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice updates subscription units"
            )(superToken.address, INDEX_ID, bob, units);

            const subscriptions = ida.listSubscriptions(
                superToken.address,
                bob
            );
            const subscriptionsLib = idav1LibMock.listSubscriptionsTest(
                superToken.address,
                bob
            );

            assert.equal(subscriptions.publishers, subscriptionsLib.publishers);
            assert.equal(subscriptions.indexIds, subscriptionsLib.indexIds);
            assert.equal(subscriptions.unitsList, subscriptionsLib.unitsList);
        });

        it("#3.4 - get susbcription", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice updates subscription units"
            )(superToken.address, INDEX_ID, bob, units);

            const subscription = ida.getSubscription(
                superToken.address,
                idav1LibMock.address,
                INDEX_ID,
                bob
            );
            const subscriptionLib = idav1LibMock.getSubscriptionTest(
                superToken.address,
                idav1LibMock.address,
                INDEX_ID,
                bob
            );

            assert.equal(subscription.exist, subscriptionLib.exist);
            assert.equal(subscription.approved, subscriptionLib.approved);
            assert.equal(subscription.units, subscriptionLib.units);
            assert.equal(
                subscription.pendingDistribution,
                subscriptionLib.pendingDistribution
            );
        });

        it("#3.4 - get susbcription by id", async () => {
            const units = 1;

            const publisherId = web3.utils.soliditySha3(
                {t: "string", v: "publisher"},
                {t: "address", v: idav1LibMock.address},
                {t: "uint32", v: INDEX_ID}
            );
            const subscriptionId = web3.utils.soliditySha3(
                {t: "string", v: "subscription"},
                {t: "address", v: bob},
                {t: "bytes32", v: publisherId}
            );

            await web3tx(idav1LibMock.createIndexTest, "Alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsTest,
                "Alice updates subscription units"
            )(superToken.address, INDEX_ID, bob, units);

            // await web3tx(host.callAgreement, "bob approves the index")(
            //     ida.address,
            //     ida.contract.methods
            //         .approveSubscription(
            //             superToken.address,
            //             idav1LibMock.address,
            //             INDEX_ID,
            //             "0x"
            //         )
            //         .encodeABI(),
            //     "0x",
            //     {from: bob}
            // );

            const subscription = ida.getSubscriptionByID(
                superToken.address,
                subscriptionId
            );
            const subscriptionLib = idav1LibMock.getSubscriptionByIDTest(
                superToken.address,
                subscriptionId
            );

            assert.equal(subscription.exist, subscriptionLib.exist);
            assert.equal(subscription.approved, subscriptionLib.approved);
            assert.equal(subscription.units, subscriptionLib.units);
            assert.equal(
                subscription.pendingDistribution,
                subscriptionLib.pendingDistribution
            );
        });
    });

    // CALLBACK TESTS
    // These tests pass `userData` to be extracted and used inside a super app callback.
    describe("#4 - Callback Index Operations", async function () {
        it("#4.1 - create index in callback", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(FunctionIndex.CREATE_INDEX, INDEX_ID),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID
                    )
                ).exist,
                true
            );
        });

        it("#4.2 - create index in callback with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(FunctionIndex.CREATE_INDEX_USER_DATA, INDEX_ID),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID
                    )
                ).exist,
                true
            );
        });

        it("#4.3 - update index in callback", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.createIndexTest,
                "super app creates index"
            )(superToken.address, INDEX_ID);

            await web3tx(
                idav1LibSuperAppMock.updateSubscriptionUnitsTest,
                "super app adds subscription to bob"
            )(superToken.address, INDEX_ID, bob, units);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.UPDATE_INDEX,
                    INDEX_ID,
                    undefined,
                    undefined,
                    units
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                units
            );
        });

        it("#4.4 - update index in callback with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.createIndexTest,
                "super app creates index"
            )(superToken.address, INDEX_ID);

            await web3tx(
                idav1LibSuperAppMock.updateSubscriptionUnitsTest,
                "super app adds subscription to bob"
            )(superToken.address, INDEX_ID, bob, units);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.UPDATE_INDEX_USER_DATA,
                    INDEX_ID,
                    undefined,
                    undefined,
                    units
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                units
            );
        });

        it("#4.5 - distribute in callback", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.createIndexTest,
                "super app creates index"
            )(superToken.address, INDEX_ID);

            await web3tx(
                idav1LibSuperAppMock.updateSubscriptionUnitsTest,
                "super app adds subscription to bob"
            )(superToken.address, INDEX_ID, bob, units);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.DISTRIBUTE,
                    INDEX_ID,
                    undefined,
                    undefined,
                    units
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                units
            );
        });

        it("#4.6 - distribute in callback with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.createIndexTest,
                "super app creates index"
            )(superToken.address, INDEX_ID);

            await web3tx(
                idav1LibSuperAppMock.updateSubscriptionUnitsTest,
                "super app adds subscription to bob"
            )(superToken.address, INDEX_ID, bob, units);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.DISTRIBUTE_USER_DATA,
                    INDEX_ID,
                    undefined,
                    undefined,
                    units
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                units
            );
        });

        it("#4.7 - approve subscription in callback", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.APROVE_SUBSCRIPTION,
                    INDEX_ID,
                    idav1LibMock.address
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID
                    )
                ).totalUnitsApproved.toNumber(),
                units
            );
        });

        it("#4.8 - approve subscription in callback with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.APROVE_SUBSCRIPTION_USER_DATA,
                    INDEX_ID,
                    idav1LibMock.address
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID
                    )
                ).totalUnitsApproved.toNumber(),
                units
            );
        });

        it("#4.9 - revoke subscription in callback", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.approveSubscriptionTest,
                "super app approves subscription"
            )(superToken.address, idav1LibMock.address, INDEX_ID);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.REVOKE_SUBSCRIPTION,
                    INDEX_ID,
                    idav1LibMock.address
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID,
                        idav1LibSuperAppMock.address
                    )
                ).approved,
                false
            );
        });

        it("#4.10 - revoke subscription in callback with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.approveSubscriptionTest,
                "super app approves subscription"
            )(superToken.address, idav1LibMock.address, INDEX_ID);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.REVOKE_SUBSCRIPTION_USER_DATA,
                    INDEX_ID,
                    idav1LibMock.address
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID,
                        idav1LibSuperAppMock.address
                    )
                ).approved,
                false
            );
        });

        it("#4.11 - update subscription units in callback", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.createIndexTest,
                "super app creates index"
            )(superToken.address, INDEX_ID);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.UPDATE_SUBSCRIPTION,
                    INDEX_ID,
                    undefined,
                    idav1LibMock.address,
                    units
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID,
                        idav1LibMock.address
                    )
                ).units.toNumber(),
                units
            );
        });

        it("#4.12 - update subscription units in callback with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.createIndexTest,
                "super app creates index"
            )(superToken.address, INDEX_ID);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.UPDATE_SUBSCRIPTION_USER_DATA,
                    INDEX_ID,
                    undefined,
                    idav1LibMock.address,
                    units
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID,
                        idav1LibMock.address
                    )
                ).units.toNumber(),
                units
            );
        });

        it("#4.13 - delete subscription in callback", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.createIndexTest,
                "super app creates index"
            )(superToken.address, INDEX_ID);

            await web3tx(
                idav1LibSuperAppMock.updateSubscriptionUnitsTest,
                "super app issues units to bob"
            )(superToken.address, INDEX_ID, bob, units);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.DELETE_SUBSCRIPTION,
                    INDEX_ID,
                    idav1LibSuperAppMock.address,
                    bob
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                0
            );
        });

        it("#4.14 - delete subscription in callback with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibSuperAppMock.createIndexTest,
                "super app creates index"
            )(superToken.address, INDEX_ID);

            await web3tx(
                idav1LibSuperAppMock.updateSubscriptionUnitsTest,
                "super app issues units to bob"
            )(superToken.address, INDEX_ID, bob, units);

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.DELETE_SUBSCRIPTION,
                    INDEX_ID,
                    idav1LibSuperAppMock.address,
                    bob
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibSuperAppMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                0
            );
        });

        it("#4.15 - claim in callback", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.CLAIM,
                    INDEX_ID,
                    idav1LibMock.address,
                    idav1LibSuperAppMock.address
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID,
                        idav1LibSuperAppMock.address
                    )
                ).pendingDistribution.toNumber(),
                0
            );
        });

        it("#4.15 - claim in callback with user data", async () => {
            const units = 1;

            await web3tx(idav1LibMock.createIndexTest, "alice creates index")(
                superToken.address,
                INDEX_ID,
                {from: alice}
            );

            await web3tx(
                idav1LibMock.updateSubscriptionUnitsWithUserDataTest,
                "alice triggers callback on super app"
            )(
                superToken.address,
                INDEX_ID,
                idav1LibSuperAppMock.address,
                units,
                userData(
                    FunctionIndex.CLAIM_USER_DATA,
                    INDEX_ID,
                    idav1LibMock.address,
                    idav1LibSuperAppMock.address
                ),
                {from: alice}
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        idav1LibMock.address,
                        INDEX_ID,
                        idav1LibSuperAppMock.address
                    )
                ).pendingDistribution.toNumber(),
                0
            );
        });
    });
});
