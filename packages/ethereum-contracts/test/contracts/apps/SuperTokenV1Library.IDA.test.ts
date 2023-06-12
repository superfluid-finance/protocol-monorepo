import {SignerWithAddress} from "@nomiclabs/hardhat-ethers/signers";
import {assert, ethers, expect, web3} from "hardhat";

import {
    InstantDistributionAgreementV1,
    SuperfluidMock,
    SuperTokenLibraryIDAMock,
    SuperTokenLibraryIDASuperAppMock,
    SuperTokenMock,
} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";

const ZERO_ADDRESS = ethers.constants.AddressZero;

describe("IDAv1Library testing", function () {
    this.timeout(300e3);

    // HELPERS

    // enum simulator.
    // This is used in the IDAv1LibrarySuperAppMock for checking all functions.
    const FunctionIndex = {
        CREATE_INDEX: 0,
        CREATE_INDEX_USER_DATA: 1,
        UPDATE_INDEX: 2,
        UPDATE_INDEX_USER_DATA: 3,
        DISTRIBUTE: 4,
        DISTRIBUTE_USER_DATA: 5,
        APPROVE_SUBSCRIPTION: 6,
        APPROVE_SUBSCRIPTION_USER_DATA: 7,
        REVOKE_SUBSCRIPTION: 8,
        REVOKE_SUBSCRIPTION_USER_DATA: 9,
        UPDATE_SUBSCRIPTION: 10,
        UPDATE_SUBSCRIPTION_USER_DATA: 11,
        DELETE_SUBSCRIPTION: 12,
        DELETE_SUBSCRIPTION_USER_DATA: 13,
        CLAIM: 14,
        CLAIM_USER_DATA: 15,
    };

    const toBytes = (text: string) =>
        web3.eth.abi.encodeParameter("string", text);

    const userData = (
        functionIndex: number,
        indexId: number,
        publisher = ZERO_ADDRESS,
        subscriber = ZERO_ADDRESS,
        units = 0
    ) =>
        web3.eth.abi.encodeParameters(
            ["uint8", "uint32", "address", "address", "uint128"],
            [functionIndex, indexId, publisher, subscriber, units]
        );

    // TEST SET UP
    const t = TestEnvironment.getSingleton();

    const INDEX_ID = 0;

    let superToken: SuperTokenMock,
        host: SuperfluidMock,
        ida: InstantDistributionAgreementV1,
        alice: string,
        bob: string,
        superTokenLibIDAMock: SuperTokenLibraryIDAMock,
        superTokenLibIDASuperAppMock: SuperTokenLibraryIDASuperAppMock,
        aliceSigner: SignerWithAddress;

    before(async () => {
        await t.beforeTestSuite({isTruffle: true, nAccounts: 4});

        ida = t.contracts.ida;
        host = t.contracts.superfluid;
        ({alice, bob} = t.aliases);
        superToken = await ethers.getContractAt(
            "SuperTokenMock",
            t.tokens.SuperToken.address
        );

        await superToken.mintInternal(
            alice,
            ethers.utils.parseUnits("100000", "ether"),
            "0x",
            "0x"
        );
        aliceSigner = await ethers.getSigner(alice);
    });

    beforeEach(async function () {
        const superTokenLibIDAMockFactory = await ethers.getContractFactory(
            "SuperTokenLibraryIDAMock"
        );
        superTokenLibIDAMock = (
            await superTokenLibIDAMockFactory.deploy()
        ).connect(aliceSigner);
        const superTokenLibIDASuperAppMockFactory =
            await ethers.getContractFactory("SuperTokenLibraryIDASuperAppMock");
        superTokenLibIDASuperAppMock = (
            await superTokenLibIDASuperAppMockFactory.deploy(host.address)
        ).connect(aliceSigner);
        await superToken
            .connect(aliceSigner)
            .transfer(
                superTokenLibIDAMock.address,
                ethers.utils.parseUnits("10", "ether")
            );
        await superToken
            .connect(aliceSigner)
            .transfer(
                superTokenLibIDASuperAppMock.address,
                ethers.utils.parseUnits("10", "ether")
            );

        t.beforeEachTestCaseBenchmark(this);
    });

    afterEach(() => {
        t.afterEachTestCaseBenchmark();
    });

    describe("#1 - Non-Callback Index Operations", async function () {
        it("#1.1 - create index", async () => {
            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID
                    )
                ).exist,
                true
            );
        });

        it("#1.2 - create index with user data", async () => {
            console.log("Alice create index with user data");
            await superTokenLibIDAMock.createIndexWithUserDataTest(
                superToken.address,
                INDEX_ID,
                toBytes("oh hello")
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID
                    )
                ).exist,
                true
            );
        });

        it("#1.3 - update index value", async () => {
            const indexValue = 1;
            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("Alice updates index value");
            await superTokenLibIDAMock.updateIndexValueTest(
                superToken.address,
                INDEX_ID,
                indexValue
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                indexValue
            );
        });

        it("#1.4 - update index value with user data", async () => {
            const indexValue = 1;
            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("Alice updates index value with user data");
            await superTokenLibIDAMock.updateIndexValueWithUserDataTest(
                superToken.address,
                INDEX_ID,
                indexValue,
                toBytes("oh hello")
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                indexValue
            );
        });

        it("#1.5 - distribute", async () => {
            const distribution = 1;
            const units = 1;

            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("Alice issues units to Bob");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            console.log("Alice distributes");
            await superTokenLibIDAMock.distributeTest(
                superToken.address,
                INDEX_ID,
                distribution
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                distribution
            );
        });

        it("#1.6 - distribute with user data", async () => {
            const distribution = 1;
            const units = 1;

            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("Alice issues units to Bob");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );
            console.log("Alice distributes with user data");
            await superTokenLibIDAMock.distributeWithUserDataTest(
                superToken.address,
                INDEX_ID,
                distribution,
                toBytes("oh hello")
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                distribution
            );
        });

        it("#1.7 - _getHostAndIDA empty cache test", async () => {
            await superTokenLibIDAMock.listSubscriptionsTest(
                superToken.address,
                bob
            );
        });
    });

    describe("#2 - Non-Callback Subscription Operations", async function () {
        it("#2.1 - approve subscription", async () => {
            // must create externally to test against mock contract
            console.log("Alice creates index");
            await host
                .connect(aliceSigner)
                .callAgreement(
                    ida.address,
                    t.agreementHelper.idaInterface.encodeFunctionData(
                        "createIndex",
                        [superToken.address, INDEX_ID, "0x"]
                    ),
                    "0x"
                );

            console.log("Bob approves subscription");
            await superTokenLibIDAMock
                .connect(await ethers.getSigner(bob))
                .approveSubscriptionTest(superToken.address, alice, INDEX_ID);

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        alice,
                        INDEX_ID,
                        superTokenLibIDAMock.address
                    )
                ).approved,
                true
            );
        });

        it("#2.2 - approve subscription with user data", async () => {
            console.log("Bob approves subscription with user data");
            await superTokenLibIDAMock
                .connect(await ethers.getSigner(bob))
                .approveSubscriptionWithUserDataTest(
                    superToken.address,
                    alice,
                    INDEX_ID,
                    toBytes("oh hello")
                );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        alice,
                        INDEX_ID,
                        superTokenLibIDAMock.address
                    )
                ).approved,
                true
            );
        });

        it("#2.3 - revoke subscription", async () => {
            console.log("Bob approves subscription");
            await superTokenLibIDAMock.approveSubscriptionTest(
                superToken.address,
                alice,
                INDEX_ID
            );

            console.log("Bob revokes subscription");
            await superTokenLibIDAMock.revokeSubscriptionTest(
                superToken.address,
                alice,
                INDEX_ID
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        alice,
                        INDEX_ID,
                        superTokenLibIDAMock.address
                    )
                ).approved,
                false
            );
        });

        it("#2.4 - revoke subscription with user data", async () => {
            console.log("Bob approves subscription");
            await superTokenLibIDAMock.approveSubscriptionTest(
                superToken.address,
                alice,
                INDEX_ID
            );

            console.log("Bob revokes subscription with user data");
            await superTokenLibIDAMock.revokeSubscriptionWithUserDataTest(
                superToken.address,
                alice,
                INDEX_ID,
                toBytes("oh hello")
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        alice,
                        INDEX_ID,
                        superTokenLibIDAMock.address
                    )
                ).approved,
                false
            );
        });

        it("#2.5 - update subscription units", async () => {
            const units = 1;
            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("Alice updates Bob's subscription");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                units
            );
        });

        it("#2.6 - update subscription with user data", async () => {
            const units = 1;

            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("Alice updates Bob's subscription with user data");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                bob,
                units,
                toBytes("oh hello")
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                units
            );
        });

        it("#2.7 - delete subscription", async () => {
            const units = 1;
            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("Alice updates Bob's subscription");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            console.log("Alice deletes Bob's subscription");
            await superTokenLibIDAMock
                .connect(await ethers.getSigner(bob))
                .deleteSubscriptionTest(
                    superToken.address,
                    superTokenLibIDAMock.address,
                    INDEX_ID,
                    bob
                );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                0
            );
        });

        it("#2.8 - delete subscription with user data", async () => {
            const units = 1;
            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("Alice updates Bob's subscription");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            console.log("Alice deletes Bob's subscription");
            await superTokenLibIDAMock
                .connect(await ethers.getSigner(bob))
                .deleteSubscriptionWithUserDataTest(
                    superToken.address,
                    superTokenLibIDAMock.address,
                    INDEX_ID,
                    bob,
                    toBytes("oh hello")
                );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                0
            );
        });

        it("#2.9 - claim", async () => {
            const units = 1;

            console.log("Alice updates subscription units");
            await host
                .connect(aliceSigner)
                .callAgreement(
                    ida.address,
                    t.agreementHelper.idaInterface.encodeFunctionData(
                        "updateSubscription",
                        [
                            superToken.address,
                            INDEX_ID,
                            superTokenLibIDAMock.address,
                            units,
                            "0x",
                        ]
                    ),
                    "0x"
                );

            console.log("Bob claims pending units");
            await superTokenLibIDAMock
                .connect(await ethers.getSigner(bob))
                .claimTest(
                    superToken.address,
                    alice,
                    INDEX_ID,
                    superTokenLibIDAMock.address
                );

            const subscription = await ida.getSubscription(
                superToken.address,
                alice,
                INDEX_ID,
                superTokenLibIDAMock.address
            );

            assert.equal(subscription.units.toNumber(), units);
            assert.equal(subscription.pendingDistribution.toNumber(), 0);
        });

        it("#2.10 - claim with user data", async () => {
            const units = 1;
            console.log("Alice updates subscription units");
            await host
                .connect(aliceSigner)
                .callAgreement(
                    ida.address,
                    t.agreementHelper.idaInterface.encodeFunctionData(
                        "updateSubscription",
                        [
                            superToken.address,
                            INDEX_ID,
                            superTokenLibIDAMock.address,
                            units,
                            "0x",
                        ]
                    ),
                    "0x"
                );

            console.log("Bob claims pending units");
            await superTokenLibIDAMock
                .connect(await ethers.getSigner(bob))
                .claimWithUserDataTest(
                    superToken.address,
                    alice,
                    INDEX_ID,
                    superTokenLibIDAMock.address,
                    toBytes("oh hello")
                );

            const subscription = await ida.getSubscription(
                superToken.address,
                alice,
                INDEX_ID,
                superTokenLibIDAMock.address
            );

            assert.equal(subscription.units.toNumber(), units);
            assert.equal(subscription.pendingDistribution.toNumber(), 0);
        });
    });

    describe("#3 - View Operations", async function () {
        it("#3.1 - get index", async () => {
            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            const index = await ida.getIndex(
                superToken.address,
                superTokenLibIDAMock.address,
                INDEX_ID
            );

            const libIndex = await superTokenLibIDAMock.getIndexTest(
                superToken.address,
                superTokenLibIDAMock.address,
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

            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("Alice updates subscription units");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            const distribution = await ida.calculateDistribution(
                superToken.address,
                superTokenLibIDAMock.address,
                INDEX_ID,
                amount
            );
            const distributionLib =
                await superTokenLibIDAMock.calculateDistributionTest(
                    superToken.address,
                    superTokenLibIDAMock.address,
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

            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("Alice updates subscription units");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            const subscriptions = await ida.listSubscriptions(
                superToken.address,
                bob
            );
            const subscriptionsLib =
                await superTokenLibIDAMock.listSubscriptionsTest(
                    superToken.address,
                    bob
                );

            expect(subscriptions.publishers).to.eql(
                subscriptionsLib.publishers
            );
            expect(subscriptions.indexIds).to.eql(subscriptionsLib.indexIds);
            expect(subscriptions.unitsList).to.eql(subscriptionsLib.unitsList);
        });

        it("#3.4 - get subscription", async () => {
            const units = 1;

            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("Alice updates subscription units");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            const subscription = await ida.getSubscription(
                superToken.address,
                superTokenLibIDAMock.address,
                INDEX_ID,
                bob
            );
            const subscriptionLib =
                await superTokenLibIDAMock.getSubscriptionTest(
                    superToken.address,
                    superTokenLibIDAMock.address,
                    INDEX_ID,
                    bob
                );

            assert.equal(subscription.exist, subscriptionLib.exist);
            assert.equal(subscription.approved, subscriptionLib.approved);
            assert.equal(
                subscription.units.toString(),
                subscriptionLib.units.toString()
            );
            assert.equal(
                subscription.pendingDistribution.toString(),
                subscriptionLib.pendingDistribution.toString()
            );
        });

        it("#3.5 - get subscription by id", async () => {
            const units = 1;

            const publisherId = ethers.utils.solidityKeccak256(
                ["string", "address", "uint32"],
                ["publisher", superTokenLibIDAMock.address, INDEX_ID]
            );
            const subscriptionId = ethers.utils.solidityKeccak256(
                ["string", "address", "bytes32"],
                ["subscription", bob, publisherId]
            );

            console.log("Alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("Alice updates subscription units");
            await superTokenLibIDAMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            const subscription = await ida.getSubscriptionByID(
                superToken.address,
                subscriptionId
            );
            const subscriptionLib =
                await superTokenLibIDAMock.getSubscriptionByIDTest(
                    superToken.address,
                    subscriptionId
                );

            assert.equal(subscription.approved, subscriptionLib.approved);
            assert.equal(
                subscription.units.toString(),
                subscriptionLib.units.toString()
            );
            assert.equal(
                subscription.pendingDistribution.toString(),
                subscriptionLib.pendingDistribution.toString()
            );
        });
    });

    // CALLBACK TESTS
    // These tests pass `userData` to be extracted and used inside a super app callback.
    describe("#4 - Callback Index Operations", async function () {
        it("#4.1 - create index in callback", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(FunctionIndex.CREATE_INDEX, INDEX_ID)
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID
                    )
                ).exist,
                true
            );
        });

        it("#4.2 - create index in callback with user data", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(FunctionIndex.CREATE_INDEX_USER_DATA, INDEX_ID)
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID
                    )
                ).exist,
                true
            );
        });

        it("#4.3 - update index in callback", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("super app creates index");
            await superTokenLibIDASuperAppMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app adds subscription to bob");
            await superTokenLibIDASuperAppMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.UPDATE_INDEX,
                    INDEX_ID,
                    undefined,
                    undefined,
                    units
                )
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                units
            );
        });

        it("#4.4 - update index in callback with user data", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("super app creates index");
            await superTokenLibIDASuperAppMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app adds subscription to bob");
            await superTokenLibIDASuperAppMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.UPDATE_INDEX_USER_DATA,
                    INDEX_ID,
                    undefined,
                    undefined,
                    units
                )
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                units
            );
        });

        it("#4.5 - distribute in callback", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("super app creates index");
            await superTokenLibIDASuperAppMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app adds subscription to bob");
            await superTokenLibIDASuperAppMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.DISTRIBUTE,
                    INDEX_ID,
                    undefined,
                    undefined,
                    units
                )
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                units
            );
        });

        it("#4.6 - distribute in callback with user data", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("super app creates index");
            await superTokenLibIDASuperAppMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app adds subscription to bob");
            await superTokenLibIDASuperAppMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.DISTRIBUTE_USER_DATA,
                    INDEX_ID,
                    undefined,
                    undefined,
                    units
                )
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID
                    )
                ).indexValue.toNumber(),
                units
            );
        });

        it("#4.7 - approve subscription in callback", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.APPROVE_SUBSCRIPTION,
                    INDEX_ID,
                    superTokenLibIDAMock.address
                )
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID
                    )
                ).totalUnitsApproved.toNumber(),
                units
            );
        });

        it("#4.8 - approve subscription in callback with user data", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.APPROVE_SUBSCRIPTION_USER_DATA,
                    INDEX_ID,
                    superTokenLibIDAMock.address
                )
            );

            assert.equal(
                (
                    await ida.getIndex(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID
                    )
                ).totalUnitsApproved.toNumber(),
                units
            );
        });

        it("#4.9 - revoke subscription in callback", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app approves subscription");
            await superTokenLibIDASuperAppMock.approveSubscriptionTest(
                superToken.address,
                superTokenLibIDAMock.address,
                INDEX_ID
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.REVOKE_SUBSCRIPTION,
                    INDEX_ID,
                    superTokenLibIDAMock.address
                )
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID,
                        superTokenLibIDASuperAppMock.address
                    )
                ).approved,
                false
            );
        });

        it("#4.10 - revoke subscription in callback with user data", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app approves subscription");
            await superTokenLibIDASuperAppMock.approveSubscriptionTest(
                superToken.address,
                superTokenLibIDAMock.address,
                INDEX_ID
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.REVOKE_SUBSCRIPTION_USER_DATA,
                    INDEX_ID,
                    superTokenLibIDAMock.address
                )
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID,
                        superTokenLibIDASuperAppMock.address
                    )
                ).approved,
                false
            );
        });

        it("#4.11 - update subscription units in callback", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app creates index");
            await superTokenLibIDASuperAppMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.UPDATE_SUBSCRIPTION,
                    INDEX_ID,
                    undefined,
                    superTokenLibIDAMock.address,
                    units
                )
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID,
                        superTokenLibIDAMock.address
                    )
                ).units.toNumber(),
                units
            );
        });

        it("#4.12 - update subscription units in callback with user data", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app creates index");
            await superTokenLibIDASuperAppMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.UPDATE_SUBSCRIPTION_USER_DATA,
                    INDEX_ID,
                    undefined,
                    superTokenLibIDAMock.address,
                    units
                )
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID,
                        superTokenLibIDAMock.address
                    )
                ).units.toNumber(),
                units
            );
        });

        it("#4.13 - delete subscription in callback", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app creates index");
            await superTokenLibIDASuperAppMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("super app issues units to bob");
            await superTokenLibIDASuperAppMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.DELETE_SUBSCRIPTION,
                    INDEX_ID,
                    superTokenLibIDASuperAppMock.address,
                    bob
                )
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                0
            );
        });

        it("#4.14 - delete subscription in callback with user data", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("super app creates index");
            await superTokenLibIDASuperAppMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );

            console.log("super app issues units to bob");
            await superTokenLibIDASuperAppMock.updateSubscriptionUnitsTest(
                superToken.address,
                INDEX_ID,
                bob,
                units
            );

            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.DELETE_SUBSCRIPTION,
                    INDEX_ID,
                    superTokenLibIDASuperAppMock.address,
                    bob
                )
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDASuperAppMock.address,
                        INDEX_ID,
                        bob
                    )
                ).units.toNumber(),
                0
            );
        });

        it("#4.15 - claim in callback", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.CLAIM,
                    INDEX_ID,
                    superTokenLibIDAMock.address,
                    superTokenLibIDASuperAppMock.address
                )
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID,
                        superTokenLibIDASuperAppMock.address
                    )
                ).pendingDistribution.toNumber(),
                0
            );
        });

        it("#4.15 - claim in callback with user data", async () => {
            const units = 1;

            console.log("alice creates index");
            await superTokenLibIDAMock.createIndexTest(
                superToken.address,
                INDEX_ID
            );
            console.log("alice triggers callback on super app");
            await superTokenLibIDAMock.updateSubscriptionUnitsWithUserDataTest(
                superToken.address,
                INDEX_ID,
                superTokenLibIDASuperAppMock.address,
                units,
                userData(
                    FunctionIndex.CLAIM_USER_DATA,
                    INDEX_ID,
                    superTokenLibIDAMock.address,
                    superTokenLibIDASuperAppMock.address
                )
            );

            assert.equal(
                (
                    await ida.getSubscription(
                        superToken.address,
                        superTokenLibIDAMock.address,
                        INDEX_ID,
                        superTokenLibIDASuperAppMock.address
                    )
                ).pendingDistribution.toNumber(),
                0
            );
        });
    });
});
