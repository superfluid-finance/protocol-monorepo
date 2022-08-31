const TestEnvironment = require("../../TestEnvironment");
const {expectCustomError} = require("../../utils/expectRevert");

const {wad4human} = require("@decentral.ee/web3-helpers");
const {toBN, toWad} = require("../utils/helpers");

const {
    shouldCreateFlow,
    expectNetFlow,
} = require("../agreements/ConstantFlowAgreementV1.behavior.js");
const CFADataModel = require("../agreements/ConstantFlowAgreementV1.data.js");

const {
    shouldCreateIndex,
    shouldDistribute,
    shouldApproveSubscription,
    shouldUpdateSubscription,
    shouldDeleteSubscription,
} = require("../agreements/InstantDistributionAgreementV1.behaviour.js");
const {ethers} = require("hardhat");

const DEFAULT_INDEX_ID = "42";

describe("Superfluid scenarios", function () {
    this.timeout(300e3);

    const t = TestEnvironment.getSingleton();
    const {FLOW_RATE1, INIT_BALANCE} = t.configs;

    let alice, bob, carol, dan;
    let superToken;

    before(async function () {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({alice, bob, carol, dan} = t.aliases);

        superToken = t.sf.tokens.TESTx;
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    async function verifyAll(opts) {
        const cfaDataModel = new CFADataModel(t, superToken);
        const block2 = await web3.eth.getBlock("latest");
        await t.validateExpectedBalances(() => {
            cfaDataModel.syncAccountExpectedBalanceDeltas({
                testenv: t,
                superToken: superToken.address,
                timestamp: block2.timestamp,
            });
        });
        await t.validateSystemInvariance(opts);
    }

    async function timeTravelOnceAndVerifyAll(opts = {}) {
        await t.timeTravelOnce(opts.time);
        await verifyAll(opts);
    }

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

    context("#1.x Multi-accounts CFA scenarios", () => {
        it("#1.1 two accounts sending to each other with the same flow rate", async () => {
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "alice",
                value: FLOW_RATE1.mul(toBN(0).sub(toBN(1))),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "bob",
                value: FLOW_RATE1,
            });
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "bob",
                receiver: "alice",
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "alice",
                value: "0",
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "bob",
                value: "0",
            });
            await timeTravelOnceAndVerifyAll();
        });

        it("#1.2 three accounts forming a flow loop", async () => {
            // alice -> bob -> carol
            //   ^---------------|
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            const flowRateBC = FLOW_RATE1.mul(2).div(3);
            const flowRateCA = FLOW_RATE1.div(3);

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "alice",
                value: toBN(0).sub(FLOW_RATE1),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "bob",
                value: FLOW_RATE1,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "carol",
                value: "0",
            });
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "bob",
                receiver: "carol",
                flowRate: flowRateBC,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "alice",
                value: toBN(0).sub(FLOW_RATE1),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "bob",
                value: FLOW_RATE1.sub(flowRateBC),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "carol",
                value: flowRateBC,
            });
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "carol",
                receiver: "alice",
                flowRate: flowRateCA,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "alice",
                value: toBN(flowRateCA).sub(FLOW_RATE1),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "bob",
                value: FLOW_RATE1.sub(flowRateBC),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "carol",
                value: flowRateBC.sub(flowRateCA),
            });
            await timeTravelOnceAndVerifyAll();
        });

        it("#1.3 a slight complex flow map", async () => {
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE.mul(2));

            const flowRateBD = FLOW_RATE1.mul(2).div(3);
            const flowRateDC = FLOW_RATE1.div(3);
            //const flowRate;

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "alice",
                receiver: "carol",
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "alice",
                value: toBN(0).sub(FLOW_RATE1.mul(2)),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "bob",
                value: FLOW_RATE1,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "carol",
                value: FLOW_RATE1,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "dan",
                value: "0",
            });
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "bob",
                receiver: "dan",
                flowRate: flowRateBD,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "alice",
                value: toBN(0).sub(FLOW_RATE1.mul(2)),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "bob",
                value: FLOW_RATE1.sub(flowRateBD),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "carol",
                value: FLOW_RATE1,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "dan",
                value: flowRateBD,
            });
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "dan",
                receiver: "carol",
                flowRate: flowRateDC,
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "alice",
                value: toBN(0).sub(FLOW_RATE1.mul(2)),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "bob",
                value: FLOW_RATE1.sub(flowRateBD),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "carol",
                value: FLOW_RATE1.add(flowRateDC),
            });
            await expectNetFlow({
                testenv: t,
                superToken,
                account: "dan",
                value: flowRateBD.sub(flowRateDC),
            });
            await timeTravelOnceAndVerifyAll();
        });
    });

    context("#2.x Multi-accounts IDA Scenarios", () => {
        it("#2.1 1to3 distribution scenario", async () => {
            await t.upgradeBalance("alice", INIT_BALANCE);

            await shouldCreateIndex({
                testenv: t,
                superToken,
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
                        superToken,
                        publisherName: "alice",
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: subscriberName,
                    });
                }

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
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
                    assert.equal(subs.length, 1);
                    assert.equal(subs[0].publisher, alice);
                    assert.equal(subs[0].indexId, DEFAULT_INDEX_ID);
                    assert.equal(subs[0].units, subscriptionUnits.toString());
                } else {
                    assert.equal(subs.length, 0);
                }
            }

            await shouldDistribute({
                testenv: t,
                superToken,
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
                superToken,
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
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "dan",
                senderName: "alice",
            });
            await testExpectedBalances([
                [alice, toWad("99.82")],
                [bob, toWad("0.03")],
                [carol, toWad("0.06")],
                [dan, toWad("0.09")],
            ]);

            await shouldDistribute({
                testenv: t,
                superToken,
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

        it("#2.2 2to1 distribution scenario", async () => {
            await t.upgradeBalance("alice", INIT_BALANCE);
            await t.upgradeBalance("bob", INIT_BALANCE);

            // alice and bob create indices and dan subscribes to them
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
                    superToken,
                    publisherName,
                    indexId: DEFAULT_INDEX_ID,
                });

                if (doApprove) {
                    await shouldApproveSubscription({
                        testenv: t,
                        superToken,
                        publisherName,
                        indexId: DEFAULT_INDEX_ID,
                        subscriberName: "dan",
                    });
                }

                await shouldUpdateSubscription({
                    testenv: t,
                    superToken,
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
            assert.equal(subs.length, 1);
            assert.equal(subs[0].publisher, alice);
            assert.equal(subs[0].indexId, DEFAULT_INDEX_ID);
            assert.equal(wad4human(subs[0].units), "0.00010");

            // Alice distributes tokens (100 * 0.0001 = 0.01)
            await shouldDistribute({
                testenv: t,
                superToken,
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
                superToken,
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
                superToken,
                publisherName: "alice",
                indexId: DEFAULT_INDEX_ID,
                subscriberName: "dan",
                units: toWad("0.0003").toString(),
            });

            // Alice distributes tokens again (100 * 0.0003 = 0.03)
            await shouldDistribute({
                testenv: t,
                superToken,
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
                superToken,
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

    context("#3.x Special CFA+IDA Scenarios", () => {
        it("#3.1 distributions should not dip into the flow deposit", async () => {
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

            await t.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: FLOW_RATE1.toString(),
            });

            await t.timeTravelOnce();

            await expectCustomError(
                t.agreementHelper.callAgreement({
                    agreementAddress: t.contracts.ida.address,
                    callData: t.agreementHelper.getIDACallData("distribute", [
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        toWad(75).toString(),
                        "0x",
                    ]),
                    signer: await ethers.getSigner(alice),
                }),
                t.contracts.ida,
                "INSUFFICIENT_BALANCE",
                t.customErrorCode.IDA_INSUFFICIENT_BALANCE
            );
        });

        it("#3.2 receiving a distribution with an incoming stream", async () => {
            await t.upgradeBalance("alice", INIT_BALANCE);
            await t.upgradeBalance("bob", INIT_BALANCE);

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

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "bob",
                receiver: "alice",
                flowRate: FLOW_RATE1,
            });
            await t.timeTravelOnce();
            await t.validateSystemInvariance();

            await t.sf.ida.distribute({
                superToken: superToken.address,
                publisher: alice,
                indexId: DEFAULT_INDEX_ID,
                amount: toWad(20).toString(),
            });
            await t.timeTravelOnce();
            await t.validateSystemInvariance();
        });

        it("#3.3 critical user receiver receiving distribution and not being critical anymore", async () => {
            await t.upgradeBalance("bob", toWad(23));

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

            await t.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: bob,
                receiver: alice,
                flowRate: FLOW_RATE1.toString(),
            });

            await t.timeTravelOnce();

            assert.isTrue(await superToken.isAccountCriticalNow(bob));

            await t.sf.ida.distribute({
                superToken: superToken.address,
                publisher: alice,
                indexId: DEFAULT_INDEX_ID,
                amount: toWad(20).toString(),
            });

            assert.isFalse(await superToken.isAccountCriticalNow(bob));
        });

        it("#3.4 not enough balance for distribution -> receive stream -> distribute", async () => {
            await t.upgradeBalance("bob", INIT_BALANCE);
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

            await expectCustomError(
                t.agreementHelper.callAgreement({
                    agreementAddress: t.contracts.ida.address,
                    callData: t.agreementHelper.getIDACallData("distribute", [
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        toWad(24).toString(),
                        "0x",
                    ]),
                    signer: await ethers.getSigner(alice),
                }),
                t.contracts.ida,
                "INSUFFICIENT_BALANCE",
                t.customErrorCode.IDA_INSUFFICIENT_BALANCE
            );

            await t.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: bob,
                receiver: alice,
                flowRate: FLOW_RATE1.toString(),
            });

            await t.timeTravelOnce();

            await t.sf.ida.distribute({
                superToken: superToken.address,
                publisher: alice,
                indexId: DEFAULT_INDEX_ID,
                amount: toWad(20).toString(),
            });
        });

        it("#3.5 distributing with an outgoing stream", async () => {
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

            await t.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: FLOW_RATE1.toString(),
            });
            await t.timeTravelOnce();
            await t.validateSystemInvariance();

            await t.sf.ida.distribute({
                superToken: superToken.address,
                publisher: alice,
                indexId: DEFAULT_INDEX_ID,
                amount: toWad(20).toString(),
            });
            await t.timeTravelOnce();
            await t.validateSystemInvariance();
        });
    });
});
