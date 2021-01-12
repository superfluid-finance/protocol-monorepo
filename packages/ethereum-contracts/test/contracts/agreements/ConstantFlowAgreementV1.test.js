const { BN, expectRevert } = require("@openzeppelin/test-helpers");
const { web3tx, toWad, toBN } = require("@decentral.ee/web3-helpers");
const {
    clipDepositNumber,
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
    syncAccountExpectedBalanceDeltas
} = require("./ConstantFlowAgreementV1.behavior.js");

const TestEnvironment = require("../../TestEnvironment");

const traveler = require("ganache-time-traveler");
const MultiFlowApp = artifacts.require("MultiFlowApp");

const TEST_TRAVEL_TIME = 3600 * 24; // 24 hours

const FLOW_RATE1 = toWad("1").div(toBN(3600)); // 1 per hour
const MAXIMUM_FLOW_RATE = toBN(2)
    .pow(toBN(95))
    .sub(toBN(1));
const MINIMAL_DEPOSIT = toBN(1).shln(32);

contract("Using ConstantFlowAgreement v1", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 5), {
        isTruffle: true,
        useMocks: true
    });
    const { admin, alice, bob, dan } = t.aliases;
    const { ZERO_ADDRESS } = t.constants;
    const { LIQUIDATION_PERIOD } = t.configs;

    let superfluid;
    let governance;
    let cfa;
    let testToken;
    let superToken;

    before(async () => {
        await t.reset();
        ({ superfluid, governance, cfa } = t.contracts);
    });

    beforeEach(async function() {
        await t.resetForTestCase();
        await t.createNewToken();
        ({ testToken, superToken } = t.contracts);
    });

    async function _timeTravelOnce(time = TEST_TRAVEL_TIME) {
        const block1 = await web3.eth.getBlock("latest");
        console.log("current block time", block1.timestamp);
        console.log(`time traveler going to the future +${time}...`);
        await traveler.advanceTimeAndBlock(time);
        const block2 = await web3.eth.getBlock("latest");
        console.log("new block time", block2.timestamp);
    }

    async function verifyAll(opts) {
        const block2 = await web3.eth.getBlock("latest");
        await t.validateExpectedBalances(() => {
            syncAccountExpectedBalanceDeltas({
                testenv: t,
                superToken: superToken.address,
                timestamp: block2.timestamp
            });
        });
        await t.validateSystemInvariance(opts);
    }

    async function timeTravelOnceAndVerifyAll(opts = {}) {
        const time = opts.time || TEST_TRAVEL_TIME;
        await _timeTravelOnce(time);
        await verifyAll(opts);
    }

    async function expectNetFlow(alias, expectedNetFlowRate) {
        const actualNetFlowRate = await cfa.getNetFlow(
            superToken.address,
            t.getAddress(alias)
        );
        console.log(
            `expected net flow for ${alias}: ${expectedNetFlowRate.toString()}`
        );
        assert.equal(
            actualNetFlowRate.toString(),
            expectedNetFlowRate.toString(),
            `Unexpected net flow for ${alias}`
        );
    }

    async function expectJailed(appAddress, reasonCode) {
        assert.isTrue(await t.contracts.superfluid.isAppJailed(appAddress));
        const events = await superfluid.getPastEvents("Jail", {
            fromBlock: 0,
            toBlock: "latest",
            filter: {
                app: appAddress
            }
        });
        assert.equal(events.length, 1);
        assert.equal(events[0].args.reason.toString(), reasonCode.toString());
    }

    async function shouldTestLiquidations({
        titlePrefix,
        sender,
        receiver,
        by,
        allowCriticalAccount
    }) {
        const liquidationType =
            by === sender ? "liquidate by agent" : "self liquidate";

        it(`${titlePrefix}.1 should ${liquidationType} when critical but solvent`, async () => {
            assert.isFalse(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
            assert.isTrue(
                await superToken.isAccountSolventNow(t.aliases[sender])
            );
            // drain the balance until critical (60sec extra)
            await timeTravelOnceAndVerifyAll({
                time:
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() -
                    LIQUIDATION_PERIOD +
                    60,
                allowCriticalAccount: true
            });
            assert.isTrue(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
            assert.isTrue(
                await superToken.isAccountSolventNow(t.aliases[sender])
            );

            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver,
                by
            });

            await verifyAll();
        });

        it(`${titlePrefix}.2 should ${liquidationType} when insolvent`, async () => {
            assert.isFalse(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
            assert.isTrue(
                await superToken.isAccountSolventNow(t.aliases[sender])
            );
            // drain the balance until insolvent (60sec extra)
            await timeTravelOnceAndVerifyAll({
                time: t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() + 60,
                allowCriticalAccount: true
            });
            assert.isTrue(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
            assert.isFalse(
                await superToken.isAccountSolventNow(t.aliases[sender])
            );

            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver,
                by
            });

            await verifyAll({ allowCriticalAccount });
        });
    }

    context("#1 without callbacks", () => {
        const sender = "alice";
        const receiver = "bob";
        const agent = "dan";

        before(() => {
            console.log(`sender is ${sender} ${t.aliases[sender]}`);
            console.log(`receiver is ${receiver} ${t.aliases[receiver]}`);
        });

        describe("#1.1 createFlow", () => {
            it("#1.1.1 should create when there is enough balance", async () => {
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.1.2 should reject when there is not enough balance", async () => {
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: FLOW_RATE1.toString()
                    }),
                    "CFA: not enough available balance"
                );
            });

            it("#1.1.3 should reject when zero flow rate", async () => {
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: "0"
                    }),
                    "CFA: invalid flow rate"
                );
            });

            it("#1.1.4 should reject when negative flow rate", async () => {
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: "-1"
                    }),
                    "CFA: invalid flow rate"
                );
            });

            it("#1.1.5 should reject when self flow", async () => {
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[sender],
                        flowRate: FLOW_RATE1.toString()
                    }),
                    "CFA: no self flow"
                );
            });

            it("#1.1.6 should not create same flow", async () => {
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1
                });
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: FLOW_RATE1.toString()
                    }),
                    "CFA: flow already exist"
                );
            });

            it("#1.1.7 should reject when overflow flow rate", async () => {
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases.carol,
                        flowRate: MAXIMUM_FLOW_RATE.toString()
                    }),
                    "CFA: deposit overflow"
                );
            });

            it("#1.1.8 should reject when receiver is zero address", async () => {
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: ZERO_ADDRESS,
                        flowRate: FLOW_RATE1.toString()
                    }),
                    "CFA: receiver is zero"
                );
            });
        });

        describe("#1.2 updateFlow", () => {
            beforeEach(async () => {
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1
                });
            });

            it("#1.2.1 can maintain existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.2.2 can increase (+10%) existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10))
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.2.3 can decrease (-10%) existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(9)).div(toBN(10))
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.2.4 should not update with zero flow rate", async () => {
                await expectRevert(
                    t.sf.cfa.updateFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: "0"
                    }),
                    "CFA: invalid flow rate"
                );
            });

            it("#1.2.5 should not update with negative flow rate", async () => {
                await expectRevert(
                    t.sf.cfa.updateFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: "-1"
                    }),
                    "CFA: invalid flow rate"
                );
            });

            it("#1.2.6 should not update non existing flow", async () => {
                await expectRevert(
                    t.sf.cfa.updateFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[agent],
                        flowRate: FLOW_RATE1.toString()
                    }),
                    "CFA: flow does not exist"
                );
            });

            it("#1.2.7 should not update non existing flow (self flow)", async () => {
                await expectRevert(
                    t.sf.cfa.updateFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[sender],
                        flowRate: FLOW_RATE1.toString()
                    }),
                    "CFA: no self flow"
                );
            });

            it("#1.2.8 should reject when there is not enough balance", async () => {
                await expectRevert(
                    t.sf.cfa.updateFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: toBN(t.configs.INIT_BALANCE)
                            .div(toBN(LIQUIDATION_PERIOD).sub(toBN(60)))
                            .toString()
                    }),
                    "CFA: not enough available balance"
                );
            });

            it("#1.2.9 should reject when overflow flow rate", async () => {
                await expectRevert(
                    t.sf.cfa.updateFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: MAXIMUM_FLOW_RATE.toString()
                    }),
                    "CFA: deposit overflow"
                );
            });

            it("#1.2.10 should reject when receiver is zero address", async () => {
                await expectRevert(
                    t.sf.cfa.updateFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: ZERO_ADDRESS,
                        flowRate: FLOW_RATE1.toString()
                    }),
                    "CFA: receiver is zero"
                );
            });
        });

        describe("#1.3 deleteFlow (non liquidation)", () => {
            beforeEach(async () => {
                // give admin some balance for liquidations
                await t.upgradeBalance("admin", t.configs.INIT_BALANCE);
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);
                await t.upgradeBalance(agent, t.configs.INIT_BALANCE);
                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1
                });
            });

            it("#1.3.1.a can delete existing flow by sender", async () => {
                await shouldDeleteFlow({
                    testenv: t,
                    sender,
                    receiver,
                    by: sender
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.3.1.b can delete existing flow by receiver", async () => {
                await shouldDeleteFlow({
                    testenv: t,
                    sender,
                    receiver,
                    by: receiver
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.3.2 can delete an updated flow", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10))
                });
                await shouldDeleteFlow({
                    testenv: t,
                    sender,
                    receiver,
                    by: sender
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.3.3 should not delete non-existing flow", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[agent]
                    }),
                    "CFA: flow does not exist"
                );
            });

            it("#1.3.4 should reject when receiver is zero address", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: ZERO_ADDRESS
                    }),
                    "CFA: receiver is zero"
                );
            });

            it("#1.3.5 should reject when sender is zero address", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: ZERO_ADDRESS,
                        receiver: t.aliases[agent],
                        by: t.aliases[sender]
                    }),
                    "CFA: sender is zero"
                );
            });

            context("#1.3.6 with reward address as admin", () => {
                beforeEach(async () => {
                    await web3tx(
                        governance.setRewardAddress,
                        "set reward address to admin"
                    )(admin);
                });
                shouldTestLiquidations({
                    titlePrefix: "#1.3.6",
                    sender,
                    receiver,
                    by: sender
                });
            });

            context("#1.3.7 with zero reward address", () => {
                beforeEach(async () => {
                    await web3tx(
                        governance.setRewardAddress,
                        "set reward address to zero"
                    )(ZERO_ADDRESS);
                });
                shouldTestLiquidations({
                    titlePrefix: "#1.3.7",
                    sender,
                    receiver,
                    by: sender,
                    // no one will bail you out, alice :(
                    allowCriticalAccount: true
                });
            });
        });

        describe("#1.4 deleteFlow (liquidations)", () => {
            beforeEach(async () => {
                // give admin some balance for liquidations
                await t.upgradeBalance("admin", t.configs.INIT_BALANCE);
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);
                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1
                });
            });

            it("#1.4.1 should reject when sender account is not critical", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        by: t.aliases[agent]
                    }),
                    "CFA: sender account is not critical"
                );
            });

            it("#1.4.2 should reject when sender is zero address", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: ZERO_ADDRESS,
                        receiver: t.aliases[receiver],
                        by: t.aliases[agent]
                    }),
                    "CFA: sender is zero"
                );
            });

            it("#1.4.3 should reject when sender account is not critical", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        by: t.aliases[agent]
                    }),
                    "CFA: sender account is not critical"
                );
            });

            context("#1.4.4 with reward address as admin", () => {
                beforeEach(async () => {
                    await web3tx(
                        governance.setRewardAddress,
                        "set reward address to admin"
                    )(admin);
                });
                shouldTestLiquidations({
                    titlePrefix: "#1.4.4",
                    sender,
                    receiver,
                    by: agent
                });
            });

            context("#1.4.5 with zero reward address", () => {
                beforeEach(async () => {
                    await web3tx(
                        governance.setRewardAddress,
                        "set reward address to zero"
                    )(ZERO_ADDRESS);
                });
                shouldTestLiquidations({
                    titlePrefix: "#1.4.5",
                    sender,
                    receiver,
                    by: agent,
                    // thanks for bailing every one out, dan :)
                    allowCriticalAccount: true
                });
            });
        });

        describe("#1.7 real-time balance", () => {
            // #1.7.1 TODO should be able to downgrade full balance
            // #1.7.2 TODO should be able to downgrade full balance
        });

        describe("#1.8 misc", () => {
            it("#1.8.1 getNetflow should return net flow rate", async () => {
                await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
                await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    sender: "alice",
                    receiver: "bob",
                    flowRate: FLOW_RATE1
                });
                await expectNetFlow("alice", FLOW_RATE1.mul(toBN(-1)));
                await expectNetFlow("bob", FLOW_RATE1);

                const flowRate2 = FLOW_RATE1.divn(3);
                await shouldCreateFlow({
                    testenv: t,
                    sender: "bob",
                    receiver: "alice",
                    flowRate: flowRate2
                });
                await expectNetFlow(
                    "alice",
                    FLOW_RATE1.mul(toBN(-1)).add(flowRate2)
                );
                await expectNetFlow("bob", FLOW_RATE1.sub(flowRate2));
            });

            it("#1.8.2 getMaximumFlowRateFromDeposit", async () => {
                const test = async deposit => {
                    const flowRate = await cfa.getMaximumFlowRateFromDeposit.call(
                        superToken.address,
                        deposit.toString()
                    );
                    const expectedFlowRate = clipDepositNumber(
                        toBN(deposit),
                        true /* rounding down */
                    ).div(toBN(LIQUIDATION_PERIOD));
                    console.log(
                        `f(${deposit.toString()}) = ${expectedFlowRate.toString()} ?`
                    );
                    assert.equal(
                        flowRate.toString(),
                        expectedFlowRate.toString(),
                        `getMaximumFlowRateFromDeposit(${deposit.toString()})`
                    );
                };
                await test(0);
                await test(1);
                await test("10000000000000");
                const maxDeposit = toBN(1)
                    .shln(95)
                    .subn(1);
                await test(maxDeposit);
                expectRevert(
                    test(maxDeposit.addn(1)),
                    "CFA: deposit number too big"
                );
            });

            it("#1.8.3 getDepositRequiredForFlowRate", async () => {
                const test = async flowRate => {
                    const deposit = await cfa.getDepositRequiredForFlowRate.call(
                        superToken.address,
                        flowRate.toString()
                    );
                    const expectedDeposit = clipDepositNumber(
                        toBN(flowRate).mul(toBN(LIQUIDATION_PERIOD))
                    );
                    console.log(
                        `f(${flowRate.toString()}) = ${expectedDeposit.toString()} ?`
                    );
                    assert.equal(
                        deposit.toString(),
                        expectedDeposit.toString(),
                        `getDepositRequiredForFlowRate(${flowRate.toString()})`
                    );
                };
                await test(0);
                await test(1);
                await test("10000000000000");
                await expectRevert(
                    cfa.getDepositRequiredForFlowRate.call(
                        superToken.address,
                        toBN("-100000000000000")
                    ),
                    "CFA: not for negative flow rate"
                );
                const maxFlowRate = toBN(1)
                    .shln(95)
                    .div(toBN(LIQUIDATION_PERIOD));
                await test(maxFlowRate);
                await expectRevert(
                    test(maxFlowRate.addn(1)),
                    "CFA: flow rate too big"
                );
            });

            it("#1.8.4 only authorized host can access token", async () => {
                const FakeSuperfluidMock = artifacts.require(
                    "FakeSuperfluidMock"
                );
                const fakeHost = await FakeSuperfluidMock.new();
                await expectRevert(
                    fakeHost.callAgreement(
                        cfa.address,
                        cfa.contract.methods
                            .createFlow(superToken.address, bob, 1, "0x")
                            .encodeABI(),
                        { from: alice }
                    ),
                    "AgreementLibrary: unauthroized host"
                );
                await expectRevert(
                    fakeHost.callAgreement(
                        cfa.address,
                        cfa.contract.methods
                            .updateFlow(superToken.address, bob, 1, "0x")
                            .encodeABI(),
                        { from: alice }
                    ),
                    "AgreementLibrary: unauthroized host"
                );
                await expectRevert(
                    fakeHost.callAgreement(
                        cfa.address,
                        cfa.contract.methods
                            .deleteFlow(superToken.address, alice, bob, "0x")
                            .encodeABI(),
                        { from: alice }
                    ),
                    "AgreementLibrary: unauthroized host"
                );
            });
        });

        describe("#1.10 should support different flow rates", () => {
            [
                ["small", toBN(2)],
                ["typical", FLOW_RATE1],
                ["large", toWad(42).div(toBN(3600))],
                ["maximum", MAXIMUM_FLOW_RATE.div(toBN(LIQUIDATION_PERIOD))]
            ].forEach(([label, flowRate], i) => {
                it(`#1.10.${i} should support ${label} flow rate (${flowRate})`, async () => {
                    // sufficient liquidity for the test case
                    // - it needs 1x liquidation period
                    // - it adds an additional 60 seconds as extra safe margin
                    const marginalLiquidity = flowRate.mul(toBN(60));
                    const sufficientLiquidity = BN.max(
                        MINIMAL_DEPOSIT.add(marginalLiquidity),
                        flowRate
                            .mul(toBN(LIQUIDATION_PERIOD))
                            .add(marginalLiquidity)
                    );
                    await testToken.mint(t.aliases.alice, sufficientLiquidity, {
                        from: t.aliases.alice
                    });
                    await t.upgradeBalance("alice", sufficientLiquidity);

                    await shouldCreateFlow({
                        testenv: t,
                        sender: "alice",
                        receiver: "bob",
                        flowRate: flowRate.div(toBN(2))
                    });

                    await shouldUpdateFlow({
                        testenv: t,
                        sender: "alice",
                        receiver: "bob",
                        flowRate: flowRate
                    });

                    await timeTravelOnceAndVerifyAll({
                        allowCriticalAccount: true
                    });
                });
            });
        });
    });

    describe("#2 multi flows super app scenarios", () => {
        const sender = "alice";
        const receiver1 = "bob";
        const receiver2 = "carol";
        const lowFlowRate = FLOW_RATE1.mul(toBN(9)).div(toBN(10));
        const highFlowRate = FLOW_RATE1.mul(toBN(11)).div(toBN(10));
        //const receiver2 = "carol";
        //const agent = "dan";
        let app;

        beforeEach(async () => {
            app = await web3tx(MultiFlowApp.new, "MultiApp.new")(
                cfa.address,
                superfluid.address
            );
            t.addAlias("mfa", app.address);
        });

        // due to clipping of flow rate, mfa outgoing flow rate is always equal or less
        // then the sender's rate
        function mfaFlowRate(flowRate, pct = 100) {
            return clipDepositNumber(
                toBN(flowRate)
                    .mul(toBN(LIQUIDATION_PERIOD))
                    .muln(pct)
                    .divn(100),
                true
            ).div(toBN(LIQUIDATION_PERIOD));
        }

        it("#2.1 mfa-1to1_100pct_create-full_updates-full_delete", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            const mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("mfa", FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1)));
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(lowFlowRate));
            await expectNetFlow(
                "mfa",
                lowFlowRate.sub(mfaFlowRate(lowFlowRate))
            );
            await expectNetFlow(receiver1, mfaFlowRate(lowFlowRate));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(highFlowRate));
            await expectNetFlow(
                "mfa",
                highFlowRate.sub(mfaFlowRate(highFlowRate))
            );
            await expectNetFlow(receiver1, mfaFlowRate(highFlowRate));
            await timeTravelOnceAndVerifyAll();

            // fully delete everything
            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                by: sender
            });
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await expectNetFlow(receiver1, "0");
            await timeTravelOnceAndVerifyAll();
        });

        it("#2.2 mfa-1to0_create-updates-delete", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            const mfa = {
                ratioPct: 0,
                sender,
                receivers: {}
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("mfa", FLOW_RATE1);
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(lowFlowRate));
            await expectNetFlow("mfa", lowFlowRate);
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(highFlowRate));
            await expectNetFlow("mfa", highFlowRate);
            await timeTravelOnceAndVerifyAll();

            // fully delete everything
            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                by: sender
            });
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await timeTravelOnceAndVerifyAll();
        });

        it("#2.3 mfa-1to2[50,50]_100pct_create-full_updates-full_delete", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            const mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow(
                "mfa",
                FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1, 50).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1, 50));
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 50));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(lowFlowRate));
            await expectNetFlow(
                "mfa",
                lowFlowRate.sub(mfaFlowRate(lowFlowRate, 50).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(lowFlowRate, 50));
            await expectNetFlow(receiver2, mfaFlowRate(lowFlowRate, 50));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(highFlowRate));
            await expectNetFlow(
                "mfa",
                highFlowRate.sub(mfaFlowRate(highFlowRate, 50).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(highFlowRate, 50));
            await expectNetFlow(receiver2, mfaFlowRate(highFlowRate, 50));
            await timeTravelOnceAndVerifyAll();

            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                by: sender
            });
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, "0");
            await timeTravelOnceAndVerifyAll();
        });

        it("#2.4 mfa-1to2[50,50]_50pct_create-full_updates-full_delete", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            const mfa = {
                ratioPct: 50,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow(
                "mfa",
                FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1, 25).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1, 25));
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 25));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(lowFlowRate));
            await expectNetFlow(
                "mfa",
                lowFlowRate.sub(mfaFlowRate(lowFlowRate, 25).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(lowFlowRate, 25));
            await expectNetFlow(receiver2, mfaFlowRate(lowFlowRate, 25));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(highFlowRate));
            await expectNetFlow(
                "mfa",
                highFlowRate.sub(mfaFlowRate(highFlowRate, 25).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(highFlowRate, 25));
            await expectNetFlow(receiver2, mfaFlowRate(highFlowRate, 25));
            await timeTravelOnceAndVerifyAll();

            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                by: sender
            });
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, "0");
            await timeTravelOnceAndVerifyAll();
        });

        it("#2.5 mfa-1to2[50,50]_150pct_create-full_updates-full_delete", async () => {
            // double the amount since it's a "bigger" flow
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE.muln(2));
            await t.transferBalance(sender, "mfa", toWad(50));

            const mfa = {
                ratioPct: 150,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow(
                "mfa",
                FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1, 75).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1, 75));
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 75));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(lowFlowRate));
            await expectNetFlow(
                "mfa",
                lowFlowRate.sub(mfaFlowRate(lowFlowRate, 75).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(lowFlowRate, 75));
            await expectNetFlow(receiver2, mfaFlowRate(lowFlowRate, 75));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate
            });
            await expectNetFlow(sender, toBN(0).sub(highFlowRate));
            await expectNetFlow(
                "mfa",
                highFlowRate.sub(mfaFlowRate(highFlowRate, 75).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(highFlowRate, 75));
            await expectNetFlow(receiver2, mfaFlowRate(highFlowRate, 75));
            await timeTravelOnceAndVerifyAll();

            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                by: sender
            });
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, "0");
            await timeTravelOnceAndVerifyAll();
        });

        it("#2.6 mfa-1to1-101pct_create-should-fail-without-extra-funds", async () => {
            const mfa = {
                ratioPct: 101,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    }
                }
            };

            await expectRevert(
                shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver: "mfa",
                    mfa,
                    flowRate: FLOW_RATE1
                }),
                "CFA: APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT"
            );
            await timeTravelOnceAndVerifyAll();
        });

        it("#2.7 mfa-1to2[50,50]_100pct_create-partial_delete", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE.muln(2));
            await t.transferBalance(sender, "mfa", toWad(50));

            let mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow(
                "mfa",
                FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1, 50).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1, 50));
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 50));
            await timeTravelOnceAndVerifyAll();

            // delete flow of receiver 1
            mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 0
                    }
                }
            };
            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                by: sender
            });
            await expectNetFlow(sender, "0");
            await expectNetFlow(
                "mfa",
                toBN(0).sub(mfaFlowRate(FLOW_RATE1, 50))
            );
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 50));
        });

        it("#2.8 mfa-loopback-100pct", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            let mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [sender]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(
                sender,
                mfaFlowRate(FLOW_RATE1).sub(FLOW_RATE1)
            );
            await expectNetFlow("mfa", FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1)));
            await timeTravelOnceAndVerifyAll();

            // shouldDeleteFlow doesn't support loopback mode for now, let's use the sf directly
            await web3tx(
                t.sf.cfa.deleteFlow.bind(t.sf.cfa),
                "delete the mfa loopback flow"
            )({
                superToken: superToken.address,
                sender: t.getAddress(sender),
                receiver: app.address,
                userData: web3.eth.abi.encodeParameters(
                    ["address", "uint256", "address[]", "uint256[]"],
                    [
                        t.getAddress(sender),
                        mfa.ratioPct,
                        [t.getAddress(sender)],
                        [1]
                    ]
                )
            });
            await t.validateSystemInvariance();
            assert.isFalse(
                await t.contracts.superfluid.isAppJailed(app.address)
            );
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
        });

        it("#2.9 mfa-1to2[50,50]_100pct_create_full_delete_by_receiver", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            const mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow(
                "mfa",
                FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1, 50).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1, 50));
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 50));
            await timeTravelOnceAndVerifyAll();

            // fully delete everything by receiver1
            await shouldDeleteFlow({
                testenv: t,
                sender: "mfa",
                receiver: receiver1,
                by: receiver1,
                mfa
            });
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, "0");
            await timeTravelOnceAndVerifyAll();
        });

        it("#2.10 mfa-1to1_100pct_create_full_delete_mfa_sender_flow_by_liquidator", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            const mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow(
                "mfa",
                FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1, 50).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1, 50));
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 50));

            await expectRevert(
                t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: app.address,
                    by: dan
                }),
                "CFA: sender account is not critical"
            );

            await timeTravelOnceAndVerifyAll({
                time:
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() -
                    LIQUIDATION_PERIOD +
                    60,
                allowCriticalAccount: true
            });

            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                by: "dan",
                mfa
            });
            assert.isFalse(await superfluid.isAppJailed(app.address));
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, "0");
            await timeTravelOnceAndVerifyAll();
        });

        it("#2.11 mfa-1to1_150pct_create_full_delete_mfa_receiver_flow_by_liquidator", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE.muln(2));
            await t.transferBalance(sender, "mfa", toWad(50));

            const mfa = {
                ratioPct: 150,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            const mfaNetFlowRate = FLOW_RATE1.sub(
                mfaFlowRate(FLOW_RATE1, 75).muln(2)
            );
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("mfa", mfaNetFlowRate);
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1, 75));
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 75));

            await expectRevert(
                t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: app.address,
                    receiver: t.getAddress(receiver1),
                    by: dan
                }),
                "CFA: sender account is not critical"
            );

            await timeTravelOnceAndVerifyAll({
                time:
                    -toWad(50)
                        .div(mfaNetFlowRate)
                        .toNumber() -
                    LIQUIDATION_PERIOD +
                    60,
                allowCriticalAccount: true
            });

            await web3tx(
                t.sf.cfa.deleteFlow.bind(t.sf.cfa),
                "liquidate the mfa receiver1 flow"
            )({
                superToken: superToken.address,
                sender: app.address,
                receiver: t.getAddress(receiver1),
                by: dan
            });
            await expectJailed(
                app.address,
                11 /* APP_RULE_NO_CRITICAL_SENDER_ACCOUNT */
            );
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow(
                "mfa",
                FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1, 75))
            );
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 75));

            // try to rescue the app, but it's already in jail
            await t.transferBalance(sender, "mfa", toWad(10));
            assert.isTrue((await superToken.balanceOf(app.address)) > 0);

            await web3tx(
                t.sf.cfa.deleteFlow.bind(t.sf.cfa),
                "liquidate the mfa receiver2 flow"
            )({
                superToken: superToken.address,
                sender: app.address,
                receiver: t.getAddress(receiver2),
                by: dan
            });
            await expectJailed(
                app.address,
                11 /* APP_RULE_NO_CRITICAL_SENDER_ACCOUNT */
            );
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("mfa", FLOW_RATE1);
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, "0");

            await web3tx(
                t.sf.cfa.deleteFlow.bind(t.sf.cfa),
                "liquidate the mfa sender flow"
            )({
                superToken: superToken.address,
                sender: t.getAddress(sender),
                receiver: app.address,
                by: dan
            });
            await expectJailed(
                app.address,
                11 /* APP_RULE_NO_CRITICAL_SENDER_ACCOUNT */
            );
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, "0");
            await t.validateSystemInvariance();
        });

        it("#2.12 mfa-1to2[50,50]_100pct_create-partial_delete-negative_app_balance", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            let mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await shouldCreateFlow({
                testenv: t,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow(
                "mfa",
                FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1, 50).muln(2))
            );
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1, 50));
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 50));

            // delete flow of receiver 1
            await web3tx(
                t.sf.cfa.deleteFlow.bind(t.sf.cfa),
                "delete the mfa flows partially"
            )({
                superToken: superToken.address,
                sender: t.getAddress(sender),
                receiver: app.address,
                userData: web3.eth.abi.encodeParameters(
                    ["address", "uint256", "address[]", "uint256[]"],
                    [
                        t.getAddress(sender),
                        mfa.ratioPct,
                        [t.getAddress(receiver1)],
                        [1]
                    ]
                )
            });
            await expectJailed(
                app.address,
                12 /* APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT */
            );
            await t.validateSystemInvariance();
            await expectNetFlow(sender, "0");
            await expectNetFlow(
                "mfa",
                toBN(0).sub(mfaFlowRate(FLOW_RATE1, 50))
            );
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, mfaFlowRate(FLOW_RATE1, 50));
        });

        it("#2.20 createFlow via app action should respect deposit rule", async () => {
            await expectRevert(
                t.sf.host.callAppAction(
                    app.address,
                    app.contract.methods
                        .createFlow(
                            superToken.address,
                            bob,
                            FLOW_RATE1.toString(),
                            "0x"
                        )
                        .encodeABI()
                ),
                "CFA: not enough available balance"
            );
        });

        it("#2.21 mfa-1to2[50,50]_150pct_create-full should fail without app balance", async () => {
            // double the amount since it's a "bigger" flow
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            const mfa = {
                ratioPct: 150,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1
                    },
                    [receiver2]: {
                        proportion: 1
                    }
                }
            };

            await expectRevert(
                shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver: "mfa",
                    mfa,
                    flowRate: FLOW_RATE1
                }),
                "CFA: APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT"
            );
        });
    });

    describe("#10 multi accounts scenarios", () => {
        it("#10.1 two accounts sending to each other with the same flow rate", async () => {
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            await shouldCreateFlow({
                testenv: t,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1
            });
            await expectNetFlow("alice", FLOW_RATE1.mul(toBN(-1)));
            await expectNetFlow("bob", FLOW_RATE1);
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                sender: "bob",
                receiver: "alice",
                flowRate: FLOW_RATE1
            });
            await expectNetFlow("alice", "0");
            await expectNetFlow("bob", "0");
            await timeTravelOnceAndVerifyAll();
        });

        it("#10.2 three accounts forming a flow loop", async () => {
            // alice -> bob -> carol
            //   ^---------------|
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            const flowRateBC = FLOW_RATE1.muln(2).divn(3);
            const flowRateCA = FLOW_RATE1.divn(3);

            await shouldCreateFlow({
                testenv: t,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("bob", FLOW_RATE1);
            await expectNetFlow("carol", "0");
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                sender: "bob",
                receiver: "carol",
                flowRate: flowRateBC
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("bob", FLOW_RATE1.sub(flowRateBC));
            await expectNetFlow("carol", flowRateBC);
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                sender: "carol",
                receiver: "alice",
                flowRate: flowRateCA
            });
            await expectNetFlow("alice", toBN(flowRateCA).sub(FLOW_RATE1));
            await expectNetFlow("bob", FLOW_RATE1.sub(flowRateBC));
            await expectNetFlow("carol", flowRateBC.sub(flowRateCA));
            await timeTravelOnceAndVerifyAll();
        });

        it("#10.3 a slight complex flow map", async () => {
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE.muln(2));

            const flowRateBD = FLOW_RATE1.muln(2).divn(3);
            const flowRateDC = FLOW_RATE1.divn(3);
            //const flowRate;

            await shouldCreateFlow({
                testenv: t,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1
            });
            await shouldCreateFlow({
                testenv: t,
                sender: "alice",
                receiver: "carol",
                flowRate: FLOW_RATE1
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1.muln(2)));
            await expectNetFlow("bob", FLOW_RATE1);
            await expectNetFlow("carol", FLOW_RATE1);
            await expectNetFlow("dan", "0");
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                sender: "bob",
                receiver: "dan",
                flowRate: flowRateBD
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1.muln(2)));
            await expectNetFlow("bob", FLOW_RATE1.sub(flowRateBD));
            await expectNetFlow("carol", FLOW_RATE1);
            await expectNetFlow("dan", flowRateBD);
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                sender: "dan",
                receiver: "carol",
                flowRate: flowRateDC
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1.muln(2)));
            await expectNetFlow("bob", FLOW_RATE1.sub(flowRateBD));
            await expectNetFlow("carol", FLOW_RATE1.add(flowRateDC));
            await expectNetFlow("dan", flowRateBD.sub(flowRateDC));
            await timeTravelOnceAndVerifyAll();
        });
    });
});
