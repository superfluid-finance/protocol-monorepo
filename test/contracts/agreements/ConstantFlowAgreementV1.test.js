const { BN, expectRevert } = require("@openzeppelin/test-helpers");
const {
    web3tx,
    toWad,
    toBN
} = require("@decentral.ee/web3-helpers");
const {
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
    shouldVerifyFlow
} = require("./ConstantFlowAgreementV1.behavior.js");

const TestEnvironment = require("../../TestEnvironment");

const traveler = require("ganache-time-traveler");

const TEST_TRAVEL_TIME = 3600 * 24; // 24 hours

const FLOW_RATE1 = toWad("1").div(toBN(3600)); // 1 per hour
const MAXIMUM_FLOW_RATE = toBN(2).pow(toBN(95)).sub(toBN(1));
const MINIMAL_DEPOSIT = toBN(1).shln(32);


contract("Using ConstantFlowAgreement v1", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 5));
    const { admin, alice, dan } = t.aliases;
    const { ZERO_ADDRESS } = t.constants;
    const { LIQUIDATION_PERIOD } = t.configs;

    let testToken;
    let governance;
    let superToken;

    before(async () => {
        await t.reset();
    });

    beforeEach(async function () {
        await t.resetData();
        await t.createNewToken();
        ({
            testToken,
            governance,
            superToken,
        } = t.contracts);
    });

    async function timeTravelOnce(time = TEST_TRAVEL_TIME) {
        console.log("current block time", (await web3.eth.getBlock("latest")).timestamp);
        console.log(`time traveler going to the future +${time}...`);
        await traveler.advanceTimeAndBlock(time);
        console.log("new block time", (await web3.eth.getBlock("latest")).timestamp);
    }

    async function upgradeBalance(account, amount) {
        await web3tx(superToken.upgrade, `Upgrade ${amount.toString()} for account ${account}`)(
            amount, { from: t.aliases[account] }
        );
        await t.updateAccountBalanceSnapshot({
            superToken: superToken.address,
            account: t.aliases[account],
            balanceSnapshot: await superToken.realtimeBalanceOfNow(t.aliases[account])
        });
    }

    async function shouldTestLiquidations({ titlePrefix, sender, receiver, by }) {
        const liquidationType = by === sender ? "liquidate by agent" : "self liquidate";

        it(`${titlePrefix}.1 should ${liquidationType} when critical but solvent`, async () => {
            assert.isFalse(await superToken.isAccountCriticalNow(t.aliases[sender]));
            assert.isTrue(await superToken.isAccountSolventNow(t.aliases[sender]));
            // drain the balance until critical (60sec extra)
            await timeTravelOnce(
                t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() - LIQUIDATION_PERIOD + 60
            );
            assert.isTrue(await superToken.isAccountCriticalNow(t.aliases[sender]));
            assert.isTrue(await superToken.isAccountSolventNow(t.aliases[sender]));

            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver,
                by
            });

            await t.validateSystemInvariance();
        });

        it.skip(`${titlePrefix}.2 should ${liquidationType} when insolvent`, async () => {
            assert.isFalse(await superToken.isAccountCriticalNow(t.aliases[sender]));
            assert.isTrue(await superToken.isAccountSolventNow(t.aliases[sender]));
            // drain the balance until insolvent (60sec extra)
            await timeTravelOnce(
                t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() + 60
            );
            assert.isTrue(await superToken.isAccountCriticalNow(t.aliases[sender]));
            assert.isFalse(await superToken.isAccountSolventNow(t.aliases[sender]));

            await shouldDeleteFlow({
                testenv: t,
                sender,
                receiver,
                by
            });

            await t.validateSystemInvariance();
        });
    }

    context("#1 without callbacks", () => {
        const sender = "alice";
        const receiver = "bob";
        const agent = "dan";

        before (() => {
            console.log(`sender is ${sender} ${t.aliases[sender]}`);
            console.log(`receiver is ${receiver} ${t.aliases[receiver]}`);
        });

        describe("#1.1 createFlow", () => {
            it("#1.1.1 should create when there is enough balance", async () => {
                await upgradeBalance(sender, t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });

                await timeTravelOnce();

                await shouldVerifyFlow({
                    testenv: t,
                    sender,
                    receiver,
                });

                await t.validateSystemInvariance();
            });

            it("#1.1.2 should reject when there is not enough balance", async () => {
                await expectRevert(t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: FLOW_RATE1.toString()
                }), "CFA: not enough available balance");
            });

            it("#1.1.3 should reject when zero flow rate", async () => {
                await expectRevert(t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: "0"
                }), "CFA: invalid flow rate");
            });

            it("#1.1.4 should reject when negative flow rate", async () => {
                await expectRevert(t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: "-1"
                }), "CFA: invalid flow rate");
            });

            it("#1.1.5 should reject when self flow", async () => {
                await expectRevert(t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[sender],
                    flowRate: FLOW_RATE1.toString()
                }), "CFA: no self flow");
            });

            it("#1.1.6 should not create same flow", async () => {
                await upgradeBalance(sender, t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });
                await expectRevert(t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: FLOW_RATE1.toString()
                }), "CFA: flow already exist");
            });

            it("#1.1.7 should reject when overflow flow rate", async () => {
                await expectRevert(t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: MAXIMUM_FLOW_RATE.toString(),
                }), "Int96SafeMath: multiplication overflow");
            });

            it("#1.1.8 should reject when receiver is zero address", async () => {
                await expectRevert(t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: ZERO_ADDRESS,
                    flowRate: FLOW_RATE1.toString(),
                }), "CFA: receiver is zero");
            });
        });

        describe("#1.2 updateFlow", () => {
            beforeEach(async () => {
                await upgradeBalance(sender, t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });
            });

            it("#1.2.1 can maintain existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });

                await timeTravelOnce();

                await shouldVerifyFlow({
                    testenv: t,
                    sender,
                    receiver,
                });

                await t.validateSystemInvariance();
            });

            it("#1.2.2 can increase (+10%) existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10)),
                });

                await timeTravelOnce();

                await shouldVerifyFlow({
                    testenv: t,
                    sender,
                    receiver,
                });

                await t.validateSystemInvariance();
            });

            it("#1.2.3 can decrease (-10%) existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(9)).div(toBN(10)),
                });

                await timeTravelOnce();

                await shouldVerifyFlow({
                    testenv: t,
                    sender,
                    receiver,
                });

                await t.validateSystemInvariance();
            });

            it("#1.2.4 should not update with zero flow rate", async () => {
                await expectRevert(t.sf.cfa.updateFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: "0"
                }), "CFA: invalid flow rate");
            });

            it("#1.2.5 should not update with negative flow rate", async () => {
                await expectRevert(t.sf.cfa.updateFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: "-1"
                }), "CFA: invalid flow rate");
            });

            it("#1.2.6 should not update non existing flow", async () => {
                await expectRevert(t.sf.cfa.updateFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: dan,
                    flowRate: FLOW_RATE1.toString(),
                }), "CFA: flow does not exist");
            });

            it("#1.2.7 should not update non existing flow (self flow)", async () => {
                await expectRevert(t.sf.cfa.updateFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[sender],
                    flowRate: FLOW_RATE1.toString(),
                }), "CFA: no self flow");
            });

            it("#1.2.8 should reject when there is not enough balance", async () => {
                await expectRevert(t.sf.cfa.updateFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: toBN(t.configs.INIT_BALANCE)
                        .div(toBN(LIQUIDATION_PERIOD).sub(toBN(60)))
                        .toString()
                }), "CFA: not enough available balance");
            });

            it("#1.2.9 should reject when overflow flow rate", async () => {
                await expectRevert(t.sf.cfa.updateFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    flowRate: MAXIMUM_FLOW_RATE.toString(),
                }), "Int96SafeMath: multiplication overflow");
            });

            it("#1.2.10 should reject when receiver is zero address", async () => {
                await expectRevert(t.sf.cfa.updateFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: ZERO_ADDRESS,
                    flowRate: FLOW_RATE1.toString(),
                }), "CFA: receiver is zero");
            });
        });

        describe("#1.3 deleteFlow (non liquidation)", () => {
            beforeEach(async () => {
                await upgradeBalance(sender, t.configs.INIT_BALANCE);
                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });
            });

            it("#1.3.1 can delete existing flow", async () => {
                await shouldDeleteFlow({
                    testenv: t,
                    sender,
                    receiver,
                    by: sender
                });

                await timeTravelOnce();

                await shouldVerifyFlow({
                    testenv: t,
                    sender,
                    receiver,
                });

                await t.validateSystemInvariance();
            });

            it("#1.3.2 can delete an updated flow", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10)),
                });
                await shouldDeleteFlow({
                    testenv: t,
                    sender,
                    receiver,
                    by: sender
                });

                await timeTravelOnce();

                await shouldVerifyFlow({
                    testenv: t,
                    sender,
                    receiver,
                });

                await t.validateSystemInvariance();
            });

            it("#1.3.3 should not delete non-existing flow", async () => {
                await expectRevert(t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: dan,
                }), "CFA: flow does not exist");
            });

            it("#1.3.4 should reject when receiver is zero address", async () => {
                await expectRevert(t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: ZERO_ADDRESS,
                }), "CFA: receiver is zero");
            });

            it("#1.3.5 should reject when sender is zero address", async () => {
                await expectRevert(t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: ZERO_ADDRESS,
                    receiver: dan,
                    by: t.aliases[sender]
                }), "CFA: sender is zero");
            });

            context("#1.3.6 with reward address as admin", () => {
                beforeEach(async () => {
                    await web3tx(governance.setRewardAddress, "set reward address to admin")(admin);
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
                    await web3tx(governance.setRewardAddress, "set reward address to zero")(ZERO_ADDRESS);
                });
                shouldTestLiquidations({
                    titlePrefix: "#1.3.7",
                    sender,
                    receiver,
                    by: sender
                });
            });
        });

        describe("#1.4 deleteFlow (liquidations)", () => {
            beforeEach(async () => {
                await upgradeBalance(sender, t.configs.INIT_BALANCE);
                await shouldCreateFlow({
                    testenv: t,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });
            });

            it("#1.4.1 should reject when sender account is not critical", async () => {
                await expectRevert(t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: dan
                }), "CFA: account is not critical");
            });

            it("#1.4.2 should reject when sender is zero address", async () => {
                await expectRevert(t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: ZERO_ADDRESS,
                    receiver: t.aliases[receiver],
                    by: dan
                }), "CFA: sender is zero");
            });

            it("#1.4.3 should reject when sender account is not critical", async () => {
                await expectRevert(t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: dan
                }), "CFA: account is not critical");
            });

            context("#1.4.4 liquidate with reward address as admin", () => {
                beforeEach(async () => {
                    await web3tx(governance.setRewardAddress, "set reward address to admin")(admin);
                });
                shouldTestLiquidations({
                    titlePrefix: "#1.4.4",
                    sender,
                    receiver,
                    by: agent
                });
            });

            context("#1.4.5 liquidate with zero reward address", () => {
                beforeEach(async () => {
                    await web3tx(governance.setRewardAddress, "set reward address to zero")(ZERO_ADDRESS);
                });
                shouldTestLiquidations({
                    titlePrefix: "#1.4.5",
                    sender,
                    receiver,
                    by: agent
                });
            });
        });

        describe("#1.10 should support different flow rates", () => {
            [
                ["small", toBN(2)],
                ["typical", FLOW_RATE1],
                ["large", toWad(42).div(toBN(3600))],
                ["maximum", MAXIMUM_FLOW_RATE.div(toBN(LIQUIDATION_PERIOD))]
            ].forEach(([label, flowRate], i) => {
                it(`#1.5.${i} should support ${label} flow rate (${flowRate})`, async () => {
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
                    await testToken.mint(alice, sufficientLiquidity, {
                        from: alice
                    });
                    await upgradeBalance("alice", sufficientLiquidity);

                    await shouldCreateFlow({
                        testenv: t,
                        sender: "alice",
                        receiver: "bob",
                        flowRate: flowRate.div(toBN(2)),
                    });

                    await shouldUpdateFlow({
                        testenv: t,
                        sender: "alice",
                        receiver: "bob",
                        flowRate: flowRate,
                    });

                    await timeTravelOnce();

                    await shouldVerifyFlow({
                        testenv: t,
                        sender: "alice",
                        receiver: "bob",
                    });

                    await t.validateSystemInvariance();
                });
            });
        });

        describe("#1.6 real-time balance", () => {
            // #1.6.1 TODO should be able to downgrade full balance

            // #1.6.2 TODO should be able to downgrade full balance
        });
    });

    describe("#2 multi accounts scenarios", () => {
        // always downgrade balances in the end

        // #2.1 net zero
        // #2.2 loop
        // #2.3 complex map
    });

    describe("#3 flow proxy super app scenarios", () => {
    });

});
