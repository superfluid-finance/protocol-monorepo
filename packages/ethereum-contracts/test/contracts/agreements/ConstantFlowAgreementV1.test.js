const TestEnvironment = require("../../TestEnvironment");

const {BN, expectRevert, expectEvent} = require("@openzeppelin/test-helpers");
const {web3tx, toWad, toBN} = require("@decentral.ee/web3-helpers");
const {
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
} = require("./ConstantFlowAgreementV1.behavior.js");

const traveler = require("ganache-time-traveler");
const CFADataModel = require("./ConstantFlowAgreementV1.data.js");

const TEST_TRAVEL_TIME = 3600 * 24; // 24 hours

const MAXIMUM_FLOW_RATE = toBN(2).pow(toBN(95)).sub(toBN(1));

describe("Using ConstantFlowAgreement v1", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const {ZERO_ADDRESS} = t.constants;
    const {LIQUIDATION_PERIOD, FLOW_RATE1, MINIMUM_DEPOSIT} = t.configs;

    let admin, alice, bob, dan;
    let superfluid;
    let governance;
    let cfa;
    let testToken;
    let superToken;

    before(async function () {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({admin, alice, bob, dan} = t.aliases);

        ({superfluid, governance, cfa} = t.contracts);
        testToken = await t.sf.contracts.TestToken.at(t.sf.tokens.TEST.address);
        superToken = t.sf.tokens.TESTx;
    });

    after(async function () {
        await t.report({title: "ConstantFlowAgreement.test"});
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    // TODO: regex from # until the end
    afterEach(() => {
        if (t.plotData.enabled) {
            t.writePlotDataIntoCSVFile(
                this.ctx.test.title
                    .split("#")[1]
                    // eslint-disable-next-line
                    .split('"')[0]
                    .split(" ")
                    .join("_"),
                superToken.address
            );
        }
    });

    async function timeTravelOnce(time = TEST_TRAVEL_TIME) {
        const block1 = await web3.eth.getBlock("latest");
        console.log("current block time", block1.timestamp);
        console.log(`time traveler going to the future +${time}...`);
        await traveler.advanceTimeAndBlock(time);
        const block2 = await web3.eth.getBlock("latest");
        console.log("new block time", block2.timestamp);
    }

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
        const time = opts.time || TEST_TRAVEL_TIME;
        await timeTravelOnce(time);
        await verifyAll(opts);
    }

    async function timeTravelOnceAndValidateSystemInvariance(opts = {}) {
        const time = opts.time || TEST_TRAVEL_TIME;
        await timeTravelOnce(time);
        await t.validateSystemInvariance(opts);
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
                app: appAddress,
            },
        });
        assert.equal(events.length, 1);
        assert.equal(events[0].args.reason.toString(), reasonCode.toString());
    }

    function shouldCreateSolventLiquidationTest({
        titlePrefix,
        sender,
        receiver,
        by,
        seconds,
    }) {
        it(`${titlePrefix}.a should be liquidated when critical but solvent`, async () => {
            const defaultSolvencyStatus = {
                preIsCritical: false,
                preIsSolvent: true,
                postIsCritical: true,
                postIsSolvent: true,
            };
            await _testLiquidation({
                sender,
                receiver,
                by,
                seconds,
                solvencyStatuses: defaultSolvencyStatus,
            });
        });
    }

    function shouldCreateBailoutTest({
        titlePrefix,
        sender,
        receiver,
        by,
        allowCriticalAccount,
        seconds,
    }) {
        it(`${titlePrefix}.b can liquidate and bail out when insolvent`, async () => {
            const defaultSolvencyStatus = {
                preIsCritical: false,
                preIsSolvent: true,
                postIsCritical: true,
                postIsSolvent: false,
            };
            await _testLiquidation({
                isBailout: true,
                sender,
                receiver,
                by,
                allowCriticalAccount,
                seconds,
                solvencyStatuses: defaultSolvencyStatus,
            });
        });
    }

    async function _testLiquidation({
        sender,
        receiver,
        by,
        seconds,
        allowCriticalAccount,
        solvencyStatuses,
        isBailout,
        shouldSkipTimeTravel,
    }) {
        // get initial state
        await t.validateSystemInvariance({
            allowCriticalAccount,
            description: "STR:" + receiver,
        });

        const accountFlowInfo = await t.sf.cfa.getAccountFlowInfo({
            superToken: superToken.address,
            account: t.aliases[sender],
        });
        const netFlowRate = toBN(accountFlowInfo.flowRate).mul(toBN(-1)); // convert net flow rate to positive

        if (solvencyStatuses.preIsCritical) {
            assert.isTrue(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
        } else {
            assert.isFalse(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
        }

        if (solvencyStatuses.preIsSolvent) {
            assert.isTrue(
                await superToken.isAccountSolventNow(t.aliases[sender])
            );
        } else {
            assert.isFalse(
                await superToken.isAccountSolventNow(t.aliases[sender])
            );
        }

        if (netFlowRate.eq(toBN(0))) {
            // ensure that when testing liquidations when the account has a 0 net flow
            // that the available balance is negative: still can be liquidated but
            // they aren't at risk of going insolvent unless their inflow stops
            assert.isTrue(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
        } else {
            // provide the option to skip time travel, especially in the multi flow cases
            if (!shouldSkipTimeTravel) {
                const liquidationPeriod = isBailout ? 0 : LIQUIDATION_PERIOD;
                // drain the balance until critical (`seconds` sec extra)
                await timeTravelOnceAndVerifyAll({
                    time:
                        t.configs.INIT_BALANCE.div(netFlowRate).toNumber() -
                        liquidationPeriod +
                        seconds,
                    allowCriticalAccount: true,
                });
            }
        }

        if (solvencyStatuses.postIsCritical) {
            assert.isTrue(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
        } else {
            assert.isFalse(
                await superToken.isAccountCriticalNow(t.aliases[sender])
            );
        }

        if (solvencyStatuses.postIsSolvent) {
            assert.isTrue(
                await superToken.isAccountSolventNow(t.aliases[sender])
            );
        } else {
            assert.isFalse(
                await superToken.isAccountSolventNow(t.aliases[sender])
            );
        }

        await shouldDeleteFlow({
            testenv: t,
            superToken,
            sender,
            receiver,
            by,
            accountFlowInfo,
        });

        await verifyAll({allowCriticalAccount, description: "LIQ"});
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
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.1.2 should reject when there is not enough balance", async () => {
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: FLOW_RATE1.toString(),
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
                        flowRate: "0",
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
                        flowRate: "-1",
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
                        flowRate: FLOW_RATE1.toString(),
                    }),
                    "CFA: no self flow"
                );
            });

            it("#1.1.6 should not create same flow", async () => {
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });
                await expectRevert(
                    t.sf.cfa.createFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: FLOW_RATE1.toString(),
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
                        flowRate: MAXIMUM_FLOW_RATE.toString(),
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
                        flowRate: FLOW_RATE1.toString(),
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
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });
            });

            it("#1.2.1 can maintain existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.2.2 can increase (+10%) existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10)),
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.2.3 can decrease (-10%) existing flow rate", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(9)).div(toBN(10)),
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.2.4 should not update with zero flow rate", async () => {
                await expectRevert(
                    t.sf.cfa.updateFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        flowRate: "0",
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
                        flowRate: "-1",
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
                        flowRate: FLOW_RATE1.toString(),
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
                        flowRate: FLOW_RATE1.toString(),
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
                            .toString(),
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
                        flowRate: MAXIMUM_FLOW_RATE.toString(),
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
                        flowRate: FLOW_RATE1.toString(),
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
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });
            });

            it("#1.3.1.a can delete existing flow by sender", async () => {
                await shouldDeleteFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    by: sender,
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.3.1.b can delete existing flow by receiver", async () => {
                await shouldDeleteFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    by: receiver,
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.3.2 can delete an updated flow", async () => {
                await shouldUpdateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10)),
                });
                await shouldDeleteFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    by: sender,
                });

                await timeTravelOnceAndVerifyAll();
            });

            it("#1.3.3 should not delete non-existing flow", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[agent],
                    }),
                    "CFA: flow does not exist"
                );
            });

            it("#1.3.4 should reject when receiver is zero address", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: ZERO_ADDRESS,
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
                        by: t.aliases[sender],
                    }),
                    "CFA: sender is zero"
                );
            });
        });

        describe("#1.4 deleteFlow (liquidations)", () => {
            beforeEach(async () => {
                // give admin some balance for liquidations
                await t.upgradeBalance("admin", t.configs.INIT_BALANCE);
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);
                await shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });

                t.initializePlotData(true); // observing all accounts
            });

            it("#1.4.1 should reject when sender account is not critical", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        by: t.aliases[agent],
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
                        by: t.aliases[agent],
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
                        by: t.aliases[agent],
                    }),
                    "CFA: sender account is not critical"
                );
            });

            context(
                "#1.4.4 with reward address as admin (agent is liquidator)",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to admin"
                        )(superfluid.address, ZERO_ADDRESS, admin);
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.4",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        seconds: 60,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.4",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        seconds: 60,
                    });
                }
            );

            context(
                "#1.4.5 with zero reward address (agent is liquidator)",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to zero"
                        )(superfluid.address, ZERO_ADDRESS, ZERO_ADDRESS);
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.5",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        seconds: 60,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.5",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        // thanks for bailing every one out, dan :)
                        allowCriticalAccount: true,
                        seconds: 60,
                    });
                }
            );

            context(
                "#1.4.6 with reward address as admin (sender is liquidator)",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to admin"
                        )(superfluid.address, ZERO_ADDRESS, admin);
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.6",
                        superToken,
                        sender,
                        receiver,
                        by: sender,
                        seconds: 60,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.6",
                        superToken,
                        sender,
                        receiver,
                        by: sender,
                        allowCriticalAccount: true,
                        seconds: 60,
                    });
                }
            );

            context(
                "#1.4.7 with zero reward address (sender is liquidator)",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to zero"
                        )(superfluid.address, ZERO_ADDRESS, ZERO_ADDRESS);
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.7",
                        superToken,
                        sender,
                        receiver,
                        by: sender,
                        seconds: 60,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.7",
                        superToken,
                        sender,
                        receiver,
                        by: sender,
                        // no one will bail you out, alice :(
                        allowCriticalAccount: true,
                        seconds: 60,
                    });
                }
            );

            context(
                "#1.4.8 test agent liquidation out of patrician period",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to admin"
                        )(superfluid.address, ZERO_ADDRESS, admin);
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.8",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.8",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        // thanks for bailing every one out, dan :)
                        allowCriticalAccount: true,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                }
            );

            context(
                "#1.4.9 test sender reward account liquidation out of patrician period",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to sender"
                        )(superfluid.address, ZERO_ADDRESS, t.aliases[sender]);
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.9",
                        superToken,
                        sender,
                        receiver,
                        by: sender,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.9",
                        superToken,
                        sender,
                        receiver,
                        by: sender,
                        // thanks for bailing every one out, alice :)
                        allowCriticalAccount: true,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                }
            );

            context(
                "#1.4.10 test reward account liquidation out of patrician period",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to agent"
                        )(superfluid.address, ZERO_ADDRESS, t.aliases[agent]);
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.10",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.10",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        // thanks for bailing every one out, dan :)
                        allowCriticalAccount: true,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                }
            );

            context(
                "#1.4.11 with zero reward address out of patrician period",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to zero"
                        )(superfluid.address, ZERO_ADDRESS, ZERO_ADDRESS);
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.11",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.11",
                        superToken,
                        sender,
                        receiver,
                        by: agent,
                        // thanks for bailing every one out, dan :)
                        allowCriticalAccount: true,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                }
            );

            context(
                "#1.4.12 with receiver as liquidator out of patrician period",
                () => {
                    beforeEach(async () => {
                        await web3tx(
                            governance.setRewardAddress,
                            "set reward address to agent"
                        )(superfluid.address, ZERO_ADDRESS, t.aliases[agent]);

                        t.initializePlotData(true); // observing all accounts
                    });
                    shouldCreateSolventLiquidationTest({
                        titlePrefix: "#1.4.12",
                        superToken,
                        sender,
                        receiver,
                        by: receiver,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                    shouldCreateBailoutTest({
                        titlePrefix: "#1.4.12",
                        superToken,
                        sender,
                        receiver,
                        by: receiver,
                        // thanks for bailing every one out, dan :)
                        allowCriticalAccount: true,
                        seconds: t.configs.PATRICIAN_PERIOD + 1,
                    });
                }
            );

            it("#1.4.13 allow liquidation with sender positive net flowrate", async () => {
                // drain the sender into criticality
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber()
                );
                assert.isTrue(
                    await superToken.isAccountCriticalNow(t.aliases[sender])
                );

                // start a reverse flow leading to a slightly positive sender net flowrate
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[receiver],
                    receiver: t.aliases[sender],
                    flowRate: FLOW_RATE1.add(toBN(1)).toString(),
                });

                assert.isTrue(
                    (
                        await cfa.getNetFlow(
                            superToken.address,
                            t.aliases[sender]
                        )
                    ).gt(toBN(0))
                );
                assert.isTrue(
                    await superToken.isAccountCriticalNow(t.aliases[sender])
                );

                // account still critical, should be possible to liquidate
                await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });
            });

            it("#1.4.14a correct reward attribution for patrician period", async () => {
                const adminInitBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentInitBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;

                // drain the sender into patrician territory
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() -
                        t.configs.LIQUIDATION_PERIOD +
                        1
                );

                // agent liquidates
                const r = await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });

                // rewards should go to the rewardAddress
                await expectEvent.inTransaction(
                    r.tx,
                    t.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {rewardAccount: admin}
                );

                // reward account (here: admin) should have received the deposit
                const adminPostBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentPostBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;
                assert.notEqual(
                    adminPostBal.toString(),
                    adminInitBal.toString()
                );

                assert.equal(agentPostBal.toString(), agentInitBal.toString());
            });

            it("#1.4.14b correct reward attribution for patrician period with two-way flows", async () => {
                const adminInitBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentInitBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;

                // drain the sender into patrician territory
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() -
                        t.configs.LIQUIDATION_PERIOD +
                        1
                );

                // start a reverse flow leading to a near-zero negative sender net flowrate
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[receiver],
                    receiver: t.aliases[sender],
                    flowRate: FLOW_RATE1.sub(toBN(1)).toString(),
                });

                // agent liquidates
                const r = await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });

                // rewards should go to the rewardAddress
                await expectEvent.inTransaction(
                    r.tx,
                    t.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {rewardAccount: admin}
                );

                // reward account (here: admin) should have received the deposit
                const adminPostBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentPostBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;
                assert.notEqual(
                    adminPostBal.toString(),
                    adminInitBal.toString()
                );

                assert.equal(agentPostBal.toString(), agentInitBal.toString());
            });

            it("#1.4.14c correct reward attribution for patrician period with multiple outflows", async () => {
                // start another, larger, outflow
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases.carol,
                    flowRate: FLOW_RATE1.mul(toBN(5)).toString(),
                });

                const adminInitBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentInitBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;

                // drain the sender into patrician territory
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(
                        FLOW_RATE1.mul(toBN(6))
                    ).toNumber() -
                        t.configs.LIQUIDATION_PERIOD +
                        1
                );

                // start a reverse flow leading to a near-zero negative sender net flowrate
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[receiver],
                    receiver: t.aliases[sender],
                    flowRate: FLOW_RATE1.sub(toBN(1)).toString(),
                });

                // agent liquidates
                const r = await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });

                // rewards should go to the rewardAddress
                await expectEvent.inTransaction(
                    r.tx,
                    t.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {rewardAccount: admin}
                );

                // reward account (here: admin) should have received the deposit
                const adminPostBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentPostBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;
                assert.notEqual(
                    adminPostBal.toString(),
                    adminInitBal.toString()
                );

                assert.equal(agentPostBal.toString(), agentInitBal.toString());
            });

            it("#1.4.15a correct reward attribution for plebs period", async () => {
                const adminInitBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentInitBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;

                // drain the sender into plebs territory
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() -
                        t.configs.LIQUIDATION_PERIOD +
                        t.configs.PATRICIAN_PERIOD +
                        1
                );

                assert.isTrue(
                    await superToken.isAccountCriticalNow(t.aliases[sender])
                );
                assert.isTrue(
                    await superToken.isAccountSolventNow(t.aliases[sender])
                );

                // agent liquidates
                const r = await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });

                // rewards should go to the agent (plebs rule)
                await expectEvent.inTransaction(
                    r.tx,
                    t.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {rewardAccount: t.aliases[agent]}
                );

                // reward account (here: agent) should have received the deposit
                const adminPostBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentPostBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;
                assert.equal(adminPostBal.toString(), adminInitBal.toString());
                assert.notEqual(
                    agentPostBal.toString(),
                    agentInitBal.toString()
                );
            });

            it("#1.4.15b correct reward attribution for plebs period with two-way flows", async () => {
                const adminInitBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentInitBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;

                // drain the sender into plebs territory
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() -
                        t.configs.LIQUIDATION_PERIOD +
                        t.configs.PATRICIAN_PERIOD +
                        1
                );

                assert.isTrue(
                    await superToken.isAccountCriticalNow(t.aliases[sender])
                );
                assert.isTrue(
                    await superToken.isAccountSolventNow(t.aliases[sender])
                );

                // start a reverse flow leading to a near-zero negative sender net flowrate
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[receiver],
                    receiver: t.aliases[sender],
                    flowRate: FLOW_RATE1.sub(toBN(1)).toString(),
                });

                // agent liquidates
                const r = await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });

                // rewards should go to the agent (plebs rule)
                await expectEvent.inTransaction(
                    r.tx,
                    t.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {rewardAccount: t.aliases[agent]}
                );

                // reward account (here: agent) should have received the deposit
                const adminPostBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentPostBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;
                assert.equal(adminPostBal.toString(), adminInitBal.toString());
                assert.notEqual(
                    agentPostBal.toString(),
                    agentInitBal.toString()
                );
            });

            it("#1.4.15c correct reward attribution for plebs period with multiple outflows", async () => {
                // start another, larger, outflow
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases.carol,
                    flowRate: FLOW_RATE1.mul(toBN(5)).toString(),
                });

                const adminInitBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentInitBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;

                // drain the sender into plebs territory
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(
                        FLOW_RATE1.mul(toBN(6))
                    ).toNumber() -
                        t.configs.LIQUIDATION_PERIOD +
                        t.configs.PATRICIAN_PERIOD +
                        1
                );

                assert.isTrue(
                    await superToken.isAccountCriticalNow(t.aliases[sender])
                );
                assert.isTrue(
                    await superToken.isAccountSolventNow(t.aliases[sender])
                );

                // agent liquidates
                const r = await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });

                // rewards should go to the agent (plebs rule)
                await expectEvent.inTransaction(
                    r.tx,
                    t.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {rewardAccount: t.aliases[agent]}
                );

                // reward account (here: agent) should have received the deposit
                const adminPostBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentPostBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;
                assert.equal(adminPostBal.toString(), adminInitBal.toString());
                assert.notEqual(
                    agentPostBal.toString(),
                    agentInitBal.toString()
                );
            });

            it("#1.4.16a correct reward attribution for pirate period", async () => {
                const adminInitBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentInitBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;

                // drain the sender into pirate territory
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() + 1
                );

                assert.isFalse(
                    await superToken.isAccountSolventNow(t.aliases[sender])
                );

                // agent liquidates
                const r = await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });

                // the agent should get the reward
                await expectEvent.inTransaction(
                    r.tx,
                    t.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {rewardAccount: t.aliases[agent]}
                );

                // reward account (here: agent) should have received the deposit
                const adminPostBal = (
                    await superToken.realtimeBalanceOfNow(admin)
                ).availableBalance;
                const agentPostBal = (
                    await superToken.realtimeBalanceOfNow(t.aliases[agent])
                ).availableBalance;
                assert.equal(
                    adminPostBal.lt(adminInitBal),
                    true,
                    "reward account not affected by bailout as expected"
                );
                assert.notEqual(
                    agentPostBal.toString(),
                    agentInitBal.toString()
                );
            });

            it("#1.4.16b correct reward attribution for pirate period with two-way flows", async () => {
                // drain the sender into pirate territory
                await timeTravelOnce(
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() + 1
                );

                assert.isFalse(
                    await superToken.isAccountSolventNow(t.aliases[sender])
                );

                // start a reverse flow leading to a near-zero negative sender net flowrate
                await t.sf.cfa.createFlow({
                    superToken: superToken.address,
                    sender: t.aliases[receiver],
                    receiver: t.aliases[sender],
                    flowRate: FLOW_RATE1.sub(toBN(1)).toString(),
                });

                // agent liquidates
                const r = await t.sf.cfa.deleteFlow({
                    superToken: superToken.address,
                    sender: t.aliases[sender],
                    receiver: t.aliases[receiver],
                    by: t.aliases[agent],
                });

                // the agent should get the reward
                await expectEvent.inTransaction(
                    r.tx,
                    t.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {rewardAccount: t.aliases[agent]}
                );
            });
        });

        describe("#1.5 multiple flow liquidations", () => {
            beforeEach(async () => {
                await web3tx(
                    governance.setRewardAddress,
                    "set reward address to admin"
                )(superfluid.address, ZERO_ADDRESS, admin);
                // give admin some balance for liquidations
                await t.upgradeBalance("admin", t.configs.INIT_BALANCE);
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);
                await t.upgradeBalance(agent, t.configs.INIT_BALANCE);
                await shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1,
                });
                await shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver: agent,
                    flowRate: FLOW_RATE1,
                });
                t.initializePlotData(true); // observing all accounts
            });

            it("#1.5.1 should be able to liquidate multiple flows when critical", async () => {
                // NOTE: the solvencyStatuses were added due to these multi flow tests because
                // the status of the accounts vary now due to the fact that there is more than
                // a single flow which the user has
                await _testLiquidation({
                    sender,
                    receiver,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });
                await _testLiquidation({
                    sender,
                    receiver: agent,
                    by: receiver,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.5.2 should be able to liquidate multiple flows when insolvent", async () => {
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver,
                    by: agent,
                    allowCriticalAccount: true,
                    seconds: 60,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver: agent,
                    by: agent,
                    allowCriticalAccount: true,
                    seconds: 60,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.5.3 should be able to liquidate an insolvent flow and then a critical one", async () => {
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver,
                    by: agent,
                    allowCriticalAccount: true,
                    seconds: 60,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
                await _testLiquidation({
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.5.4 should be able to liquidate a critical flow and then an insolvent one", async () => {
                await _testLiquidation({
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });

                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver,
                    by: agent,
                    allowCriticalAccount: true,
                    seconds: 60,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.5.5 test agent multiple critical flow liquidations out of patrician period", async () => {
                await _testLiquidation({
                    sender,
                    receiver,
                    by: agent,
                    allowCriticalAccount: true,
                    seconds: t.configs.PATRICIAN_PERIOD + 1,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });

                await _testLiquidation({
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: t.configs.PATRICIAN_PERIOD + 1,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.5.6 test agent multiple insolvent flow liquidations out of patrician period", async () => {
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver,
                    by: agent,
                    allowCriticalAccount: true,
                    seconds: t.configs.PATRICIAN_PERIOD + 1,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });

                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: t.configs.PATRICIAN_PERIOD + 1,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.5.7 test agent critical then insolvent flow liquidations out of patrician period", async () => {
                await _testLiquidation({
                    sender,
                    receiver,
                    by: agent,
                    allowCriticalAccount: true,
                    seconds: t.configs.PATRICIAN_PERIOD + 1,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });

                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: t.configs.PATRICIAN_PERIOD + 1,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.5.8 test agent insolvent then critical flow liquidations out of patrician period", async () => {
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: t.configs.PATRICIAN_PERIOD + 1,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });

                await _testLiquidation({
                    sender,
                    receiver,
                    by: agent,
                    allowCriticalAccount: true,
                    seconds: t.configs.PATRICIAN_PERIOD + 1,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });
        });

        describe("#1.6 sender multi flow with inflow liquidations", () => {
            beforeEach(async () => {
                await web3tx(
                    governance.setRewardAddress,
                    "set reward address to admin"
                )(superfluid.address, ZERO_ADDRESS, admin);
                // give admin some balance for liquidations
                await t.upgradeBalance("admin", t.configs.INIT_BALANCE);
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);
                await t.upgradeBalance(agent, t.configs.INIT_BALANCE);
                /**
                 * Sender => Receiver @ 1.1
                 * Sender => Agent @ 1
                 * Agent => Sender @ 1
                 */
                await shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver,
                    flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10)),
                });
                await shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver: agent,
                    flowRate: FLOW_RATE1,
                });
                await shouldCreateFlow({
                    sender: agent,
                    receiver: sender,
                    superToken,
                    testenv: t,
                    flowRate: FLOW_RATE1,
                });
                t.initializePlotData(true); // observing all accounts
            });

            it("#1.6.1 should be able to liquidate multiple critical flows with an inflow", async () => {
                await _testLiquidation({
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });
                await _testLiquidation({
                    sender,
                    receiver,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.6.2 should be able to liquidate multiple insolvent flows with an inflow", async () => {
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.6.3 should be able to liquidate insolvent flow then critical flow with an inflow", async () => {
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
                await _testLiquidation({
                    sender,
                    receiver,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it("#1.6.4 should be able to liquidate critical flow then insolvent flow with an inflow", async () => {
                await _testLiquidation({
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });
            });

            it(`#1.6.5 should be able to liquidate a user with a negative
                 AB and a net flow rate of 0 (crit->crit)`, async () => {
                await _testLiquidation({
                    sender,
                    receiver,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });
                await _testLiquidation({
                    sender,
                    receiver: agent,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: true,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });
            });

            it("#1.6.6 should reject when account is not critical", async () => {
                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[receiver],
                        by: t.aliases[agent],
                    }),
                    "CFA: sender account is not critical"
                );

                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[agent],
                        by: t.aliases[receiver],
                    }),
                    "CFA: sender account is not critical"
                );

                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[agent],
                        receiver: t.aliases[sender],
                        by: t.aliases[receiver],
                    }),
                    "CFA: sender account is not critical"
                );
            });

            it("#1.6.7 should reject when account is not critical after a bailout", async () => {
                await _testLiquidation({
                    isBailout: true,
                    sender,
                    receiver,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: false,
                    },
                });

                await shouldUpdateFlow({
                    sender: agent,
                    receiver: sender,
                    superToken,
                    testenv: t,
                    flowRate: FLOW_RATE1.mul(toBN(2)),
                });

                const accountFlowInfo = await t.sf.cfa.getAccountFlowInfo({
                    superToken: superToken.address,
                    account: t.aliases[sender],
                });
                const netFlowRate = toBN(accountFlowInfo.flowRate);
                const balanceData = await superToken.realtimeBalanceOfNow(
                    t.aliases[sender]
                );

                // bring the user back to a non-critical state
                await timeTravelOnceAndVerifyAll({
                    time: Math.round(
                        balanceData.availableBalance
                            .mul(toBN(-1))
                            .div(netFlowRate)
                            .toNumber() * 1.1
                    ),
                    allowCriticalAccount: true,
                });

                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[agent],
                        by: t.aliases[receiver],
                    }),
                    "CFA: sender account is not critical"
                );
            });

            it("#1.6.8 should reject when account is not critical after a liquidation", async () => {
                await _testLiquidation({
                    sender,
                    receiver,
                    by: agent,
                    seconds: 60,
                    allowCriticalAccount: true,
                    solvencyStatuses: {
                        preIsCritical: false,
                        preIsSolvent: true,
                        postIsCritical: true,
                        postIsSolvent: true,
                    },
                });

                await shouldUpdateFlow({
                    sender: agent,
                    receiver: sender,
                    superToken,
                    testenv: t,
                    flowRate: FLOW_RATE1.mul(toBN(2)),
                });

                const accountFlowInfo = await t.sf.cfa.getAccountFlowInfo({
                    superToken: superToken.address,
                    account: t.aliases[sender],
                });
                const netFlowRate = toBN(accountFlowInfo.flowRate);
                const balanceData = await superToken.realtimeBalanceOfNow(
                    t.aliases[sender]
                );

                // bring the user back to a non-critical state
                await timeTravelOnceAndVerifyAll({
                    time: Math.round(
                        balanceData.availableBalance
                            .mul(toBN(-1))
                            .div(netFlowRate)
                            .toNumber() * 1.1
                    ),
                    allowCriticalAccount: true,
                });

                await expectRevert(
                    t.sf.cfa.deleteFlow({
                        superToken: superToken.address,
                        sender: t.aliases[sender],
                        receiver: t.aliases[agent],
                        by: t.aliases[receiver],
                    }),
                    "CFA: sender account is not critical"
                );
            });
        });

        describe("#1.7 real-time balance", () => {
            //TODO should be able to downgrade full balance
        });

        describe("#1.8 misc", () => {
            it("#1.8.1 getNetflow should return net flow rate", async () => {
                await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
                await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

                await shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender: "alice",
                    receiver: "bob",
                    flowRate: FLOW_RATE1,
                });
                await expectNetFlow("alice", FLOW_RATE1.mul(toBN(-1)));
                await expectNetFlow("bob", FLOW_RATE1);

                const flowRate2 = FLOW_RATE1.divn(3);
                await shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender: "bob",
                    receiver: "alice",
                    flowRate: flowRate2,
                });
                await expectNetFlow(
                    "alice",
                    FLOW_RATE1.mul(toBN(-1)).add(flowRate2)
                );
                await expectNetFlow("bob", FLOW_RATE1.sub(flowRate2));
            });

            it("#1.8.2 getMaximumFlowRateFromDeposit", async () => {
                const test = async (deposit) => {
                    const flowRate =
                        await cfa.getMaximumFlowRateFromDeposit.call(
                            superToken.address,
                            deposit.toString()
                        );
                    const expectedFlowRate = CFADataModel.clipDepositNumber(
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
                const maxDeposit = toBN(1).shln(95).subn(1);
                await test(maxDeposit);
                expectRevert(
                    test(maxDeposit.addn(1)),
                    "CFA: deposit number too big"
                );
            });

            it("#1.8.3 getDepositRequiredForFlowRate", async () => {
                const test = async (flowRate) => {
                    const deposit =
                        await cfa.getDepositRequiredForFlowRate.call(
                            superToken.address,
                            flowRate.toString()
                        );
                    let expectedDeposit = CFADataModel.clipDepositNumber(
                        toBN(flowRate).mul(toBN(LIQUIDATION_PERIOD))
                    );
                    expectedDeposit =
                        expectedDeposit.lt(t.configs.MINIMUM_DEPOSIT) &&
                        toBN(flowRate).gt(toBN(0))
                            ? t.configs.MINIMUM_DEPOSIT
                            : expectedDeposit;
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
                const FakeSuperfluidMock =
                    artifacts.require("FakeSuperfluidMock");
                const fakeHost = await FakeSuperfluidMock.new();
                await expectRevert(
                    fakeHost.callAgreement(
                        cfa.address,
                        cfa.contract.methods
                            .createFlow(superToken.address, bob, 1, "0x")
                            .encodeABI(),
                        {from: alice}
                    ),
                    "unauthorized host"
                );
                await expectRevert(
                    fakeHost.callAgreement(
                        cfa.address,
                        cfa.contract.methods
                            .updateFlow(superToken.address, bob, 1, "0x")
                            .encodeABI(),
                        {from: alice}
                    ),
                    "unauthorized host"
                );
                await expectRevert(
                    fakeHost.callAgreement(
                        cfa.address,
                        cfa.contract.methods
                            .deleteFlow(superToken.address, alice, bob, "0x")
                            .encodeABI(),
                        {from: alice}
                    ),
                    "unauthorized host"
                );
            });
        });

        describe("#1.10 should support different flow rates", () => {
            [
                ["small", toBN(2)],
                ["typical", FLOW_RATE1],
                ["large", toWad(42).div(toBN(3600))],
                ["maximum", MAXIMUM_FLOW_RATE.div(toBN(LIQUIDATION_PERIOD))],
            ].forEach(([label, flowRate], i) => {
                it(`#1.10.${i} should support ${label} flow rate (${flowRate})`, async () => {
                    // sufficient liquidity for the test case
                    // - it needs 1x liquidation period
                    // - it adds an additional 60 seconds as extra safe margin
                    const marginalLiquidity = flowRate.mul(toBN(60));
                    const sufficientLiquidity = BN.max(
                        MINIMUM_DEPOSIT.add(marginalLiquidity),
                        flowRate
                            .mul(toBN(LIQUIDATION_PERIOD))
                            .add(marginalLiquidity)
                    );
                    await testToken.mint(t.aliases.alice, sufficientLiquidity, {
                        from: t.aliases.alice,
                    });
                    await t.upgradeBalance(sender, sufficientLiquidity);

                    await shouldCreateFlow({
                        testenv: t,
                        superToken,
                        sender,
                        receiver,
                        flowRate: flowRate.div(toBN(2)),
                    });

                    await shouldUpdateFlow({
                        testenv: t,
                        superToken,
                        sender,
                        receiver,
                        flowRate: flowRate,
                    });

                    await timeTravelOnceAndVerifyAll({
                        allowCriticalAccount: true,
                    });
                });
            });
        });

        describe("#1.11 should properly handle minimum deposit", () => {
            beforeEach(async () => {
                // give admin some balance for liquidations
                await t.upgradeBalance(sender, t.configs.INIT_BALANCE);
            });
            const flowRateAtMinDeposit = t.configs.MINIMUM_DEPOSIT.div(
                toBN(t.configs.LIQUIDATION_PERIOD)
            );

            // calcDeposit = flowRate * t.configs.LIQUIDATION_PERIOD
            context(
                "#1.11.1 should be able to create flow where calcDeposit < min deposit.",
                () => {
                    beforeEach(async () => {
                        await shouldCreateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit.div(toBN(2)),
                        });
                    });

                    it("#1.11.1.a should handle update flow where calcDeposit < min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit.div(toBN(4)),
                        });
                    });

                    it("#1.11.1.b should be able to create flow where calcDeposit > min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit.mul(toBN(2)),
                        });
                    });

                    it("#1.11.1.c should be able to create flow where calcDeposit = min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit,
                        });
                    });
                }
            );

            context(
                "#1.11.2 should be able to create flow where calcDeposit > min deposit.",
                () => {
                    beforeEach(async () => {
                        await shouldCreateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit.mul(toBN(2)),
                        });
                    });

                    it("#1.11.2.a should handle update flow where calcDeposit < min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit.div(toBN(2)),
                        });
                    });

                    it("#1.11.2.b should be able to update flow where calcDeposit > min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit.mul(toBN(4)),
                        });
                    });

                    it("#1.11.2.c should be able to update flow where calcDeposit = min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit,
                        });
                    });
                }
            );

            context(
                "#1.11.3 should be able to create flow where calcDeposit = min deposit.",
                () => {
                    beforeEach(async () => {
                        await shouldCreateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit,
                        });
                    });

                    it("#1.11.3.a should handle update flow where calcDeposit < min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit.div(toBN(2)),
                        });
                    });

                    it("#1.11.3.b should be able to update flow where calcDeposit > min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit.mul(toBN(2)),
                        });
                    });

                    it("#1.11.3.c should be able to update flow where calcDeposit = min deposit.", async () => {
                        await shouldUpdateFlow({
                            testenv: t,
                            superToken,
                            sender,
                            receiver,
                            flowRate: flowRateAtMinDeposit,
                        });
                    });
                }
            );

            it("#1.11.4 should revert if trying to create a flow without enough minimum deposit.", async () => {
                // transfer balance - (minimumDeposit + 1 away)
                await t.transferBalance(
                    sender,
                    receiver,
                    t.configs.INIT_BALANCE.sub(
                        t.configs.MINIMUM_DEPOSIT.add(toBN(1))
                    )
                );
                await expectRevert(
                    shouldCreateFlow({
                        testenv: t,
                        superToken,
                        sender,
                        receiver,
                        flowRate: FLOW_RATE1,
                    }),
                    "CFA: not enough available balance"
                );
            });
        });
    });

    context("#2 multi flows super app scenarios", () => {
        const MultiFlowApp = artifacts.require("MultiFlowApp");

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
            return CFADataModel.clipDepositNumber(
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
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("mfa", FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1)));
            await expectNetFlow(receiver1, mfaFlowRate(FLOW_RATE1));
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                by: sender,
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
                receivers: {},
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow(sender, toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("mfa", FLOW_RATE1);
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate,
            });
            await expectNetFlow(sender, toBN(0).sub(lowFlowRate));
            await expectNetFlow("mfa", lowFlowRate);
            await timeTravelOnceAndVerifyAll();

            await shouldUpdateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate,
            });
            await expectNetFlow(sender, toBN(0).sub(highFlowRate));
            await expectNetFlow("mfa", highFlowRate);
            await timeTravelOnceAndVerifyAll();

            // fully delete everything
            await shouldDeleteFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                by: sender,
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
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                by: sender,
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
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                by: sender,
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
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: lowFlowRate,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: highFlowRate,
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
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                by: sender,
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
                        proportion: 1,
                    },
                },
            };

            await expectRevert(
                shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver: "mfa",
                    mfa,
                    flowRate: FLOW_RATE1,
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
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
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
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 0,
                    },
                },
            };
            await shouldDeleteFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                by: sender,
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
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow(
                sender,
                mfaFlowRate(FLOW_RATE1).sub(FLOW_RATE1)
            );
            await expectNetFlow("mfa", FLOW_RATE1.sub(mfaFlowRate(FLOW_RATE1)));
            await timeTravelOnceAndVerifyAll();

            // shouldDeleteFlow doesn't support loopback mode for now, let's use the sf directly
            await web3tx(
                t.sf.cfa.deleteFlow,
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
                        [1],
                    ]
                ),
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
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
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
                superToken,
                sender: "mfa",
                receiver: receiver1,
                by: receiver1,
                mfa,
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
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
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
                    by: dan,
                }),
                "CFA: sender account is not critical"
            );

            const accountFlowInfo = await t.sf.cfa.getAccountFlowInfo({
                superToken: superToken.address,
                account: t.aliases[sender],
            });
            await timeTravelOnceAndVerifyAll({
                time:
                    t.configs.INIT_BALANCE.div(FLOW_RATE1).toNumber() -
                    LIQUIDATION_PERIOD +
                    60,
                allowCriticalAccount: true,
            });

            const balanceData = await superToken.realtimeBalanceOfNow(
                t.aliases[sender]
            );
            const timeInDeficit = balanceData.availableBalance.div(
                toBN(0).sub(FLOW_RATE1)
            );

            await shouldDeleteFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                by: "dan",
                mfa,
                time: timeInDeficit,
                accountFlowInfo,
            });
            assert.isFalse(await superfluid.isAppJailed(app.address));
            await expectNetFlow(sender, "0");
            await expectNetFlow("mfa", "0");
            await expectNetFlow(receiver1, "0");
            await expectNetFlow(receiver2, "0");
            await timeTravelOnceAndVerifyAll();
            t.writePlotDataIntoCSVFile(
                this.ctx.test.title.split(" ").join("_"),
                superToken.address
            );
        });

        it("#2.11 mfa-1to1_150pct_create_full_delete_mfa_receiver_flow_by_liquidator", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE.muln(2));
            t.initializePlotData(true); // observing all accounts
            await t.transferBalance(sender, "mfa", toWad(50));

            const mfa = {
                ratioPct: 150,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
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
                    by: dan,
                }),
                "CFA: sender account is not critical"
            );

            await timeTravelOnceAndVerifyAll({
                time:
                    -toWad(50).div(mfaNetFlowRate).toNumber() -
                    LIQUIDATION_PERIOD +
                    60,
                allowCriticalAccount: true,
            });

            await web3tx(
                t.sf.cfa.deleteFlow,
                "liquidate the mfa receiver1 flow"
            )({
                superToken: superToken.address,
                sender: app.address,
                receiver: t.getAddress(receiver1),
                by: dan,
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
                t.sf.cfa.deleteFlow,
                "liquidate the mfa receiver2 flow"
            )({
                superToken: superToken.address,
                sender: app.address,
                receiver: t.getAddress(receiver2),
                by: dan,
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
                t.sf.cfa.deleteFlow,
                "liquidate the mfa sender flow"
            )({
                superToken: superToken.address,
                sender: t.getAddress(sender),
                receiver: app.address,
                by: dan,
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
            t.writePlotDataIntoCSVFile(
                this.ctx.test.title.split(" ").join("_"),
                superToken.address
            );
        });

        it("#2.12 mfa-1to2[50,50]_100pct_create-partial_delete-negative_app_balance", async () => {
            await t.upgradeBalance(sender, t.configs.INIT_BALANCE);

            let mfa = {
                ratioPct: 100,
                sender,
                receivers: {
                    [receiver1]: {
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender,
                receiver: "mfa",
                mfa,
                flowRate: FLOW_RATE1,
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
                t.sf.cfa.deleteFlow,
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
                        [1],
                    ]
                ),
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
                        proportion: 1,
                    },
                    [receiver2]: {
                        proportion: 1,
                    },
                },
            };

            await expectRevert(
                shouldCreateFlow({
                    testenv: t,
                    superToken,
                    sender,
                    receiver: "mfa",
                    mfa,
                    flowRate: FLOW_RATE1,
                }),
                "CFA: APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT"
            );
        });
    });

    context("#3 callbacks", () => {
        it("#3.1 ExclusiveInflowTestApp", async () => {
            const ExclusiveInflowTestApp = artifacts.require(
                "ExclusiveInflowTestApp"
            );
            const app = await web3tx(
                ExclusiveInflowTestApp.new,
                "ExclusiveInflowTestApp.new"
            )(cfa.address, superfluid.address);
            t.addAlias("app", app.address);

            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            await t.upgradeBalance("bob", t.configs.INIT_BALANCE);

            await web3tx(
                t.sf.cfa.createFlow,
                "alice -> app"
            )({
                superToken: superToken.address,
                sender: alice,
                receiver: app.address,
                flowRate: FLOW_RATE1.toString(),
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1).toString());
            await expectNetFlow("bob", "0");
            await expectNetFlow("app", FLOW_RATE1.toString());
            await timeTravelOnceAndValidateSystemInvariance();

            await web3tx(
                t.sf.cfa.createFlow,
                "bob -> app"
            )({
                superToken: superToken.address,
                sender: bob,
                receiver: app.address,
                flowRate: FLOW_RATE1.muln(2).toString(),
            });
            await expectNetFlow("alice", "0");
            await expectNetFlow(
                "bob",
                toBN(0).sub(FLOW_RATE1.muln(2)).toString()
            );
            await expectNetFlow("app", FLOW_RATE1.muln(2)).toString();
            await timeTravelOnceAndValidateSystemInvariance();

            await web3tx(
                t.sf.cfa.deleteFlow,
                "bob -> app"
            )({
                superToken: superToken.address,
                sender: bob,
                receiver: app.address,
            });
            await expectNetFlow("alice", "0");
            await expectNetFlow("bob", "0");
            await expectNetFlow("app", "0");
            await timeTravelOnceAndValidateSystemInvariance();
        });

        it("#3.2 NonClosableOutflowTestApp", async () => {
            const NonClosableOutflowTestApp = artifacts.require(
                "NonClosableOutflowTestApp"
            );
            const app = await web3tx(
                NonClosableOutflowTestApp.new,
                "NonClosableOutflowTestApp.new"
            )(cfa.address, superfluid.address);
            t.addAlias("app", app.address);

            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            await t.transferBalance("alice", "app", t.configs.INIT_BALANCE);

            await web3tx(app.setupOutflow, "app.setupOutflow")(
                superToken.address,
                alice,
                FLOW_RATE1.toString()
            );
            assert.equal(
                (
                    await t.sf.cfa.getFlow({
                        superToken: superToken.address,
                        sender: app.address,
                        receiver: alice,
                    })
                ).flowRate,
                FLOW_RATE1.toString()
            );
            await expectNetFlow("alice", FLOW_RATE1.toString());
            await expectNetFlow("app", toBN(0).sub(FLOW_RATE1).toString());
            await timeTravelOnceAndValidateSystemInvariance();

            await web3tx(
                t.sf.cfa.deleteFlow,
                "app -> alice by alice"
            )({
                superToken: superToken.address,
                sender: app.address,
                receiver: alice,
                by: alice,
            });
            assert.equal(
                (
                    await t.sf.cfa.getFlow({
                        superToken: superToken.address,
                        sender: app.address,
                        receiver: alice,
                    })
                ).flowRate,
                FLOW_RATE1.toString()
            );
            await expectNetFlow("alice", FLOW_RATE1.toString());
            await expectNetFlow("app", toBN(0).sub(FLOW_RATE1).toString());
            await timeTravelOnceAndValidateSystemInvariance();
        });

        it("#3.3 SelfDeletingFlowTestApp", async () => {
            const SelfDeletingFlowTestApp = artifacts.require(
                "SelfDeletingFlowTestApp"
            );
            const app = await web3tx(
                SelfDeletingFlowTestApp.new,
                "NonClosableOutflowTestApp.new"
            )(cfa.address, superfluid.address);
            t.addAlias("app", app.address);

            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            await web3tx(
                t.sf.cfa.createFlow,
                "alice -> app"
            )({
                superToken: superToken.address,
                sender: alice,
                receiver: app.address,
                flowRate: FLOW_RATE1.toString(),
            });
            assert.equal(
                (
                    await t.sf.cfa.getFlow({
                        superToken: superToken.address,
                        sender: alice,
                        receiver: app.address,
                    })
                ).flowRate,
                "0"
            );
            await expectNetFlow("alice", "0");
            await expectNetFlow("app", "0");
            await timeTravelOnceAndValidateSystemInvariance();
        });

        it("#3.4 ClosingOnUpdateFlowTestApp", async () => {
            const ClosingOnUpdateFlowTestApp = artifacts.require(
                "ClosingOnUpdateFlowTestApp"
            );
            const app = await web3tx(
                ClosingOnUpdateFlowTestApp.new,
                "ClosingOnUpdateFlowTestApp.new"
            )(cfa.address, superfluid.address);
            t.addAlias("app", app.address);

            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            await web3tx(
                t.sf.cfa.createFlow,
                "alice -> app"
            )({
                superToken: superToken.address,
                sender: alice,
                receiver: app.address,
                flowRate: FLOW_RATE1.toString(),
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1).toString());
            await expectNetFlow("bob", "0");
            await expectNetFlow("app", FLOW_RATE1.toString());
            await timeTravelOnceAndValidateSystemInvariance();

            await web3tx(
                t.sf.cfa.updateFlow,
                "alice -> app"
            )({
                superToken: superToken.address,
                sender: alice,
                receiver: app.address,
                flowRate: FLOW_RATE1.muln(2).toString(),
            });
            assert.equal(
                (
                    await t.sf.cfa.getFlow({
                        superToken: superToken.address,
                        sender: alice,
                        receiver: app.address,
                    })
                ).flowRate,
                "0"
            );
            await expectNetFlow("alice", "0");
            await expectNetFlow("app", "0");
            await timeTravelOnceAndValidateSystemInvariance();
        });

        it("#3.5 FlowExchangeTestApp", async () => {
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            const {superToken: superToken2} = await t.deployNewToken("TEST2", {
                doUpgrade: true,
                isTruffle: true,
            });
            const FlowExchangeTestApp = artifacts.require(
                "FlowExchangeTestApp"
            );
            const app = await web3tx(
                FlowExchangeTestApp.new,
                "FlowExchangeTestApp.new"
            )(cfa.address, superfluid.address, superToken2.address);
            t.addAlias("app", app.address);

            await expectRevert(
                web3tx(
                    t.sf.cfa.createFlow,
                    "alice -> app"
                )({
                    superToken: superToken.address,
                    sender: alice,
                    receiver: app.address,
                    flowRate: FLOW_RATE1.toString(),
                }),
                "CFA: not enough available balance."
            );

            // fund the app with
            await superToken2.transfer(app.address, t.configs.INIT_BALANCE, {
                from: alice,
            });
            await web3tx(
                t.sf.cfa.createFlow,
                "alice -> app"
            )({
                superToken: superToken.address,
                sender: alice,
                receiver: app.address,
                flowRate: FLOW_RATE1.toString(),
            });
            let flow1, flow2;
            flow1 = await cfa.getFlow(superToken.address, alice, app.address);
            flow2 = await cfa.getFlow(superToken2.address, app.address, alice);
            const deposit = CFADataModel.clipDepositNumber(
                FLOW_RATE1.muln(LIQUIDATION_PERIOD)
            ).toString();
            console.log(
                "Flow: alice -> app (token1)",
                flow1.flowRate.toString(),
                flow1.deposit.toString(),
                flow1.owedDeposit.toString()
            );
            assert.equal(flow1.flowRate.toString(), FLOW_RATE1.toString());
            assert.equal(flow1.deposit.toString(), deposit);
            assert.equal(flow1.owedDeposit.toString(), "0");
            console.log(
                "Flow: app -> alice (token2)",
                flow2.flowRate.toString(),
                flow2.deposit.toString(),
                flow2.owedDeposit.toString()
            );
            assert.equal(flow2.flowRate.toString(), FLOW_RATE1.toString());
            assert.equal(flow2.deposit.toString(), deposit);
            assert.equal(flow2.owedDeposit.toString(), "0");
            console.log(
                "App balances",
                (await superToken.balanceOf(app.address)).toString(),
                (await superToken2.balanceOf(app.address)).toString()
            );

            await timeTravelOnceAndValidateSystemInvariance();

            await web3tx(
                t.sf.cfa.deleteFlow,
                "alice -> app"
            )({
                superToken: superToken.address,
                sender: alice,
                receiver: app.address,
            });
            flow1 = await cfa.getFlow(superToken.address, alice, app.address);
            flow2 = await cfa.getFlow(superToken2.address, app.address, alice);
            console.log(
                "Flow: alice -> app (token1)",
                flow1.flowRate.toString(),
                flow1.deposit.toString(),
                flow1.owedDeposit.toString()
            );
            assert.equal(flow1.flowRate.toString(), "0");
            assert.equal(flow1.deposit.toString(), "0");
            assert.equal(flow1.owedDeposit.toString(), "0");
            console.log(
                "Flow: app -> alice (token2)",
                flow2.flowRate.toString(),
                flow2.deposit.toString(),
                flow2.owedDeposit.toString()
            );
            assert.equal(flow2.flowRate.toString(), FLOW_RATE1.toString());
            assert.equal(flow2.deposit.toString(), deposit);
            assert.equal(flow2.owedDeposit.toString(), "0");
            console.log(
                "App balances",
                (await superToken.balanceOf(app.address)).toString(),
                (await superToken2.balanceOf(app.address)).toString()
            );
            assert.equal(
                toBN(await superToken2.balanceOf(app.address))
                    .add(toBN(await superToken2.balanceOf(alice)))
                    .add(toBN(deposit))
                    .toString(),
                t.configs.INIT_BALANCE.toString()
            );

            await timeTravelOnceAndValidateSystemInvariance();
            assert.isFalse(
                await t.contracts.superfluid.isAppJailed(app.address)
            );
        });
    });

    context("#10 scenarios", () => {
        it("#10.1 two accounts sending to each other with the same flow rate", async () => {
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow("alice", FLOW_RATE1.mul(toBN(-1)));
            await expectNetFlow("bob", FLOW_RATE1);
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "bob",
                receiver: "alice",
                flowRate: FLOW_RATE1,
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
                superToken,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("bob", FLOW_RATE1);
            await expectNetFlow("carol", "0");
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "bob",
                receiver: "carol",
                flowRate: flowRateBC,
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1));
            await expectNetFlow("bob", FLOW_RATE1.sub(flowRateBC));
            await expectNetFlow("carol", flowRateBC);
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "carol",
                receiver: "alice",
                flowRate: flowRateCA,
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
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1.muln(2)));
            await expectNetFlow("bob", FLOW_RATE1);
            await expectNetFlow("carol", FLOW_RATE1);
            await expectNetFlow("dan", "0");
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "bob",
                receiver: "dan",
                flowRate: flowRateBD,
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1.muln(2)));
            await expectNetFlow("bob", FLOW_RATE1.sub(flowRateBD));
            await expectNetFlow("carol", FLOW_RATE1);
            await expectNetFlow("dan", flowRateBD);
            await timeTravelOnceAndVerifyAll();

            await shouldCreateFlow({
                testenv: t,
                superToken,
                sender: "dan",
                receiver: "carol",
                flowRate: flowRateDC,
            });
            await expectNetFlow("alice", toBN(0).sub(FLOW_RATE1.muln(2)));
            await expectNetFlow("bob", FLOW_RATE1.sub(flowRateBD));
            await expectNetFlow("carol", FLOW_RATE1.add(flowRateDC));
            await expectNetFlow("dan", flowRateBD.sub(flowRateDC));
            await timeTravelOnceAndVerifyAll();
        });

        it("#10.4 ctx should not be exploited", async () => {
            await expectRevert(
                superfluid.callAgreement(
                    cfa.address,
                    cfa.contract.methods
                        .createFlow(
                            superToken.address,
                            alice,
                            FLOW_RATE1,
                            web3.eth.abi.encodeParameters(
                                ["bytes", "bytes"],
                                ["0xdeadbeef", "0x"]
                            )
                        )
                        .encodeABI(),
                    "0x",
                    {
                        from: alice,
                    }
                ),
                "invalid ctx"
            );
        });
    });
});
