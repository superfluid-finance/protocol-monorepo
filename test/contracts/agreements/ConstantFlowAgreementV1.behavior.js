const _ = require("lodash");
const { BN, expectRevert } = require("@openzeppelin/test-helpers");
const {
    web3tx,
    //wad4human,
    toWad,
    toBN
} = require("@decentral.ee/web3-helpers");

//const traveler = require("ganache-time-traveler");


const FLOW_RATE1 = toWad("1").div(toBN(3600)); // 1 per hour
const MAXIMUM_FLOW_RATE = toBN(2).pow(toBN(95)).sub(toBN(1));
const MINIMAL_DEPOSIT = toBN(1).shln(32);

function clipDepositNumber(deposit) {
    // last 32 bites of the deposit (96 bites) is clipped off
    // and minimal deposit is (1<<32)
    return BN.max(toBN(1), deposit.shrn(32)).shln(32);
}
function clipOwedDepositNumber(deposit) {
    // last 32 bites of the deposit (96 bites) is clipped off
    return deposit.shrn(32).shln(32);
}

function _updateFlowInfo(root, sender, receiver, flowInfo) {
    _.merge(root, {
        cfa: {
            flows: {
                [`${sender}:${receiver}`]: flowInfo
            }
        }
    });
}

function getFlowInfo(root, sender, receiver) {
    _.defaultsDeep(root, {
        cfa: {
            flows: {
                [`${sender}:${receiver}`]: {
                    timestamp: 0,
                    flowRate: 0,
                    deposit: 0,
                    owedDeposit: 0
                }
            }
        }
    });
    return _.clone(root.cfa.flows[`${sender}:${receiver}`]);
}

function _updateAccountNetFlow(root, account, netFlow) {
    _.merge(root, {
        accounts: {
            [account]: {
                cfa: {
                    netFlow
                }
            }
        }
    });
}

function getAccountNetFlow(root, account) {
    _.defaultsDeep(root, {
        accounts: {
            [account]: {
                cfa: {
                    netFlow: 0
                }
            }
        }
    });
    return root.accounts[account].cfa.netFlow;
}

async function _shouldChangeFlow({
    fn,
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    const { superToken } = testenv.contracts;
    const flowId = {
        superToken: superToken.address,
        sender: testenv.aliases[sender],
        receiver: testenv.aliases[receiver],
    };
    const oldFlowInfo = await testenv.sf.cfa.getFlow(flowId);
    let expectedNewDeposit, expectedNewOwedDeposit;
    if (fn !== "deleteFlow") {
        expectedNewDeposit = clipDepositNumber(toBN(flowRate)
            .mul(toBN(testenv.configs.LIQUIDATION_PERIOD)));
        expectedNewOwedDeposit = clipOwedDepositNumber(toBN(expectedNewDeposit)
            .mul(toBN(expectedOwedDepositRatio || 0)));
    } else {
        expectedNewDeposit = toBN(0);
        expectedNewOwedDeposit = toBN(0);
    }

    const senderBalance1 = await superToken.realtimeBalanceOfNow(testenv.aliases[sender]);
    const receiverBalance1 = await superToken.realtimeBalanceOfNow(testenv.aliases[receiver]);
    console.log("sender balance before",
        senderBalance1.availableBalance.toString(),
        senderBalance1.deposit.toString(),
        senderBalance1.owedDeposit.toString());
    console.log("receiver balance before",
        receiverBalance1.availableBalance.toString(),
        receiverBalance1.deposit.toString(),
        receiverBalance1.owedDeposit.toString());

    const tx = await web3tx(
        testenv.sf.cfa[fn].bind(testenv.sf.cfa),
        `${fn} from ${sender} to ${receiver}`
    )({
        ...flowId,
        flowRate: flowRate.toString(),
    });

    const senderBalance2 = await superToken.realtimeBalanceOfNow(testenv.aliases[sender]);
    const receiverBalance2 = await superToken.realtimeBalanceOfNow(testenv.aliases[receiver]);
    console.log("sender balance after",
        senderBalance2.availableBalance.toString(),
        senderBalance2.deposit.toString(),
        senderBalance2.owedDeposit.toString());
    console.log("receiver balance after",
        receiverBalance2.availableBalance.toString(),
        receiverBalance2.deposit.toString(),
        receiverBalance2.owedDeposit.toString());

    const flowInfo = await testenv.sf.cfa.getFlow(flowId);
    if (fn !== "deleteFlow") {
        assert.equal(
            flowInfo.timestamp.getTime()/1000,
            (await web3.eth.getBlock(tx.receipt.blockNumber)).timestamp,
            "wrong flow timestamp of the flow");
    } else {
        assert.equal(flowInfo.timestamp.getTime(), 0);
    }
    assert.equal(
        flowInfo.flowRate,
        flowRate.toString(),
        "wrong flowrate of the flow");
    assert.equal(
        flowInfo.deposit,
        expectedNewDeposit.toString(),
        "wrong deposit amount of the flow");
    assert.equal(
        flowInfo.owedDeposit,
        expectedNewOwedDeposit.toString(),
        "wrong owed deposit aount of the flow");

    assert.equal(
        toBN(senderBalance2.deposit).sub(toBN(senderBalance1.deposit)).toString(),
        expectedNewDeposit.sub(toBN(oldFlowInfo.deposit)).toString(),
        "wrong deposit amount of sender");
    assert.equal(
        toBN(senderBalance2.owedDeposit).sub(toBN(senderBalance1.owedDeposit)).toString(),
        expectedNewOwedDeposit.sub(toBN(oldFlowInfo.owedDeposit)).toString(),
        "wrong owed deposit amount of sender");

    assert.equal(
        toBN(receiverBalance2.deposit).sub(toBN(receiverBalance1.deposit)).toString(),
        "0",
        "wrong deposit amount of receiver");
    assert.equal(
        toBN(receiverBalance2.owedDeposit).sub(toBN(receiverBalance1.owedDeposit)).toString(),
        "0",
        "wrong owed deposit amount of receiver");

    // store test data
    _updateAccountNetFlow(
        testenv.data,
        testenv.aliases[sender],
        await testenv.sf.cfa.getNetFlow({
            superToken: superToken.address,
            account: testenv.aliases[sender],
        })
    );
    _updateAccountNetFlow(
        testenv.data,
        testenv.aliases[receiver],
        await testenv.sf.cfa.getNetFlow({
            superToken: superToken.address,
            account: testenv.aliases[receiver],
        })
    );
    _updateFlowInfo(
        testenv.data,
        testenv.aliases[sender],
        testenv.aliases[receiver],
        flowInfo);
}

async function shouldCreateFlow({
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    const senderNetFlow1 = getAccountNetFlow(testenv.data, testenv.aliases[sender]);
    const receiverNetFlow1 = getAccountNetFlow(testenv.data, testenv.aliases[receiver]);

    await _shouldChangeFlow({
        fn: "createFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        expectedOwedDepositRatio
    });

    const senderNetFlow2 = getAccountNetFlow(testenv.data, testenv.aliases[sender]);
    const receiverNetFlow2 = getAccountNetFlow(testenv.data, testenv.aliases[receiver]);

    assert.equal(
        toBN(senderNetFlow1).sub(toBN(senderNetFlow2)).toString(),
        flowRate.toString()
    );
    assert.equal(
        toBN(receiverNetFlow2).sub(toBN(receiverNetFlow1)).toString(),
        flowRate.toString()
    );
}

async function shouldUpdateFlow({
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    const senderNetFlow1 = getAccountNetFlow(testenv.data, testenv.aliases[sender]);
    const receiverNetFlow1 = getAccountNetFlow(testenv.data, testenv.aliases[receiver]);
    const oldFlowInfo = getFlowInfo(
        testenv.data,
        testenv.aliases[sender],
        testenv.aliases[receiver]);
    const flowRateDelta = toBN(flowRate).sub(toBN(oldFlowInfo.flowRate)).toString();

    await _shouldChangeFlow({
        fn: "updateFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        expectedOwedDepositRatio
    });

    const senderNetFlow2 = getAccountNetFlow(testenv.data, testenv.aliases[sender]);
    const receiverNetFlow2 = getAccountNetFlow(testenv.data, testenv.aliases[receiver]);

    assert.equal(
        toBN(senderNetFlow1).sub(toBN(senderNetFlow2)).toString(),
        flowRateDelta
    );
    assert.equal(
        toBN(receiverNetFlow2).sub(toBN(receiverNetFlow1)).toString(),
        flowRateDelta
    );
}

async function shouldDeleteFlow({
    testenv,
    sender,
    receiver
}) {
    const senderNetFlow1 = getAccountNetFlow(testenv.data, testenv.aliases[sender]);
    const receiverNetFlow1 = getAccountNetFlow(testenv.data, testenv.aliases[receiver]);
    const oldFlowInfo = getFlowInfo(
        testenv.data,
        testenv.aliases[sender],
        testenv.aliases[receiver]);

    await _shouldChangeFlow({
        fn: "deleteFlow",
        testenv,
        sender,
        receiver,
        flowRate: "0",
    });

    const senderNetFlow2 = getAccountNetFlow(testenv.data, testenv.aliases[sender]);
    const receiverNetFlow2 = getAccountNetFlow(testenv.data, testenv.aliases[receiver]);

    const flowRateDelta = toBN(0).sub(toBN(oldFlowInfo.flowRate)).toString();
    assert.equal(
        toBN(senderNetFlow1).sub(toBN(senderNetFlow2)).toString(),
        flowRateDelta
    );
    assert.equal(
        toBN(receiverNetFlow2).sub(toBN(receiverNetFlow1)).toString(),
        flowRateDelta
    );
}

// Testing the behavior of CFA v1
//
// NOTE:
//   - assuming all accounts start with super token balance of zero
//
function shouldBehaveLikeCFAv1({prefix, testenv}) {

    let testToken;
    let superToken;

    beforeEach(async function () {
        ({
            testToken,
            superToken,
        } = testenv.contracts);
    });

    describe(`${prefix}.1 createFlow`, () => {
        it(`${prefix}.1.1 should create when there is enough balance`, async () => {
            await superToken.upgrade(testenv.configs.INIT_BALANCE, { from: testenv.aliases.alice });
            await shouldCreateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
        });

        it(`${prefix}.1.2 should reject when there is not enough balance`, async () => {
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: FLOW_RATE1.toString()
            }), "CFA: not enough available balance");
        });

        it(`${prefix}.1.3 should reject when zero flow rate`, async () => {
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: "0"
            }), "CFA: invalid flow rate");
        });

        it(`${prefix}.1.4 should reject when negative flow rate`, async () => {
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: "-1"
            }), "CFA: invalid flow rate");
        });

        it(`${prefix}.1.5 should reject when self flow`, async () => {
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.alice,
                flowRate: FLOW_RATE1.toString()
            }), "CFA: no self flow");
        });

        it(`${prefix}.1.6 should not create same flow`, async () => {
            await superToken.upgrade(testenv.configs.INIT_BALANCE, { from: testenv.aliases.alice });
            await shouldCreateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: FLOW_RATE1.toString()
            }), "CFA: flow already exist");
        });

        it(`${prefix}.1.7 should reject when overflow flow rate`, async () => {
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: MAXIMUM_FLOW_RATE.toString(),
            }), "Int96SafeMath: multiplication overflow");
        });

        it(`${prefix}.1.8 should reject when receiver is zero address`, async () => {
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.constants.ZERO_ADDRESS,
                flowRate: FLOW_RATE1.toString(),
            }), "CFA: receiver is zero");
        });
    });

    describe(`${prefix}.2 updateFlow`, () => {
        beforeEach(async () => {
            await superToken.upgrade(testenv.configs.INIT_BALANCE, { from: testenv.aliases.alice });
            await shouldCreateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
        });

        it(`${prefix}.2.1 can maintain existing flowrate`, async () => {
            await shouldUpdateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
        });

        it(`${prefix}.2.2 can increase (+10%) existing flowrate`, async () => {
            await shouldUpdateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10)),
            });
        });

        it(`${prefix}.2.3 can decrease (-10%) existing flowrate`, async () => {
            await shouldUpdateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1.mul(toBN(9)).div(toBN(10)),
            });
        });

        it(`${prefix}.2.4 should not update with zero flowrate`, async () => {
            await expectRevert(testenv.sf.cfa.updateFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: "0"
            }), "CFA: invalid flow rate");
        });

        it(`${prefix}.2.5 should not update with negative flowrate`, async () => {
            await expectRevert(testenv.sf.cfa.updateFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: "-1"
            }), "CFA: invalid flow rate");
        });

        it(`${prefix}.2.6 should not update non existing flow`, async () => {
            await expectRevert(testenv.sf.cfa.updateFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.dan,
                flowRate: FLOW_RATE1.toString(),
            }), "CFA: flow does not exist");
        });

        it(`${prefix}.2.7 should not update non existing flow (self flow)`, async () => {
            await expectRevert(testenv.sf.cfa.updateFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.alice,
                flowRate: FLOW_RATE1.toString(),
            }), "CFA: no self flow");
        });

        it(`${prefix}.2.8 should reject when there is not enough balance`, async () => {
            await expectRevert(testenv.sf.cfa.updateFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: toBN(testenv.configs.INIT_BALANCE)
                    .div(toBN(testenv.configs.LIQUIDATION_PERIOD).sub(toBN(60)))
                    .toString()
            }), "CFA: not enough available balance");
        });

        it(`${prefix}.2.9 should reject when overflow flow rate`, async () => {
            await expectRevert(testenv.sf.cfa.updateFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: MAXIMUM_FLOW_RATE.toString(),
            }), "Int96SafeMath: multiplication overflow");
        });

        it(`${prefix}.2.10 should reject when receiver is zero address`, async () => {
            await expectRevert(testenv.sf.cfa.updateFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.constants.ZERO_ADDRESS,
                flowRate: FLOW_RATE1.toString(),
            }), "CFA: receiver is zero");
        });
    });

    describe(`${prefix}.3 deleteFlow (non liquidation)`, () => {
        beforeEach(async () => {
            await superToken.upgrade(testenv.configs.INIT_BALANCE, { from: testenv.aliases.alice });
            await shouldCreateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
        });

        it(`${prefix}.3.1 can delete existing flow`, async () => {
            await shouldDeleteFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
            });
        });

        it(`${prefix}.3.2 can delete an updated flow`, async () => {
            await shouldUpdateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1.mul(toBN(11)).div(toBN(10)),
            });
            await shouldDeleteFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
            });
        });

        it(`${prefix}.3.3 should not delete non-existing flow`, async () => {
            await expectRevert(testenv.sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.dan,
            }), "CFA: flow does not exist");
        });

        it(`${prefix}.3.4 should reject when receiver is zero address`, async () => {
            await expectRevert(testenv.sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.constants.ZERO_ADDRESS,
            }), "CFA: receiver is zero");
        });

        it(`${prefix}.3.5 should reject when sender is zero address`, async () => {
            await expectRevert(testenv.sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: testenv.constants.ZERO_ADDRESS,
                receiver: testenv.aliases.dan,
                by: testenv.aliases.alice
            }), "CFA: sender is zero");
        });
    });

    describe(`${prefix}.4 deleteFlow (liquidations)`, () => {
        beforeEach(async () => {
            await superToken.upgrade(testenv.configs.INIT_BALANCE, { from: testenv.aliases.alice });
            await shouldCreateFlow({
                testenv,
                sender: "alice",
                receiver: "bob",
                flowRate: FLOW_RATE1,
            });
        });

        it(`${prefix}.4.1 should reject when sender is zero address`, async () => {
            await expectRevert(testenv.sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: testenv.constants.ZERO_ADDRESS,
                receiver: testenv.aliases.dan,
                by: testenv.aliases.alice
            }), "CFA: sender is zero");
        });
    });

    describe(`${prefix}.5 should support different flow rates`, () => {
        [
            ["small", toBN(2)],
            ["typical", FLOW_RATE1],
            ["large", toWad(42).div(toBN(3600))],
            ["maximum", MAXIMUM_FLOW_RATE.div(toBN(testenv.configs.LIQUIDATION_PERIOD))]
        ].forEach(([label, flowRate], i) => {
            it(`${prefix}.5.${i} should support ${label} flow rate (${flowRate})`, async () => {
                // sufficient liquidity for the test case
                // - it needs 1x liquidation period
                // - it adds an additional 60 seconds as extra safe margin
                const marginalLiquidity = flowRate.mul(toBN(60));
                const sufficientLiquidity = BN.max(
                    MINIMAL_DEPOSIT.add(marginalLiquidity),
                    flowRate
                        .mul(toBN(testenv.configs.LIQUIDATION_PERIOD))
                        .add(marginalLiquidity)
                );
                await testToken.mint(testenv.aliases.alice, sufficientLiquidity, {
                    from: testenv.aliases.alice
                });
                await superToken.upgrade(sufficientLiquidity, {
                    from: testenv.aliases.alice
                });

                await shouldCreateFlow({
                    testenv,
                    sender: "alice",
                    receiver: "bob",
                    flowRate: flowRate.div(toBN(2)),
                });

                await shouldUpdateFlow({
                    testenv,
                    sender: "alice",
                    receiver: "bob",
                    flowRate: flowRate,
                });
            });
        });
    });
}

module.exports = {
    getFlowInfo,
    getAccountNetFlow,
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
    shouldBehaveLikeCFAv1,
};
