const { expectRevert } = require("@openzeppelin/test-helpers");
const {
    web3tx,
    //wad4human,
    toWad,
    toBN
} = require("@decentral.ee/web3-helpers");

//const traveler = require("ganache-time-traveler");

const FLOW_RATE1 = toWad("1").div(toBN(3600)); // 1 per hour


async function _shouldChangeFlow({
    fn,
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    const { superToken, governance } = testenv.contracts;
    const LIQUIDATION_PERIOD = (await governance.getLiquidationPeriod(superToken.address)).toNumber();
    const expectedDeposit = toBN(flowRate)
        .mul(toBN(LIQUIDATION_PERIOD));
    const expectedOwedDeposit = toBN(expectedDeposit)
        .mul(toBN(expectedOwedDepositRatio || 0))
        .toString();
    const flowId = {
        superToken: superToken.address,
        sender: testenv.aliases[sender],
        receiver: testenv.aliases[receiver],
    };

    const senderBalance1 = await superToken.realtimeBalanceOfNow(testenv.aliases[sender]);

    const tx = await web3tx(
        testenv.sf.cfa[fn].bind(testenv.sf.cfa),
        `${fn} from ${sender} to ${receiver}`
    )({
        ...flowId,
        flowRate: flowRate.toString(),
    });

    const senderBalance2 = await superToken.realtimeBalanceOfNow(testenv.aliases[sender]);

    const flowInfo = await testenv.sf.cfa.getFlow({
        ...flowId
    });
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
        expectedDeposit.toString(),
        "wrong deposit amount of the flow");
    assert.equal(
        flowInfo.owedDeposit,
        expectedOwedDeposit,
        "wrong owed deposit aount of the flow");

    assert.equal(
        toBN(senderBalance2.deposit).sub(toBN(senderBalance1.deposit)).toString(),
        expectedDeposit.toString(),
        "wrong deposit amount of sender");

    assert.equal(
        toBN(senderBalance2.owedDeposit).sub(toBN(senderBalance1.owedDeposit)).toString(),
        expectedOwedDeposit,
        "wrong deposit amount of sender");
}

async function shouldCreateFlow({
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    const { superToken } = testenv.contracts;
    const senderNetFlow1 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[sender],
    });
    const receiverNetFlow1 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[receiver],
    });

    await _shouldChangeFlow({
        fn: "createFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        expectedOwedDepositRatio
    });

    const senderNetFlow2 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[sender],
    });
    const receiverNetFlow2 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[receiver],
    });

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
    const { superToken } = testenv.contracts;
    const oldFlowInfo = await testenv.sf.cfa.getFlow({
        superToken: superToken.address,
        sender: testenv.aliases[sender],
        receiver: testenv.aliases[receiver],
    });
    const senderNetFlow1 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[sender],
    });
    const receiverNetFlow1 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[receiver],
    });

    await _shouldChangeFlow({
        fn: "updateFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        expectedOwedDepositRatio
    });

    const senderNetFlow2 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[sender],
    });
    const receiverNetFlow2 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[receiver],
    });

    const flowRateDelta = toBN(flowRate).sub(toBN(oldFlowInfo.flowRate)).toString();
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
    const { superToken } = testenv.contracts;
    const oldFlowInfo = await testenv.sf.cfa.getFlow({
        superToken: superToken.address,
        sender: testenv.aliases[sender],
        receiver: testenv.aliases[receiver],
    });
    const senderNetFlow1 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[sender],
    });
    const receiverNetFlow1 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[receiver],
    });

    await _shouldChangeFlow({
        fn: "deleteFlow",
        testenv,
        sender,
        receiver,
        flowRate: "0",
    });

    const senderNetFlow2 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[sender],
    });
    const receiverNetFlow2 = await testenv.sf.cfa.getNetFlow({
        superToken: superToken.address,
        account: testenv.aliases[receiver],
    });

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

    let superToken;

    beforeEach(async function () {
        ({
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

        it(`${prefix}.1.2 reject when there is zero balance`, async () => {
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: FLOW_RATE1.toString()
            }), "CFA: not enough available balance");
        });

        it(`${prefix}.1.3 should reject when zero flowrate`, async () => {
            await expectRevert(testenv.sf.cfa.createFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.bob,
                flowRate: "0"
            }), "CFA: invalid flow rate");
        });

        it(`${prefix}.1.4 should reject when negative flowrate`, async () => {
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
            }), "CFA: flow doesn't exist");
        });

        it(`${prefix}.2.7 should not update non existing flow (self flow)`, async () => {
            await expectRevert(testenv.sf.cfa.updateFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.alice,
                flowRate: FLOW_RATE1.toString(),
            }), "CFA: flow doesn't exist");
        });
    });

    describe(`${prefix}.3 deleteFlow`, () => {
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

        it(`${prefix}.3.3 should note delete non-existing flow`, async () => {
            await expectRevert(testenv.sf.cfa.deleteFlow({
                superToken: superToken.address,
                sender: testenv.aliases.alice,
                receiver: testenv.aliases.dan,
            }), "CFA: flow doesn't exist");
        });
    });

}

module.exports = {
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldBehaveLikeCFAv1
};
