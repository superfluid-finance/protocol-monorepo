const _ = require("lodash");
const { BN } = require("@openzeppelin/test-helpers");
const {
    web3tx,
    toBN
} = require("@decentral.ee/web3-helpers");

function clipDepositNumber(deposit) {
    // last 32 bites of the deposit (96 bites) is clipped off
    // and minimal deposit is (1<<32)
    return BN.max(toBN(1), deposit.shrn(32)).shln(32);
}
function clipOwedDepositNumber(deposit) {
    // last 32 bites of the deposit (96 bites) is clipped off
    return deposit.shrn(32).shln(32);
}

function _realtimeBalance(availableBalance, deposit, owedDeposit) {
    return toBN(availableBalance)
        .add(BN.max(toBN(0), toBN(deposit).sub(toBN(owedDeposit))));
}

function _updateFlowInfo({
    testenv,
    superToken,
    sender,
    receiver,
    flowInfo
}) {
    _.merge(testenv.data, {
        tokens: {
            [superToken]: {
                cfa: {
                    flows: {
                        [`${sender}:${receiver}`]: {
                            timestamp: flowInfo.timestamp,
                            flowRate: flowInfo.flowRate,
                            deposit: flowInfo.deposit,
                            owedDeposit: flowInfo.owedDeposit
                        }
                    }
                }
            }
        }
    });
}

function getFlowInfo({
    testenv,
    superToken,
    sender,
    receiver
}) {
    _.defaultsDeep(testenv.data, {
        tokens: {
            [superToken]: {
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
            }
        }
    });
    return _.clone(testenv.data.tokens[superToken].cfa.flows[`${sender}:${receiver}`]);
}

function _updateAccountFlowInfo({
    testenv,
    superToken,
    account,
    flowInfo
}) {
    _.merge(testenv.data, {
        tokens: {
            [superToken]: {
                accounts: {
                    [account]: {
                        cfa: {
                            flowInfo: {
                                timestamp: flowInfo.timestamp,
                                flowRate: flowInfo.flowRate,
                                deposit: flowInfo.deposit,
                                owedDeposit: flowInfo.owedDeposit
                            }
                        }
                    }
                }
            }
        }
    });
}

function getAccountFlowInfo({
    testenv,
    superToken,
    account
}) {
    _.defaultsDeep(testenv.data, {
        tokens: {
            [superToken]: {
                accounts: {
                    [account]: {
                        cfa: {
                            flowInfo: {
                                timestamp: 0,
                                flowRate: 0,
                                deposit: 0,
                                owedDeposit: 0,
                            }
                        }
                    }
                }
            }
        }
    });
    return _.clone(testenv.data.tokens[superToken].accounts[account].cfa.flowInfo);
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

    const senderAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[sender]
    });
    const receiverAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[receiver]
    });

    const senderBalanceSnapshot1 = testenv.getAccountBalanceSnapshot({
        superToken: superToken.address,
        account: testenv.aliases[sender]
    });
    const receiverBalanceSnapshot1 = testenv.getAccountBalanceSnapshot({
        superToken: superToken.address,
        account: testenv.aliases[receiver]
    });
    console.log("sender balance snapshot before",
        senderBalanceSnapshot1.availableBalance.toString(),
        senderBalanceSnapshot1.deposit.toString(),
        senderBalanceSnapshot1.owedDeposit.toString());
    console.log("receiver balance snapshot before",
        receiverBalanceSnapshot1.availableBalance.toString(),
        receiverBalanceSnapshot1.deposit.toString(),
        receiverBalanceSnapshot1.owedDeposit.toString());

    let flowInfo;
    let senderBalance2;
    let receiverBalance2;
    if (fn !== "noop") {
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
        //const txBlock = await web3.eth.getBlock(tx.receipt.blockNumber);

        senderBalance2 = await superToken.realtimeBalanceOfNow(testenv.aliases[sender]);
        receiverBalance2 = await superToken.realtimeBalanceOfNow(testenv.aliases[receiver]);
        console.log("sender balance after",
            senderBalance2.availableBalance.toString(),
            senderBalance2.deposit.toString(),
            senderBalance2.owedDeposit.toString());
        console.log("receiver balance after",
            receiverBalance2.availableBalance.toString(),
            receiverBalance2.deposit.toString(),
            receiverBalance2.owedDeposit.toString());

        // validate flow info changes
        flowInfo = await testenv.sf.cfa.getFlow(flowId);
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

        // validate sender deposit changes and account flow info change
        const senderDepositDelta = toBN(senderBalance2.deposit)
            .sub(toBN(senderBalance1.deposit));
        const senderOwedDepositDelta = toBN(senderBalance2.owedDeposit)
            .sub(toBN(senderBalance1.owedDeposit));
        assert.equal(
            senderDepositDelta.toString(),
            expectedNewDeposit.sub(toBN(oldFlowInfo.deposit)).toString(),
            "wrong deposit amount of sender");
        assert.equal(
            senderOwedDepositDelta.toString(),
            expectedNewOwedDeposit.sub(toBN(oldFlowInfo.owedDeposit)).toString(),
            "wrong owed deposit amount of sender");

        // validate receiver deposit changes and account flow info change
        const receiverDepositDelta = toBN(receiverBalance2.deposit)
            .sub(toBN(receiverBalance1.deposit));
        const receiverOwedDepositDelta = toBN(receiverBalance2.owedDeposit)
            .sub(toBN(receiverBalance1.owedDeposit));
        assert.equal(
            receiverDepositDelta.toString(),
            "0",
            "wrong deposit amount of receiver");
        assert.equal(
            receiverOwedDepositDelta.toString(),
            "0",
            "wrong owed deposit amount of receiver");
    } else {
        senderBalance2 = await superToken.realtimeBalanceOfNow(testenv.aliases[sender]);
        receiverBalance2 = await superToken.realtimeBalanceOfNow(testenv.aliases[receiver]);
        console.log("sender balance snapshot after",
            senderBalance2.availableBalance.toString(),
            senderBalance2.deposit.toString(),
            senderBalance2.owedDeposit.toString());
        console.log("receiver balance snapshot after",
            receiverBalance2.availableBalance.toString(),
            receiverBalance2.deposit.toString(),
            receiverBalance2.owedDeposit.toString());
    }

    // validate sender balance snapshot changes
    const senderRealtimeBalanceDelta = _realtimeBalance(
        senderBalance2.availableBalance,
        senderBalance2.deposit,
        senderBalance2.owedDeposit
    ).sub(_realtimeBalance(
        senderBalanceSnapshot1.availableBalance,
        senderBalanceSnapshot1.deposit,
        senderBalanceSnapshot1.owedDeposit
    ));
    assert.equal(
        senderRealtimeBalanceDelta.toString(),
        toBN(senderAccountFlow1.flowRate)
            .mul(
                toBN(senderBalance2.timestamp.toString())
                    .sub(toBN(senderBalanceSnapshot1.timestamp.toString()))
            )
            .toString(),
        "wrong available balance changes of sender");

    // validate receiver balance snapshot changes
    const receiverRealtimeBalanceDelta = _realtimeBalance(
        receiverBalance2.availableBalance,
        receiverBalance2.deposit,
        receiverBalance2.owedDeposit
    ).sub(_realtimeBalance(
        receiverBalanceSnapshot1.availableBalance,
        receiverBalanceSnapshot1.deposit,
        receiverBalanceSnapshot1.owedDeposit
    ));
    assert.equal(
        receiverRealtimeBalanceDelta.toString(),
        toBN(receiverAccountFlow1.flowRate)
            .mul(
                toBN(receiverBalance2.timestamp.toString())
                    .sub(toBN(receiverBalanceSnapshot1.timestamp.toString()))
            )
            .toString(),
        "wrong available balance changes of receiver");

    // update balance snapshots
    testenv.updateAccountBalanceSnapshot({
        superToken: superToken.address,
        account: testenv.aliases[sender],
        balanceSnapshot: senderBalance2
    });
    testenv.updateAccountBalanceSnapshot({
        superToken: superToken.address,
        account: testenv.aliases[receiver],
        balanceSnapshot: receiverBalance2
    });

    if (fn !== "noop") {
        _updateFlowInfo({
            testenv,
            superToken: superToken.address,
            sender: testenv.aliases[sender],
            receiver: testenv.aliases[receiver],
            flowInfo
        });
        _updateAccountFlowInfo({
            testenv,
            superToken: superToken.address,
            account: testenv.aliases[sender],
            flowInfo: await testenv.sf.cfa.getAccountFlowInfo({
                superToken: superToken.address,
                account: testenv.aliases[sender],
            })
        });
        _updateAccountFlowInfo({
            testenv,
            superToken: superToken.address,
            account: testenv.aliases[receiver],
            flowInfo: await testenv.sf.cfa.getAccountFlowInfo({
                superToken: superToken.address,
                account: testenv.aliases[receiver],
            })
        });
    }
}

async function shouldCreateFlow({
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    console.log("======== shouldCreateFlow begins ========");
    const { superToken } = testenv.contracts;
    const senderAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[sender]
    });
    const receiverAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[receiver]
    });

    //console.log("!!! 1", JSON.stringify(testenv.data, null, 4));
    await _shouldChangeFlow({
        fn: "createFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        expectedOwedDepositRatio
    });
    //console.log("!!! 2", JSON.stringify(testenv.data, null, 4));

    const senderAccountFlow2 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[sender]
    });
    const receiverAccountFlow2 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[receiver]
    });

    assert.equal(
        toBN(senderAccountFlow1.flowRate).sub(toBN(senderAccountFlow2.flowRate)).toString(),
        flowRate.toString()
    );
    assert.equal(
        toBN(receiverAccountFlow2.flowRate).sub(toBN(receiverAccountFlow1.flowRate)).toString(),
        flowRate.toString()
    );
    console.log("======== shouldCreateFlow ends ========");
}

async function shouldUpdateFlow({
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    console.log("======== shouldUpdateFlow begins ========");
    const { superToken } = testenv.contracts;
    const senderAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[sender]
    });
    const receiverAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[receiver]
    });
    const oldFlowInfo = getFlowInfo({
        testenv,
        superToken: superToken.address,
        sender: testenv.aliases[sender],
        receiver: testenv.aliases[receiver]
    });
    const flowRateDelta = toBN(flowRate).sub(toBN(oldFlowInfo.flowRate)).toString();

    await _shouldChangeFlow({
        fn: "updateFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        expectedOwedDepositRatio
    });

    const senderAccountFlow2 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[sender]
    });
    const receiverAccountFlow2 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[receiver]
    });

    assert.equal(
        toBN(senderAccountFlow1.flowRate).sub(toBN(senderAccountFlow2.flowRate)).toString(),
        flowRateDelta
    );
    assert.equal(
        toBN(receiverAccountFlow2.flowRate).sub(toBN(receiverAccountFlow1.flowRate)).toString(),
        flowRateDelta
    );
    console.log("======== shouldUpdateFlow ends ========");
}

async function shouldDeleteFlow({
    testenv,
    sender,
    receiver
}) {
    console.log("======== shouldDeleteFlow begins ========");
    const { superToken } = testenv.contracts;
    const senderAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[sender]
    });
    const receiverAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[receiver]
    });
    const oldFlowInfo = getFlowInfo({
        testenv,
        superToken: superToken.address,
        sender: testenv.aliases[sender],
        receiver: testenv.aliases[receiver]
    });

    await _shouldChangeFlow({
        fn: "deleteFlow",
        testenv,
        sender,
        receiver,
        flowRate: "0",
    });

    const senderAccountFlow2 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[sender]
    });
    const receiverAccountFlow2 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: testenv.aliases[receiver]
    });

    const flowRateDelta = toBN(0).sub(toBN(oldFlowInfo.flowRate)).toString();
    assert.equal(
        toBN(senderAccountFlow1.flowRate).sub(toBN(senderAccountFlow2.flowRate)).toString(),
        flowRateDelta
    );
    assert.equal(
        toBN(receiverAccountFlow2.flowRate).sub(toBN(receiverAccountFlow1.flowRate)).toString(),
        flowRateDelta
    );
    console.log("======== shouldDeleteFlow ends ========");
}

async function shouldVerifyFlow({
    testenv,
    sender,
    receiver
}) {
    console.log("======== shouldVerifyFlow begins ========");
    await _shouldChangeFlow({
        fn: "noop",
        testenv,
        sender,
        receiver,
        flowRate: "0",
    });
    console.log("======== shouldVerifyFlow ends ========");
}

module.exports = {
    clipDepositNumber,
    clipOwedDepositNumber,
    getFlowInfo,
    getAccountFlowInfo,
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
    shouldVerifyFlow
};
