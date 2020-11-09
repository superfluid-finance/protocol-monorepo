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

function _realtimeBalance(balance) {
    return toBN(balance.availableBalance)
        .add(BN.max(toBN(0), toBN(balance.deposit).sub(toBN(balance.owedDeposit))));
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
                                timestamp: new Date(),
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

function _printFlowInfo(title, flowInfo) {
    console.log(title,
        flowInfo.timestamp.getTime(),
        flowInfo.flowRate.toString(),
        flowInfo.deposit.toString(),
        flowInfo.owedDeposit.toString());
}

async function _shouldChangeFlow({
    fn,
    testenv,
    sender,
    receiver,
    by,
    flowRate,
    expectedOwedDepositRatio
}) {
    console.log(`======== ${fn} begins ========`);
    //console.log("!!! 1", JSON.stringify(testenv.data, null, 4));
    const { superToken, governance } = testenv.contracts;
    const senderAddress = testenv.aliases[sender];
    const receiverAddress = testenv.aliases[receiver];
    let byAddress;
    let rewardAddress = await governance.getRewardAddress(superToken.address);
    if (fn === "deleteFlow") {
        assert.isDefined(by);
        byAddress = testenv.aliases[by];
        if (rewardAddress === testenv.constants.ZERO_ADDRESS) {
            rewardAddress = byAddress;
        }
        console.log("agent address", byAddress);
        console.log("reward address", rewardAddress);
    }
    const flowId = {
        superToken: superToken.address,
        sender: senderAddress,
        receiver: receiverAddress,
    };
    const oldFlowInfo = await testenv.sf.cfa.getFlow(flowId);
    _printFlowInfo("current flow info", oldFlowInfo);

    const senderAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: senderAddress
    });
    _printFlowInfo("sender account flow info snapshot before", senderAccountFlow1);
    const receiverAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: superToken.address,
        account: receiverAddress
    });
    _printFlowInfo("receiver account flow info snapshot before", receiverAccountFlow1);

    const senderBalanceSnapshot1 = testenv.getAccountBalanceSnapshot({
        superToken: superToken.address,
        account: senderAddress
    });
    testenv.printRealtimeBalance("sender balance snapshot before", senderBalanceSnapshot1);
    const receiverBalanceSnapshot1 = testenv.getAccountBalanceSnapshot({
        superToken: superToken.address,
        account: receiverAddress
    });
    testenv.printRealtimeBalance("receiver balance snapshot before", receiverBalanceSnapshot1);
    let agentBalance1;
    let rewardBalance1;
    if (fn === "deleteFlow") {
        agentBalance1 = await testenv.getAccountBalanceSnapshot({
            superToken: superToken.address,
            account: byAddress
        });
        testenv.printRealtimeBalance("agent balance snapshot before", agentBalance1);
        rewardBalance1 = await testenv.getAccountBalanceSnapshot({
            superToken: superToken.address,
            account: rewardAddress
        });
        testenv.printRealtimeBalance("reward balance snapshot before", rewardBalance1);
    }

    let senderBalance2;
    let receiverBalance2;
    let agentBalance2;
    let rewardBalance2;
    let expectedRealtimeBalanceDeltas = {}; // some address can be the same one in some test cases
    expectedRealtimeBalanceDeltas[senderAddress] = toBN(0);
    expectedRealtimeBalanceDeltas[receiverAddress] = toBN(0);
    expectedRealtimeBalanceDeltas[byAddress] = toBN(0);
    expectedRealtimeBalanceDeltas[rewardAddress] = toBN(0);
    if (fn !== "verifyFlow") {
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

        const senderBalance1 = await superToken.realtimeBalanceOfNow(senderAddress);
        testenv.printRealtimeBalance("sender balance before", senderBalance1);
        const isSenderCritical = await superToken.isAccountCriticalNow(senderAddress);
        const isSenderSolvent = await superToken.isAccountSolventNow(senderAddress);
        console.log("Is sender critical before: ", isSenderCritical);
        console.log("Is sender solvent before: ", isSenderSolvent);

        const receiverBalance1 = await superToken.realtimeBalanceOfNow(receiverAddress);
        testenv.printRealtimeBalance("receiver balance before", receiverBalance1);

        let agentBalance1;
        let rewardBalance1;
        if (fn === "deleteFlow") {
            agentBalance1 = await superToken.realtimeBalanceOfNow(byAddress);
            testenv.printRealtimeBalance("agent balance before", agentBalance1);
            rewardBalance1 = await superToken.realtimeBalanceOfNow(rewardAddress);
            testenv.printRealtimeBalance("reward balance before", rewardBalance1);
        }

        let tx;
        switch (fn) {
        case "createFlow":
        case "updateFlow":
            tx = await web3tx(
                testenv.sf.cfa[fn].bind(testenv.sf.cfa),
                `${fn} from ${sender} to ${receiver}`
            )({
                ...flowId,
                flowRate: flowRate.toString(),
            });
            break;
        case "deleteFlow":
            tx = await web3tx(
                testenv.sf.cfa[fn].bind(testenv.sf.cfa),
                `${fn} from ${sender} to ${receiver}`
            )({
                ...flowId,
                by: byAddress,
            });
            break;
        default:
            assert(false);
        }
        const txBlock = await web3.eth.getBlock(tx.receipt.blockNumber);

        // update data
        senderBalance2 = await superToken.realtimeBalanceOfNow(senderAddress);
        testenv.printRealtimeBalance("sender balance after", senderBalance2);
        assert.equal(txBlock.timestamp, senderBalance2.timestamp);
        receiverBalance2 = await superToken.realtimeBalanceOfNow(receiverAddress);
        testenv.printRealtimeBalance("receiver balance after", receiverBalance2);
        assert.equal(txBlock.timestamp, receiverBalance2.timestamp);
        if (fn === "deleteFlow") {
            agentBalance2 = await superToken.realtimeBalanceOfNow(byAddress);
            testenv.printRealtimeBalance("agent balance after", agentBalance2);
            rewardBalance2 = await superToken.realtimeBalanceOfNow(rewardAddress);
            testenv.printRealtimeBalance("reward balance after", rewardBalance2);
        }
        const senderAccountFlow2 = await testenv.sf.cfa.getAccountFlowInfo({
            superToken: superToken.address,
            account: senderAddress,
        });
        _printFlowInfo("sender account flow info after", senderAccountFlow2);
        const receiverAccountFlow2 = await testenv.sf.cfa.getAccountFlowInfo({
            superToken: superToken.address,
            account: receiverAddress,
        });
        _printFlowInfo("receiver account flow info after", receiverAccountFlow2);

        // validate flow info changes
        const flowInfo = await testenv.sf.cfa.getFlow(flowId);
        _printFlowInfo("updated flow info", flowInfo);
        if (fn !== "deleteFlow") {
            assert.equal(
                flowInfo.timestamp.getTime()/1000,
                txBlock.timestamp,
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

        // validate account flow info changes
        const flowRateDelta = toBN(flowInfo.flowRate).sub(toBN(oldFlowInfo.flowRate)).toString();
        assert.equal(
            toBN(senderAccountFlow1.flowRate).sub(toBN(senderAccountFlow2.flowRate)).toString(),
            flowRateDelta.toString()
        );
        assert.equal(
            toBN(receiverAccountFlow2.flowRate).sub(toBN(receiverAccountFlow1.flowRate)).toString(),
            flowRateDelta.toString()
        );

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

        // caculate expected balance changes per liquidation rules
        if (fn === "deleteFlow") {
            // the tx itself may move the balance more
            const adjustedRewardAmount = toBN(oldFlowInfo.flowRate)
                .mul(toBN(txBlock.timestamp).sub(toBN(senderBalance1.timestamp)));
            // console.log("!!!!",
            //     senderBalance1.timestamp.toString(),
            //     txBlock.timestamp,
            //     senderBalance2.timestamp.toString());
            if (isSenderCritical) {
                if (isSenderSolvent) {
                    const expectedRewardAmount = toBN(oldFlowInfo.deposit)
                        .add(toBN(senderBalance1.availableBalance /* is negative */))
                        .sub(adjustedRewardAmount);
                    expectedRealtimeBalanceDeltas[rewardAddress] = expectedRealtimeBalanceDeltas[rewardAddress]
                        .add(expectedRewardAmount);
                    testenv.printSingleBalance("expected reward amount (to reward account)", expectedRewardAmount);
                    expectedRealtimeBalanceDeltas[senderAddress] = expectedRealtimeBalanceDeltas[senderAddress]
                        .sub(expectedRewardAmount);
                } else {
                    const expectedRewardAmount = toBN(oldFlowInfo.deposit);
                    const expectedBailoutAmount = toBN(senderBalance1.availableBalance /* is negative */)
                        .add(toBN(oldFlowInfo.deposit))
                        .mul(toBN(-1))
                        .add(adjustedRewardAmount);
                    expectedRealtimeBalanceDeltas[byAddress] = expectedRealtimeBalanceDeltas[byAddress]
                        .add(expectedRewardAmount);
                    testenv.printSingleBalance("expected reward amount (to agent)", expectedRewardAmount);
                    expectedRealtimeBalanceDeltas[rewardAddress] = expectedRealtimeBalanceDeltas[rewardAddress]
                        .sub(expectedRewardAmount)
                        .sub(expectedBailoutAmount);
                    testenv.printSingleBalance("expected bailout amount (from reward account)", expectedBailoutAmount);
                    expectedRealtimeBalanceDeltas[senderAddress] = expectedRealtimeBalanceDeltas[senderAddress]
                        .add(expectedBailoutAmount);
                }
            }
        }

        // update flow info
        _updateFlowInfo({
            testenv,
            superToken: superToken.address,
            sender: senderAddress,
            receiver: receiverAddress,
            flowInfo
        });
        // update account flow info
        _updateAccountFlowInfo({
            testenv,
            superToken: superToken.address,
            account: senderAddress,
            flowInfo: senderAccountFlow2
        });
        _updateAccountFlowInfo({
            testenv,
            superToken: superToken.address,
            account: receiverAddress,
            flowInfo: receiverAccountFlow2
        });
    } else {
        senderBalance2 = await superToken.realtimeBalanceOfNow(senderAddress);
        receiverBalance2 = await superToken.realtimeBalanceOfNow(receiverAddress);
        testenv.printRealtimeBalance("sender balance after", senderBalance2);
        testenv.printRealtimeBalance("receiver balance after", receiverBalance2);
    }

    // validate sender balance snapshot changes
    const senderRealtimeBalanceDelta = _realtimeBalance(senderBalance2)
        .sub(_realtimeBalance(senderBalanceSnapshot1));
    console.log("sender real-time balance delta", senderRealtimeBalanceDelta.toString());
    expectedRealtimeBalanceDeltas[senderAddress] = expectedRealtimeBalanceDeltas[senderAddress]
        .add(
            toBN(senderAccountFlow1.flowRate)
                .mul(
                    toBN(senderBalance2.timestamp.toString())
                        .sub(toBN(senderBalanceSnapshot1.timestamp.toString()))
                )
        );
    assert.equal(
        senderRealtimeBalanceDelta.toString(),
        expectedRealtimeBalanceDeltas[senderAddress].toString(),
        "wrong real-time balance changes of sender");

    // validate receiver balance snapshot changes
    const receiverRealtimeBalanceDelta = _realtimeBalance(receiverBalance2)
        .sub(_realtimeBalance(receiverBalanceSnapshot1));
    console.log("receiver real-time balance delta", receiverRealtimeBalanceDelta.toString());
    expectedRealtimeBalanceDeltas[receiverAddress] = expectedRealtimeBalanceDeltas[receiverAddress]
        .add(
            toBN(receiverAccountFlow1.flowRate)
                .mul(
                    toBN(receiverBalance2.timestamp.toString())
                        .sub(toBN(receiverBalanceSnapshot1.timestamp.toString()))
                )
        );
    assert.equal(
        receiverRealtimeBalanceDelta.toString(),
        expectedRealtimeBalanceDeltas[receiverAddress].toString(),
        "wrong real-time balance changes of receiver");

    if (fn === "deleteFlow") {
        // validate agent balance
        const agentRealtimeBalanceDelta = _realtimeBalance(agentBalance2)
            .sub(_realtimeBalance(agentBalance1));
        console.log("agent real-time balance delta", agentRealtimeBalanceDelta.toString());
        assert.equal(
            agentRealtimeBalanceDelta.toString(),
            expectedRealtimeBalanceDeltas[byAddress].toString(),
            "wrong real-time balance changes of agent");

        // validate reward account balance changes
        const rewardRealtimeBalanceDelta = _realtimeBalance(rewardBalance2)
            .sub(_realtimeBalance(rewardBalance1));
        console.log("reward real-time balance delta", rewardRealtimeBalanceDelta.toString());
        assert.equal(
            rewardRealtimeBalanceDelta.toString(),
            expectedRealtimeBalanceDeltas[rewardAddress].toString(),
            "wrong real-time balance changes of reward account");
    }

    // update balance snapshots
    testenv.updateAccountBalanceSnapshot({
        superToken: superToken.address,
        account: senderAddress,
        balanceSnapshot: senderBalance2
    });
    testenv.updateAccountBalanceSnapshot({
        superToken: superToken.address,
        account: receiverAddress,
        balanceSnapshot: receiverBalance2
    });
    if (fn === "deleteFlow") {
        testenv.updateAccountBalanceSnapshot({
            superToken: superToken.address,
            account: rewardAddress,
            balanceSnapshot: rewardBalance2
        });
        testenv.updateAccountBalanceSnapshot({
            superToken: superToken.address,
            account: byAddress,
            balanceSnapshot: agentBalance2
        });
    }
    //console.log("!!! 2", JSON.stringify(testenv.data, null, 4));
    console.log(`======== ${fn} ends ========`);
}

async function shouldCreateFlow({
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    await _shouldChangeFlow({
        fn: "createFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        expectedOwedDepositRatio
    });
}

async function shouldUpdateFlow({
    testenv,
    sender,
    receiver,
    flowRate,
    expectedOwedDepositRatio
}) {
    await _shouldChangeFlow({
        fn: "updateFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        expectedOwedDepositRatio
    });
}

async function shouldDeleteFlow({
    testenv,
    sender,
    receiver,
    by
}) {
    await _shouldChangeFlow({
        fn: "deleteFlow",
        testenv,
        sender,
        receiver,
        by,
        flowRate: "0"
    });
}

async function shouldVerifyFlow({
    testenv,
    sender,
    receiver
}) {
    await _shouldChangeFlow({
        fn: "verifyFlow",
        testenv,
        sender,
        receiver,
        flowRate: "0",
    });
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
