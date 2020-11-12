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

//
// Flow info test data operations
//
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

function _printFlowInfo(title, flowInfo) {
    console.log(title,
        flowInfo.timestamp.getTime(),
        flowInfo.flowRate.toString(),
        flowInfo.deposit.toString(),
        flowInfo.owedDeposit.toString());
}

//
// Account flow info test data operations
//
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

//
// Flow update validation operations
//
async function _prepareFlowUpdateValidation({
    testenv,
    flowId
}) {
    const oldFlowInfo = await testenv.sf.cfa.getFlow(flowId);
    _printFlowInfo("current flow info", oldFlowInfo);
    const senderAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: flowId.superToken,
        account: flowId.sender
    });
    _printFlowInfo("sender account flow info snapshot before", senderAccountFlow1);
    const receiverAccountFlow1 = getAccountFlowInfo({
        testenv,
        superToken: flowId.superToken,
        account: flowId.receiver
    });
    _printFlowInfo("receiver account flow info snapshot before", receiverAccountFlow1);
    return {
        oldFlowInfo,
        senderAccountFlow1,
        receiverAccountFlow1
    };
}

async function _validateFlowUpdate({
    testenv,
    flowId,
    oldFlowInfo,
    senderAccountFlow1,
    receiverAccountFlow1,
    txBlock,
    expectedFlowRate,
    expectedNewDeposit,
    expectedNewOwedDeposit,
    expectedReceiverRatioPct
}) {
    const senderAccountFlow2 = await testenv.sf.cfa.getAccountFlowInfo({
        superToken: flowId.superToken,
        account: flowId.sender,
    });
    _printFlowInfo("sender account flow info after", senderAccountFlow2);
    const receiverAccountFlow2 = await testenv.sf.cfa.getAccountFlowInfo({
        superToken: flowId.superToken,
        account: flowId.receiver,
    });
    _printFlowInfo("receiver account flow info after", receiverAccountFlow2);

    const newFlowInfo = await testenv.sf.cfa.getFlow(flowId);
    _printFlowInfo("new flow info", newFlowInfo);
    if (newFlowInfo.flowRate.toString() !== "0") {
        assert.equal(
            newFlowInfo.timestamp.getTime()/1000,
            txBlock.timestamp,
            "wrong flow timestamp of the flow");
    } else {
        assert.equal(newFlowInfo.timestamp.getTime(), 0);
    }
    assert.equal(
        newFlowInfo.flowRate,
        expectedFlowRate.toString(),
        "wrong flowrate of the flow");
    assert.equal(
        newFlowInfo.deposit,
        expectedNewDeposit.toString(),
        "wrong deposit amount of the flow");
    assert.equal(
        newFlowInfo.owedDeposit,
        expectedNewOwedDeposit.toString(),
        "wrong owed deposit aount of the flow");

    // validate account flow info changes
    const flowRateDelta = toBN(newFlowInfo.flowRate).sub(toBN(oldFlowInfo.flowRate));
    assert.equal(
        toBN(senderAccountFlow1.flowRate).sub(toBN(senderAccountFlow2.flowRate)).toString(),
        flowRateDelta.toString(),
        "wrong sender flow rate delta"
    );
    assert.equal(
        toBN(receiverAccountFlow2.flowRate).sub(toBN(receiverAccountFlow1.flowRate)).toString(),
        flowRateDelta.mul(toBN(expectedReceiverRatioPct)).div(toBN(100)).toString(),
        "wrong receiver flow rate delta"
    );

    // update flow info
    _updateFlowInfo({
        testenv,
        superToken: flowId.superToken,
        sender: flowId.sender,
        receiver: flowId.receiver,
        flowInfo: newFlowInfo
    });
    // update account flow info
    _updateAccountFlowInfo({
        testenv,
        superToken: flowId.superToken,
        account: flowId.sender,
        flowInfo: senderAccountFlow2
    });
    _updateAccountFlowInfo({
        testenv,
        superToken: flowId.superToken,
        account: flowId.receiver,
        flowInfo: receiverAccountFlow2
    });
}

//
// test functions
//
async function _shouldChangeFlow({
    fn,
    testenv,
    sender,
    receiver,
    flowRate,
    mfa,
    by
}) {
    console.log(`======== ${fn} begins ========`);
    //console.log("!!! 1", JSON.stringify(testenv.data, null, 4));
    const { superToken, governance } = testenv.contracts;
    const senderAddress = web3.utils.isAddress(sender) ? sender : testenv.aliases[sender];
    const receiverAddress = web3.utils.isAddress(receiver) ? receiver : testenv.aliases[receiver];
    let agentAddress;
    let rewardAddress = await governance.getRewardAddress(superToken.address);
    if (fn === "deleteFlow") {
        assert.isDefined(by);
        agentAddress = testenv.aliases[by];
        if (rewardAddress === testenv.constants.ZERO_ADDRESS) {
            rewardAddress = agentAddress;
        }
        console.log("agent address", agentAddress);
        console.log("reward address", rewardAddress);
    }
    const flowId = {
        superToken: superToken.address,
        sender: senderAddress,
        receiver: receiverAddress,
    };
    let expectedRealtimeBalanceDeltas = {}; // some address can be the same one in some test cases

    // prepare main flow update validation
    const {
        oldFlowInfo,
        senderAccountFlow1,
        receiverAccountFlow1
    } = await _prepareFlowUpdateValidation({
        testenv,
        flowId
    });

    // prepare mfa flow update validation(s)
    let calculateOwedDeposit = () => toBN(0);
    let expectedReceiverRatioPct = 100;
    if (mfa) {
        calculateOwedDeposit = deposit => toBN(deposit).mul(toBN(100)).div(toBN(mfa.ratioPct));
        expectedReceiverRatioPct = toBN(100).sub(toBN(mfa.ratioPct));
    }

    // load current balance snapshot
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
    let agentBalanceSnapshot1;
    let rewardBalanceSnapshot1;
    if (fn === "deleteFlow") {
        agentBalanceSnapshot1 = await testenv.getAccountBalanceSnapshot({
            superToken: superToken.address,
            account: agentAddress
        });
        testenv.printRealtimeBalance("agent balance snapshot before", agentBalanceSnapshot1);
        rewardBalanceSnapshot1 = await testenv.getAccountBalanceSnapshot({
            superToken: superToken.address,
            account: rewardAddress
        });
        testenv.printRealtimeBalance("reward account balance snapshot before", rewardBalanceSnapshot1);
    }

    let senderBalance2;
    let receiverBalance2;
    let agentBalance2;
    let rewardBalance2;
    expectedRealtimeBalanceDeltas[senderAddress] = toBN(0);
    expectedRealtimeBalanceDeltas[receiverAddress] = toBN(0);
    expectedRealtimeBalanceDeltas[agentAddress] = toBN(0);
    expectedRealtimeBalanceDeltas[rewardAddress] = toBN(0);
    if (fn !== "verifyFlow") {
        let expectedNewDeposit, expectedNewOwedDeposit;
        if (fn !== "deleteFlow") {
            const deposit = toBN(flowRate)
                .mul(toBN(testenv.configs.LIQUIDATION_PERIOD));
            const owedDeposit = calculateOwedDeposit(deposit);
            // clipping twice due to implementation
            expectedNewDeposit = clipDepositNumber(
                clipDepositNumber(deposit)
                    .add(owedDeposit));
            expectedNewOwedDeposit = clipOwedDepositNumber(owedDeposit);
        } else {
            expectedNewDeposit = toBN(0);
            expectedNewOwedDeposit = toBN(0);
        }

        // load balance before flow change
        const senderBalance1 = await superToken.realtimeBalanceOfNow(senderAddress);
        testenv.printRealtimeBalance("sender balance before", senderBalance1);
        const receiverBalance1 = await superToken.realtimeBalanceOfNow(receiverAddress);
        testenv.printRealtimeBalance("receiver balance before", receiverBalance1);
        let agentBalanceSnapshot1;
        let rewardBalanceSnapshot1;
        if (fn === "deleteFlow") {
            agentBalanceSnapshot1 = await superToken.realtimeBalanceOfNow(agentAddress);
            testenv.printRealtimeBalance("agent balance before", agentBalanceSnapshot1);
            rewardBalanceSnapshot1 = await superToken.realtimeBalanceOfNow(rewardAddress);
            testenv.printRealtimeBalance("reward account balance before", rewardBalanceSnapshot1);
        }

        // check sender solvency
        const isSenderCritical = await superToken.isAccountCriticalNow(senderAddress);
        const isSenderSolvent = await superToken.isAccountSolventNow(senderAddress);
        console.log("Is sender critical before: ", isSenderCritical);
        console.log("Is sender solvent before: ", isSenderSolvent);

        // change flow
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
                by: agentAddress,
            });
            break;
        default:
            assert(false);
        }
        const txBlock = await web3.eth.getBlock(tx.receipt.blockNumber);

        // load data after flow change
        senderBalance2 = await superToken.realtimeBalanceOfNow(senderAddress);
        testenv.printRealtimeBalance("sender balance after", senderBalance2);
        receiverBalance2 = await superToken.realtimeBalanceOfNow(receiverAddress);
        testenv.printRealtimeBalance("receiver balance after", receiverBalance2);
        if (fn === "deleteFlow") {
            agentBalance2 = await superToken.realtimeBalanceOfNow(agentAddress);
            testenv.printRealtimeBalance("agent balance after", agentBalance2);
            rewardBalance2 = await superToken.realtimeBalanceOfNow(rewardAddress);
            testenv.printRealtimeBalance("reward account balance after", rewardBalance2);
        }

        // validate balance timestamps
        assert.equal(senderBalance2.timestamp, txBlock.timestamp);
        assert.equal(receiverBalance2.timestamp, txBlock.timestamp);

        // validate flow info changes
        await _validateFlowUpdate({
            testenv,
            flowId,
            oldFlowInfo,
            senderAccountFlow1,
            receiverAccountFlow1,
            txBlock,
            expectedFlowRate: flowRate,
            expectedNewDeposit,
            expectedNewOwedDeposit,
            expectedReceiverRatioPct
        });

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
            "0",
            "wrong owed deposit amount of sender");

        // validate receiver deposit changes and account flow info change
        const receiverDepositDelta = toBN(receiverBalance2.deposit)
            .sub(toBN(receiverBalance1.deposit));
        const receiverOwedDepositDelta = toBN(receiverBalance2.owedDeposit)
            .sub(toBN(receiverBalance1.owedDeposit));
        assert.equal(
            receiverDepositDelta.toString(),
            expectedNewOwedDeposit.sub(toBN(oldFlowInfo.owedDeposit)).toString(),
            "wrong deposit amount of receiver");
        assert.equal(
            receiverOwedDepositDelta.toString(),
            expectedNewOwedDeposit.sub(toBN(oldFlowInfo.owedDeposit)).toString(),
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
                    expectedRealtimeBalanceDeltas[agentAddress] = expectedRealtimeBalanceDeltas[agentAddress]
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
    } else {
        senderBalance2 = await superToken.realtimeBalanceOfNow(senderAddress);
        testenv.printRealtimeBalance("sender balance after", senderBalance2);
        receiverBalance2 = await superToken.realtimeBalanceOfNow(receiverAddress);
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
            .sub(_realtimeBalance(agentBalanceSnapshot1));
        console.log("agent real-time balance delta", agentRealtimeBalanceDelta.toString());
        assert.equal(
            agentRealtimeBalanceDelta.toString(),
            expectedRealtimeBalanceDeltas[agentAddress].toString(),
            "wrong real-time balance changes of agent");

        // validate reward account balance changes
        const rewardRealtimeBalanceDelta = _realtimeBalance(rewardBalance2)
            .sub(_realtimeBalance(rewardBalanceSnapshot1));
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
            account: agentAddress,
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
    mfa
}) {
    await _shouldChangeFlow({
        fn: "createFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        mfa,
    });
}

async function shouldUpdateFlow({
    testenv,
    sender,
    receiver,
    flowRate,
    mfa
}) {
    await _shouldChangeFlow({
        fn: "updateFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        mfa
    });
}

async function shouldDeleteFlow({
    testenv,
    sender,
    receiver,
    mfa,
    by
}) {
    await _shouldChangeFlow({
        fn: "deleteFlow",
        testenv,
        sender,
        receiver,
        flowRate: 0,
        mfa,
        by
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
