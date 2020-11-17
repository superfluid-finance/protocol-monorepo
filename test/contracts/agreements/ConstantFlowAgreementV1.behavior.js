const _ = require("lodash");
const { BN } = require("@openzeppelin/test-helpers");
const {
    web3tx,
    toBN
} = require("@decentral.ee/web3-helpers");

function clipDepositNumber(deposit) {
    // last 32 bites of the deposit (96 bites) is clipped off
    const rounding = deposit.and(toBN(0xFFFFFFFF)).isZero() ? 0 : 1;
    return deposit.shrn(32).addn(rounding).shln(32);
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

function syncAccountExpectedBalanceDeltas({
    testenv,
    superToken,
    timestamp
}) {
    testenv.listAddresses().forEach(account => {
        const accuntFlowInfo = getAccountFlowInfo({ testenv, superToken, account });
        const balanceSnapshot = testenv.getAccountBalanceSnapshot(superToken, account);
        const expectedBalanceDelta1 = testenv.getAccountExpectedBalanceDelta(superToken, account);
        const expectedBalanceDelta2 = expectedBalanceDelta1
            .add(
                toBN(accuntFlowInfo.flowRate)
                    .mul(toBN(timestamp).sub(toBN(balanceSnapshot.timestamp)))
            );
        // console.log("!!! syncAccountExpectedBalanceDeltas", testenv.toAlias(account),
        //     toBN(timestamp).sub(toBN(balanceSnapshot.timestamp)).toString(),
        //     expectedBalanceDelta1.toString(),
        //     expectedBalanceDelta2.toString());
        testenv.updateAccountExpectedBalanceDelta(
            superToken,
            account,
            expectedBalanceDelta2);
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

    const roles = {};
    const _balanceSnapshots1 = {};
    const _accountFlowInfo1 = {};
    const _accountFlowInfo2 = {};
    const _balances1 = {};
    const _balances2 = {};
    const flows = {};
    const expectedFlowInfo = {};
    const expectedNetFlowDeltas = {};
    let txBlock;
    let mfaDeposit = toBN(0);

    const addRole = (role, alias) => {
        roles[role] = testenv.getAddress(alias);
        console.log(`${role} account address ${roles[role]} (${alias})`);
    };

    const addToBalanceSnapshots1 = (role) => {
        _balanceSnapshots1[roles[role]] = testenv.getAccountBalanceSnapshot(superToken.address, roles[role]);
        testenv.printRealtimeBalance(`${role} balance snapshot before`, getBalanceSnapshots1(role));
    };
    const getBalanceSnapshots1 = role => _balanceSnapshots1[roles[role]];

    const updateAccountExpectedBalanceDelta = (role, expectedBalanceDelta) => {
        testenv.updateAccountExpectedBalanceDelta(
            superToken.address,
            roles[role],
            expectedBalanceDelta
        );
    };
    const getAccountExpectedBalanceDelta = (role) => {
        return testenv.getAccountExpectedBalanceDelta(
            superToken.address,
            roles[role]
        );
    };

    const addAccountFlowInfo1 = (role) => {
        _accountFlowInfo1[roles[role]] = getAccountFlowInfo({
            testenv,
            superToken: superToken.address,
            account: roles[role]
        });
        _printFlowInfo(`${role} account flow info snapshot before`, getAccountFlowInfo1(role));
    };
    const getAccountFlowInfo1 = role => _accountFlowInfo1[roles[role]];

    const addAccountFlowInfo2 = async (role) => {
        _accountFlowInfo2[roles[role]] = await testenv.sf.cfa.getAccountFlowInfo({
            superToken: superToken.address,
            account: roles[role]
        });
        _printFlowInfo(`${role} account flow info after`, getAccountFlowInfo2(role));
    };
    const getAccountFlowInfo2 = role => _accountFlowInfo2[roles[role]];

    const addToBalances1 = async (role) => {
        _balances1[roles[role]] = await superToken.realtimeBalanceOfNow(roles[role]);
        testenv.printRealtimeBalance(`${role} balance before`, getBalances1(role));
    };
    const getBalances1 = role => _balances1[roles[role]];

    const addToBalances2 = async (role) => {
        _balances2[roles[role]] = await superToken.realtimeBalanceOfNow(roles[role]);
        testenv.printRealtimeBalance(`${role} balance after`, getBalances2(role));
    };
    const getBalances2 = role => _balances2[roles[role]];

    const addFlowInfo1 = async (flowName, flowParams) => {
        const flowId = {
            superToken: superToken.address,
            sender: flowParams.sender,
            receiver: flowParams.receiver,
        };
        const flowInfo = await testenv.sf.cfa.getFlow(flowId);
        flows[flowName] =  {
            senderName: flowParams.senderName,
            receiverName: flowParams.receiverName,
            flowId,
            flowInfo1: flowInfo
        };
        _printFlowInfo(`${flowName} flow info before`, flowInfo);
    };

    const addFlowInfo2 = async flowName => {
        const flowData = flows[flowName];
        const flowInfo = await testenv.sf.cfa.getFlow(flowData.flowId);
        _printFlowInfo(`${flowName} flow info after`, flowInfo);
        flowData.flowInfo2 = flowInfo;
    };

    const validateFlowInfoChange = (flowName) => {
        console.log(`validating ${flowName} flow change...`);
        const flowData = flows[flowName];

        // validate flow info
        if (flowData.flowInfo2.flowRate.toString() !== "0") {
            assert.equal(
                flowData.flowInfo2.timestamp.getTime()/1000,
                txBlock.timestamp,
                "wrong flow timestamp of the flow");
        } else {
            assert.equal(flowData.flowInfo2.timestamp.getTime(), 0);
        }
        assert.equal(
            flowData.flowInfo2.flowRate,
            expectedFlowInfo[flowName].flowRate.toString(),
            "wrong flowrate of the flow");
        assert.equal(
            flowData.flowInfo2.deposit,
            expectedFlowInfo[flowName].deposit.toString(),
            "wrong deposit amount of the flow");
        assert.equal(
            flowData.flowInfo2.owedDeposit,
            expectedFlowInfo[flowName].owedDeposit.toString(),
            "wrong owed deposit aount of the flow");
    };

    const validateAccountFlowInfoChange = (role) => {
        console.log(`validating ${role} account net flow and deposit change...`);

        const inFlowNames = Object.keys(flows).filter(i => flows[i].flowId.receiver === roles[role]);
        const outFlowNames = Object.keys(flows).filter(i => flows[i].flowId.sender === roles[role]);
        console.log("in flows", inFlowNames);
        console.log("out flows", outFlowNames);

        let actualNetFlow = toBN(0);
        let expectedDepositDelta = toBN(0);
        let expectedOwedDepositDelta = toBN(0);

        inFlowNames.forEach(flowName => {
            const flowData = flows[flowName];
            actualNetFlow = actualNetFlow
                .add(toBN(flowData.flowInfo2.flowRate));
            expectedOwedDepositDelta = expectedOwedDepositDelta
                .add(toBN(flowData.flowInfo2.owedDeposit))
                .sub(toBN(flowData.flowInfo1.owedDeposit));
        });
        outFlowNames.forEach(flowName => {
            const flowData = flows[flowName];
            actualNetFlow = actualNetFlow
                .sub(toBN(flowData.flowInfo2.flowRate));
            expectedDepositDelta = expectedDepositDelta
                .add(toBN(flowData.flowInfo2.deposit))
                .sub(toBN(flowData.flowInfo1.deposit));
        });

        assert.equal(
            getAccountFlowInfo2(role).flowRate.toString(),
            actualNetFlow.toString(),
            `unexpected netflow of ${role}`);

        const flowRateDelta = toBN(getAccountFlowInfo2(role).flowRate)
            .sub(toBN(getAccountFlowInfo1(role).flowRate));
        assert.equal(
            flowRateDelta.toString(),
            expectedNetFlowDeltas[roles[role]].toString(),
            `wrong netflow delta of ${role}`);

        const depositDelta = toBN(getBalances2(role).deposit)
            .sub(toBN(getBalances1(role).deposit));
        assert.equal(
            depositDelta.toString(),
            expectedDepositDelta.toString(),
            `wrong deposit delta amount of ${role}`);

        const owedDepositDelta = toBN(getBalances2(role).owedDeposit)
            .sub(toBN(getBalances1(role).owedDeposit));
        assert.equal(
            owedDepositDelta.toString(),
            expectedOwedDepositDelta.toString(),
            `wrong owed deposit delta amount of ${role}`);
    };

    // add all roles
    addRole("sender", sender);
    addRole("receiver", receiver);
    if (fn === "deleteFlow") {
        assert.isDefined(by);
        const agentAddress = testenv.getAddress(by);
        let rewardAddress = await governance.getRewardAddress(superToken.address);
        if (rewardAddress === testenv.constants.ZERO_ADDRESS) {
            rewardAddress = agentAddress;
        }
        addRole("agent", by);
        addRole("reward", testenv.toAlias(rewardAddress));
    }
    if (mfa) {
        Object.keys(mfa.receivers).forEach(async receiverAlias => {
            const mfaReceiverName = "mfa.receiver." + receiverAlias;
            addRole(mfaReceiverName, receiverAlias);
        });
    }
    console.log("--------");

    // load current balance snapshot
    Object.keys(roles).forEach(role => addToBalanceSnapshots1(role));
    console.log("--------");

    // load account flow info before
    Object.keys(roles).forEach(role => addAccountFlowInfo1(role));
    console.log("--------");

    // load flow info before
    await addFlowInfo1("main", {
        senderName: "sender",
        receiverName: "receiver",
        sender: roles.sender,
        receiver: roles.receiver,
    });
    console.log("--------");

    // mfa support
    expectedNetFlowDeltas[roles.sender] =  toBN(flowRate)
        .mul(toBN(-1))
        .sub(toBN(getAccountFlowInfo1("sender").flowRate));
    expectedNetFlowDeltas[roles.receiver] = expectedNetFlowDeltas[roles.sender]
        .mul(toBN(-1));
    if (fn === "deleteFlow") {
        if (!(roles.agent in expectedNetFlowDeltas)) {
            expectedNetFlowDeltas[roles.agent] = toBN(0);
        }
        if (!(roles.reward in expectedNetFlowDeltas)) {
            expectedNetFlowDeltas[roles.reward] = toBN(0);
        }
    }
    if (mfa) {
        console.log("mfa enabled with", JSON.stringify(mfa));

        let totalProportions = Object.values(mfa.receivers)
            .map(i => i.proportion)
            .reduce((acc,cur) => acc + cur, 0);

        await Promise.all(Object.keys(mfa.receivers).map(async receiverAlias => {
            const mfaReceiverName = "mfa.receiver." + receiverAlias;
            const mfaFlowName = "mfa.flow." + receiverAlias;

            await addFlowInfo1(mfaFlowName, {
                senderName: "receiver",
                receiverName: mfaReceiverName,
                sender: roles.receiver,
                receiver: roles[mfaReceiverName],
            });
            const mfaFlowRate = toBN(flowRate)
                .mul(toBN(mfa.receivers[receiverAlias].proportion))
                .div(toBN(totalProportions));
            const mfaFlowDeposit = clipDepositNumber(
                toBN(mfaFlowRate)
                    .mul(toBN(testenv.configs.LIQUIDATION_PERIOD))
            );
            //console.log("!!!! mfaFlowDeposit", receiverAlias, mfaFlowDeposit.toString());
            const mfaFlowRateDelta = toBN(mfaFlowRate)
                .sub(toBN(getAccountFlowInfo1(mfaReceiverName).flowRate));
            mfaDeposit = mfaDeposit.add(mfaFlowDeposit);
            expectedNetFlowDeltas[roles[mfaReceiverName]] = mfaFlowRateDelta;
            expectedNetFlowDeltas[roles.receiver] = expectedNetFlowDeltas[roles.receiver]
                .sub(mfaFlowRateDelta);
        }));
        //console.log("!!!! mfaDeposit", mfaDeposit.toString());
        console.log("--------");
    }
    // console.log("!!!", flowRate.toString(),
    //     expectedNetFlowDeltas[roles.sender].toString(),
    //     expectedNetFlowDeltas[roles.receiver].toString());

    // calculate main flow expectations
    if (fn === "deleteFlow") {
        expectedFlowInfo.main = {
            flowRate: toBN(flowRate),
            deposit: toBN(0),
            owedDeposit: toBN(0)
        };
    } else {
        const deposit = clipDepositNumber(toBN(flowRate)
            .mul(toBN(testenv.configs.LIQUIDATION_PERIOD)));
        // take into account deposit allowance
        const owedDeposit = BN.min(deposit, mfaDeposit);
        expectedFlowInfo.main = {
            flowRate: toBN(flowRate),
            deposit: deposit.add(owedDeposit),
            owedDeposit: owedDeposit
        };
    }

    // load balance before flow change
    await Promise.all(Object.keys(roles).map(addToBalances1));
    console.log("--------");

    // check sender solvency
    const isSenderCritical = await superToken.isAccountCriticalNow(roles.sender);
    const isSenderSolvent = await superToken.isAccountSolventNow(roles.sender);
    console.log("Is sender critical before: ", isSenderCritical);
    console.log("Is sender solvent before: ", isSenderSolvent);
    console.log("--------");

    // change flow
    let tx;
    switch (fn) {
    case "createFlow":
    case "updateFlow":
        tx = await web3tx(
            testenv.sf.cfa[fn].bind(testenv.sf.cfa),
            `${fn} from ${sender} to ${receiver}`
        )({
            ...flows.main.flowId,
            flowRate: flowRate.toString(),
        });
        break;
    case "deleteFlow":
        tx = await web3tx(
            testenv.sf.cfa[fn].bind(testenv.sf.cfa),
            `${fn} from ${sender} to ${receiver}`
        )({
            ...flows.main.flowId,
            by: roles.agent,
        });
        break;
    default:
        assert(false);
    }
    txBlock = await web3.eth.getBlock(tx.receipt.blockNumber);
    console.log("--------");

    // load new balances
    await Promise.all(Object.keys(roles).map(async role => {
        await addToBalances2(role);
        assert.equal(getBalances2(role).timestamp, txBlock.timestamp);
    }));
    console.log("--------");

    // load new account flow info
    await Promise.all(Object.keys(roles).map(addAccountFlowInfo2));
    console.log("--------");

    // load new flow info
    await Promise.all(Object.keys(flows).map(addFlowInfo2));
    console.log("--------");

    // validate flow info changes
    validateFlowInfoChange("main");
    // TODO mfa flow changes
    console.log("--------");

    // validate account flow info changes
    Object.keys(roles).forEach(validateAccountFlowInfoChange);
    console.log("--------");

    // caculate expected balance changes per liquidation rules
    if (fn === "deleteFlow") {
        // console.log("!!!!",
        //     senderBalance1.timestamp.toString(),
        //     txBlock.timestamp,
        //     senderBalance2.timestamp.toString());
        if (isSenderCritical) {
            console.log("validating liquidation rules...");
            // the tx itself may move the balance more
            const adjustedRewardAmount = toBN(flows.main.flowInfo1.flowRate)
                .mul(toBN(txBlock.timestamp).sub(toBN(getBalances1("sender").timestamp)));
            if (isSenderSolvent) {
                const expectedRewardAmount =
                    toBN(getBalances1("sender").availableBalance /* is negative */)
                        .add(toBN(flows.main.flowInfo1.deposit))
                        .sub(adjustedRewardAmount);
                testenv.printSingleBalance("expected reward amount (to reward account)", expectedRewardAmount);
                updateAccountExpectedBalanceDelta(
                    "reward",
                    getAccountExpectedBalanceDelta("reward")
                        .add(expectedRewardAmount));
                updateAccountExpectedBalanceDelta(
                    "sender",
                    getAccountExpectedBalanceDelta("sender")
                        .sub(expectedRewardAmount));
            } else {
                const expectedRewardAmount = toBN(flows.main.flowInfo1.deposit);
                const expectedBailoutAmount =
                    toBN(getBalances1("sender").availableBalance /* is negative */)
                        .add(toBN(flows.main.flowInfo1.deposit))
                        .mul(toBN(-1))
                        .add(adjustedRewardAmount);
                testenv.printSingleBalance("expected reward amount (to agent)", expectedRewardAmount);
                testenv.printSingleBalance("expected bailout amount (from reward account)", expectedBailoutAmount);
                updateAccountExpectedBalanceDelta(
                    "agent",
                    getAccountExpectedBalanceDelta("agent")
                        .add(expectedRewardAmount));
                updateAccountExpectedBalanceDelta(
                    "reward",
                    getAccountExpectedBalanceDelta("reward")
                        .sub(expectedRewardAmount)
                        .sub(expectedBailoutAmount));
                updateAccountExpectedBalanceDelta(
                    "sender",
                    getAccountExpectedBalanceDelta("sender")
                        .add(expectedBailoutAmount));
            }
            console.log("--------");
        }
    }

    await testenv.validateExpectedBalances(() => {
        syncAccountExpectedBalanceDeltas({
            testenv,
            superToken: superToken.address,
            timestamp: txBlock.timestamp
        });
    });

    // update flow info
    Object.keys(flows).forEach(flowName => {
        const flowData = flows[flowName];
        if (flowData.flowInfo2) {
            //console.log(`saving ${flowName} flow info...`);
            //console.log("!!!", flowName, flowData.flowInfo2);
            _updateFlowInfo({
                testenv,
                superToken: superToken.address,
                sender: flowData.flowId.sender,
                receiver: flowData.flowId.receiver,
                flowInfo: flowData.flowInfo2
            });
        }
    });
    //console.log("--------");

    // update account flow info
    Object.keys(roles).forEach(role => {
        //console.log("!!!", role, accountFlowInfo2[role]);
        if (getAccountFlowInfo2(role)) {
            //console.log(`saving ${role} account flow info...`);
            _updateAccountFlowInfo({
                testenv,
                superToken: superToken.address,
                account: roles[role],
                flowInfo: getAccountFlowInfo2(role)
            });
        }
    });
    //console.log("--------");

    // make sure app is not jailed
    if (mfa) {
        assert.isFalse(await testenv.contracts.superfluid.isAppJailed(roles.receiver));
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

module.exports = {
    clipDepositNumber,
    getFlowInfo,
    getAccountFlowInfo,
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
    syncAccountExpectedBalanceDeltas
};
