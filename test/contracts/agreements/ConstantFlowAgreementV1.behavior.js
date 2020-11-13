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
    const addresses = {};
    const flows = {};
    const accountFlowInfo1 = {};
    const accountFlowInfo2 = {};
    const balanceSnapshots1 = {};
    const balances1 = {};
    const balances2 = {};
    const expectedRealtimeBalanceDeltas = {}; // some address can be the same one in some test cases

    const addToAddresses = (name, addressOrName) => {
        if (web3.utils.isAddress(addressOrName)) {
            addresses[name] = addressOrName;
            console.log(`${name} account address`, addressOrName);
        } else {
            addresses[name] = testenv.aliases[addressOrName];
            console.log(`${name} account address ${testenv.aliases[addressOrName]} (${addressOrName})`);
        }
    };

    const addToBalanceSnapshots1 = (name) => {
        balanceSnapshots1[name] = testenv.getAccountBalanceSnapshot({
            superToken: superToken.address,
            account: addresses[name]
        });
        testenv.printRealtimeBalance(`${name} balance snapshot before`, balanceSnapshots1[name]);
    };

    const addToBalances1 = async (name) => {
        balances1[name] = await superToken.realtimeBalanceOfNow(addresses[name]);
        balances1[name].address = addresses[name];
        testenv.printRealtimeBalance(`${name} balance before`, balances1[name]);
    };

    const addToBalances2 = async (name) => {
        balances2[name] = await superToken.realtimeBalanceOfNow(addresses[name]);
        balances2[name].address = addresses[name];
        testenv.printRealtimeBalance(`${name} balance after`, balances2[name]);
    };

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

    const addFlowInfo2 = async (flowName) => {
        const flowData = flows[flowName];
        const flowInfo = await testenv.sf.cfa.getFlow(flowData.flowId);
        _printFlowInfo(`${flowName} flow info after`, flowInfo);
        flowData.flowInfo2 = flowInfo;
    };

    const addAccountFlowInfo1 = (name) => {
        accountFlowInfo1[name] = getAccountFlowInfo({
            testenv,
            superToken: superToken.address,
            account: addresses[name]
        });
        _printFlowInfo(`${name} account flow info snapshot before`, accountFlowInfo1[name]);
    };

    const addAccountFlowInfo2 = async (name) => {
        accountFlowInfo2[name] = await testenv.sf.cfa.getAccountFlowInfo({
            superToken: superToken.address,
            account: addresses[name]
        });
        _printFlowInfo(`${name} account flow info after`, accountFlowInfo2[name]);
    };

    const validateFlowChange = ({
        flowName,
        txBlock,
        expectedFlowRate,
        expectedFlowDeposit,
        expectedFlowOwedDeposit
    }) => {
        console.log(`validating ${flowName} flow change...`);
        const flowData = flows[flowName];
        ///console.log("!!!!", flowData);
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
            expectedFlowRate.toString(),
            "wrong flowrate of the flow");
        assert.equal(
            flowData.flowInfo2.deposit,
            expectedFlowDeposit.toString(),
            "wrong deposit amount of the flow");
        assert.equal(
            flowData.flowInfo2.owedDeposit,
            expectedFlowOwedDeposit.toString(),
            "wrong owed deposit aount of the flow");

        // validate account flow info changes
        const flowRateDelta = toBN(flowData.flowInfo2.flowRate)
            .sub(toBN(flowData.flowInfo1.flowRate));
        assert.equal(
            toBN(accountFlowInfo1[flowData.senderName].flowRate)
                .sub(toBN(accountFlowInfo2[flowData.senderName].flowRate)).toString(),
            flowRateDelta.toString(),
            "wrong sender flow rate delta"
        );
        assert.equal(
            toBN(accountFlowInfo2[flowData.receiverName].flowRate)
                .sub(toBN(accountFlowInfo1[flowData.receiverName].flowRate)).toString(),
            flowRateDelta.mul(toBN(expectedReceiverRatioPct)).div(toBN(100)).toString(),
            "wrong receiver flow rate delta"
        );
    };

    const validateNetFlowAndDepositChange = ({
        name,
        accountBalance1,
        inFlowNames,
        outFlowNames
    }) => {
        console.log(`validating ${name} account net flow and deposit change...`);

        let expectedNetFlow = toBN(0);
        let expectedDepositDelta = toBN(0);
        let expectedOwedDepositDelta = toBN(0);

        inFlowNames.forEach(flowName => {
            const flowData = flows[flowName];
            expectedNetFlow = expectedNetFlow
                .add(toBN(flowData.flowInfo2.flowRate));
            expectedOwedDepositDelta = expectedOwedDepositDelta
                .add(toBN(flowData.flowInfo2.owedDeposit))
                .sub(toBN(flowData.flowInfo1.owedDeposit));
        });
        outFlowNames.forEach(flowName => {
            const flowData = flows[flowName];
            expectedNetFlow = expectedNetFlow
                .sub(toBN(flowData.flowInfo2.flowRate));
            expectedDepositDelta = expectedDepositDelta
                .add(toBN(flowData.flowInfo2.deposit))
                .sub(toBN(flowData.flowInfo1.deposit));
        });

        assert.equal(
            accountFlowInfo2[name].flowRate.toString(),
            expectedNetFlow.toString(),
            `wrong netflow of ${name}`);

        const depositDelta = toBN(balances2[name].deposit)
            .sub(toBN(accountBalance1.deposit));
        assert.equal(
            depositDelta.toString(),
            expectedDepositDelta.toString(),
            `wrong deposit amount of ${name}`);

        const owedDepositDelta = toBN(balances2[name].owedDeposit)
            .sub(toBN(accountBalance1.owedDeposit));
        assert.equal(
            owedDepositDelta.toString(),
            expectedOwedDepositDelta.toString(),
            `wrong owed deposit amount of ${name}`);
    };

    const validateBalanceChange = (name) => {
        const realtimeBalanceDelta = _realtimeBalance(balances2[name])
            .sub(_realtimeBalance(balanceSnapshots1[name]));
        console.log(`${name} real-time balance delta`, realtimeBalanceDelta.toString());
        const expectedRealtimeBalanceDelta =
            expectedRealtimeBalanceDeltas[addresses[name]]
                .add(
                    toBN(accountFlowInfo1[name].flowRate)
                        .mul(
                            toBN(balances2[name].timestamp.toString())
                                .sub(toBN(balanceSnapshots1[name].timestamp.toString()))
                        )
                );
        assert.equal(
            realtimeBalanceDelta.toString(),
            expectedRealtimeBalanceDelta.toString(),
            `wrong real-time balance changes of ${name}`);
    };

    addToAddresses("sender", sender);
    addToAddresses("receiver", receiver);
    if (fn === "deleteFlow") {
        assert.isDefined(by);
        const agentAddress = testenv.aliases[by];
        let rewardAddress = await governance.getRewardAddress(superToken.address);
        if (rewardAddress === testenv.constants.ZERO_ADDRESS) {
            rewardAddress = agentAddress;
        }
        addToAddresses("agent", agentAddress);
        addToAddresses("reward", rewardAddress);
    }

    // load current balance snapshot
    addToBalanceSnapshots1("sender");
    addToBalanceSnapshots1("receiver");
    if (fn === "deleteFlow") {
        addToBalanceSnapshots1("agent");
        addToBalanceSnapshots1("reward");
    }

    // prepare main flow update validation
    addAccountFlowInfo1("sender");
    addAccountFlowInfo1("receiver");
    if (fn === "deleteFlow") {
        addAccountFlowInfo1("agent");
        addAccountFlowInfo1("reward");
    }
    await addFlowInfo1("main", {
        senderName: "sender",
        receiverName: "receiver",
        sender: addresses.sender,
        receiver: addresses.receiver,
    });

    // mfa support
    let mfaReceivers = [];
    let mfaFlows = [];
    let calculateOwedDeposit = () => toBN(0);
    let expectedReceiverRatioPct = 100;
    if (mfa) {
        calculateOwedDeposit = deposit => toBN(deposit).mul(toBN(100)).div(toBN(mfa.ratioPct));
        expectedReceiverRatioPct = toBN(100).sub(toBN(mfa.ratioPct));
        mfaReceivers = Object.keys(mfa.receivers);
        for (let i = 0; i < mfaReceivers.length; ++i) {
            const mfaReceiver = mfaReceivers[i];
            const mfaReceiverAddress = mfa.receivers[mfaReceiver].address;
            addToAddresses(mfaReceiver, mfaReceiverAddress);
            addToBalanceSnapshots1(mfaReceiver);
            addAccountFlowInfo1(mfaReceiver);
            await addFlowInfo1("mfa." + mfaReceiver, {
                senderName: "receiver",
                receiverName: mfaReceiver,
                sender: addresses.receiver,
                receiver: mfaReceiverAddress
            });
            mfaFlows.push("mfa." + mfaReceiver);
        }
        console.log("mfa enabled with", JSON.stringify(mfa));
    }

    // init expected realtime balance deltas
    Object.keys(addresses).forEach(name => {
        expectedRealtimeBalanceDeltas[addresses[name]] = toBN(0);
    });

    const loadBalances2 = async () => {
        const names = Object.keys(addresses);
        for (let i = 0; i < names.length; ++i) {
            await addToBalances2(names[i]);
        }
    };

    if (fn !== "verifyFlow") {
        // calculate main flow expectations
        let expectedMainFlowDeposit;
        let expectedMainFlowOwedDeposit;
        if (fn !== "deleteFlow") {
            const deposit = toBN(flowRate)
                .mul(toBN(testenv.configs.LIQUIDATION_PERIOD));
            const owedDeposit = calculateOwedDeposit(deposit);
            // clipping twice due to implementation
            expectedMainFlowDeposit = clipDepositNumber(
                clipDepositNumber(deposit)
                    .add(owedDeposit));
            expectedMainFlowOwedDeposit = clipOwedDepositNumber(owedDeposit);
        } else {
            expectedMainFlowDeposit = toBN(0);
            expectedMainFlowOwedDeposit = toBN(0);
        }

        // load balance before flow change
        await addToBalances1("sender");
        await addToBalances1("receiver");
        if (fn === "deleteFlow") {
            await addToBalances1("agent");
            await addToBalances1("reward");
        }
        for (let i = 0; i < mfaReceivers.length; ++i) {
            await addToBalances1(mfaReceivers[i]);
        }

        // check sender solvency
        const isSenderCritical = await superToken.isAccountCriticalNow(addresses.sender);
        const isSenderSolvent = await superToken.isAccountSolventNow(addresses.sender);
        console.log("Is sender critical before: ", isSenderCritical);
        console.log("Is sender solvent before: ", isSenderSolvent);

        // change flow
        console.log("--------");
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
                by: addresses.agent,
            });
            break;
        default:
            assert(false);
        }
        const txBlock = await web3.eth.getBlock(tx.receipt.blockNumber);
        console.log("--------");

        await loadBalances2();
        console.log("--------");

        // load updated data
        await addAccountFlowInfo2("sender");
        await addAccountFlowInfo2("receiver");
        for (let i = 0; i < mfaReceivers.length; ++i) {
            await addAccountFlowInfo2(mfaReceivers[i]);
        }
        await addFlowInfo2("main");
        for (let i = 0; i < mfaFlows.length; ++i) {
            await addFlowInfo2(mfaFlows[i]);
        }

        // validate balance timestamps
        assert.equal(balances2.sender.timestamp, txBlock.timestamp);
        assert.equal(balances2.receiver.timestamp, txBlock.timestamp);
        console.log("--------");

        // validate flow info changes
        validateFlowChange({
            flowName: "main",
            txBlock,
            expectedFlowRate: flowRate,
            expectedFlowDeposit: expectedMainFlowDeposit,
            expectedFlowOwedDeposit: expectedMainFlowOwedDeposit
        });
        console.log("--------");

        // validate deposit changes
        validateNetFlowAndDepositChange({
            name: "sender",
            accountBalance1: balances1.sender,
            inFlowNames: [],
            outFlowNames: ["main"]
        });
        validateNetFlowAndDepositChange({
            name: "receiver",
            accountBalance1: balances1.receiver,
            inFlowNames: ["main"],
            outFlowNames: mfaFlows
        });
        for (let i = 0; i < mfaReceivers.length; ++i) {
            const mfaReceiver = mfaReceivers[i];
            validateNetFlowAndDepositChange({
                name: mfaReceiver,
                accountBalance1: balances1[mfaReceiver],
                inFlowNames: ["mfa." + mfaReceiver],
                outFlowNames: []
            });
        }
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
                    .mul(toBN(txBlock.timestamp).sub(toBN(balances1.sender.timestamp)));
                if (isSenderSolvent) {
                    const expectedRewardAmount = toBN(flows.main.flowInfo1.deposit)
                        .add(toBN(balances1.sender.availableBalance /* is negative */))
                        .sub(adjustedRewardAmount);
                    expectedRealtimeBalanceDeltas[addresses.reward] =
                        expectedRealtimeBalanceDeltas[addresses.reward]
                            .add(expectedRewardAmount);
                    testenv.printSingleBalance("expected reward amount (to reward account)", expectedRewardAmount);
                    expectedRealtimeBalanceDeltas[addresses.sender] =
                        expectedRealtimeBalanceDeltas[addresses.sender]
                            .sub(expectedRewardAmount);
                } else {
                    const expectedRewardAmount = toBN(flows.main.flowInfo1.deposit);
                    const expectedBailoutAmount = toBN(balances1.sender.availableBalance /* is negative */)
                        .add(toBN(flows.main.flowInfo1.deposit))
                        .mul(toBN(-1))
                        .add(adjustedRewardAmount);
                    expectedRealtimeBalanceDeltas[addresses.agent] =
                        expectedRealtimeBalanceDeltas[addresses.agent]
                            .add(expectedRewardAmount);
                    testenv.printSingleBalance("expected reward amount (to agent)", expectedRewardAmount);
                    expectedRealtimeBalanceDeltas[addresses.reward] =
                        expectedRealtimeBalanceDeltas[addresses.reward]
                            .sub(expectedRewardAmount)
                            .sub(expectedBailoutAmount);
                    testenv.printSingleBalance("expected bailout amount (from reward account)", expectedBailoutAmount);
                    expectedRealtimeBalanceDeltas[addresses.sender] =
                        expectedRealtimeBalanceDeltas[addresses.sender]
                            .add(expectedBailoutAmount);
                }
                console.log("--------");
            }
        }
    } else {
        await loadBalances2();
    }

    // validate balance changes
    Object.keys(addresses).forEach(name => {
        console.log(`validating ${name} account balance changes...`);
        validateBalanceChange(name);
    });

    // update test data
    Object.keys(flows).forEach(flowName => {
        const flowData = flows[flowName];
        if (flowData.flowInfo2) {
            console.log(`saving ${flowName} flow info...`);
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
    Object.keys(addresses).forEach(name => {
        //console.log("!!!", name, accountFlowInfo2[name]);
        if (accountFlowInfo2[name]) {
            console.log(`saving ${name} account flow info...`);
            _updateAccountFlowInfo({
                testenv,
                superToken: superToken.address,
                account: addresses[name],
                flowInfo: accountFlowInfo2[name]
            });
        }
        console.log(`saving ${name} account balance snapshot...`);
        testenv.updateAccountBalanceSnapshot({
            superToken: superToken.address,
            account: addresses[name],
            balanceSnapshot: balances2[name]
        });
    });

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
