const _ = require("lodash");
const { BN } = require("@openzeppelin/test-helpers");
const { web3tx, toBN } = require("@decentral.ee/web3-helpers");

function clipDepositNumber(deposit, roundingDown = false) {
    // last 32 bites of the deposit (96 bites) is clipped off
    const rounding = roundingDown
        ? 0
        : deposit.and(toBN(0xffffffff)).isZero()
        ? 0
        : 1;
    return deposit
        .shrn(32)
        .addn(rounding)
        .shln(32);
}

function adjustNewAppAllowanceUsed(
    appAllowance,
    appAllowanceWanted,
    newAppAllowanceUsed
) {
    //return BN.max(toBN(0), BN.min(appAllowance, BN.max(newAppAllowanceUsed, appAllowanceUsed)));
    return BN.max(toBN(0), BN.min(appAllowance, newAppAllowanceUsed));
}

//
// Flow info test data operations
//
function _updateFlowInfo({ testenv, superToken, sender, receiver, flowInfo }) {
    _.merge(testenv.data, {
        tokens: {
            [superToken]: {
                cfa: {
                    flows: {
                        [`${sender}:${receiver}`]: {
                            sender,
                            receiver,
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

function getFlowInfo({ testenv, superToken, sender, receiver }) {
    _.defaultsDeep(testenv.data, {
        tokens: {
            [superToken]: {
                cfa: {
                    flows: {
                        [`${sender}:${receiver}`]: {
                            sender,
                            receiver,
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
    return _.clone(
        testenv.data.tokens[superToken].cfa.flows[`${sender}:${receiver}`]
    );
}

function _printFlowInfo(title, flowInfo) {
    console.log(
        title,
        flowInfo.timestamp.getTime(),
        flowInfo.flowRate.toString(),
        flowInfo.deposit.toString(),
        flowInfo.owedDeposit.toString()
    );
}

//
// Account flow info test data operations
//
function _updateAccountFlowInfo({ testenv, superToken, account, flowInfo }) {
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

function getAccountFlowInfo({ testenv, superToken, account }) {
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
                                owedDeposit: 0
                            }
                        }
                    }
                }
            }
        }
    });
    return _.clone(
        testenv.data.tokens[superToken].accounts[account].cfa.flowInfo
    );
}

function validateAccountNetFlow({ testenv, superToken, account }) {
    const alias = testenv.toAlias(account);
    console.log(`validating ${alias} account net flow ...`);

    // ${sender}:${receiver} => flowInfo
    const flows = testenv.data.tokens[superToken].cfa.flows;

    const inFlows = Object.keys(flows)
        .filter(i => i.endsWith(`:${account}`))
        .map(i => flows[i]);
    const outFlows = Object.keys(flows)
        .filter(i => i.startsWith(`${account}:`))
        .map(i => flows[i]);
    console.log(
        "in flows",
        inFlows.map(i => testenv.toAlias(i.sender))
    );
    console.log(
        "out flows",
        outFlows.map(i => testenv.toAlias(i.receiver))
    );

    let actualNetFlow = toBN(0);

    inFlows.forEach(flowInfo => {
        actualNetFlow.iadd(toBN(flowInfo.flowRate));
    });
    outFlows.forEach(flowInfo => {
        actualNetFlow.isub(toBN(flowInfo.flowRate));
    });

    const accountFlowInfo = getAccountFlowInfo({
        testenv,
        superToken,
        account
    });

    assert.equal(
        accountFlowInfo.flowRate.toString(),
        actualNetFlow.toString(),
        `unexpected netflow of ${alias}`
    );
}

function syncAccountExpectedBalanceDeltas({ testenv, superToken, timestamp }) {
    console.log("syncing accounting expected balance deltas due to flows...");

    testenv.listAddresses().forEach(account => {
        const accuntFlowInfo = getAccountFlowInfo({
            testenv,
            superToken,
            account
        });
        const balanceSnapshot = testenv.getAccountBalanceSnapshot(
            superToken,
            account
        );
        const expectedBalanceDelta1 = testenv.getAccountExpectedBalanceDelta(
            superToken,
            account
        );
        const expectedBalanceDelta2 = expectedBalanceDelta1.add(
            toBN(accuntFlowInfo.flowRate).mul(
                toBN(timestamp).sub(toBN(balanceSnapshot.timestamp))
            )
        );
        // console.log("!!! syncAccountExpectedBalanceDeltas", testenv.toAlias(account),
        //     toBN(timestamp).sub(toBN(balanceSnapshot.timestamp)).toString(),
        //     expectedBalanceDelta1.toString(),
        //     expectedBalanceDelta2.toString());
        testenv.updateAccountExpectedBalanceDelta(
            superToken,
            account,
            expectedBalanceDelta2
        );
    });
}

class MFASupport {
    static async setup({ testenv, mfa, roles }) {
        roles.mfaSender = testenv.getAddress(mfa.sender);
        roles.mfa = testenv.getAddress("mfa");

        Object.keys(mfa.receivers).forEach(async receiverAlias => {
            const mfaReceiverName = "mfa.receiver." + receiverAlias;
            roles[mfaReceiverName] = testenv.getAddress(receiverAlias);
            console.log(
                `${receiverAlias} account address ${roles[mfaReceiverName]} (${receiverAlias})`
            );
        });

        const receivers = Object.keys(mfa.receivers).filter(
            i => mfa.receivers[i].proportion > 0
        );
        return {
            userData: web3.eth.abi.encodeParameters(
                ["address", "uint256", "address[]", "uint256[]"],
                [
                    testenv.getAddress(mfa.sender),
                    mfa.ratioPct,
                    receivers.map(i => testenv.getAddress(i)),
                    receivers.map(i => mfa.receivers[i].proportion)
                ]
            )
        };
    }

    static async updateFlowExpectations({
        testenv,
        superToken,
        mfa,
        flowRate,
        roles,
        addFlowInfo1,
        getAccountFlowInfo1,
        expectedNetFlowDeltas,
        expectedFlowInfo
    }) {
        let totalProportions = Object.values(mfa.receivers)
            .map(i => i.proportion)
            .reduce((acc, cur) => acc + cur, 0);

        const depositAllowance = clipDepositNumber(
            toBN(flowRate).mul(toBN(testenv.configs.LIQUIDATION_PERIOD)),
            true /* rounding down */
        );

        // expected unwindng of mfa receiver flows
        Object.keys(mfa.receivers).forEach(receiverAlias => {
            const receiverAddress = testenv.getAddress(receiverAlias);
            if (!(receiverAddress in expectedNetFlowDeltas)) {
                expectedNetFlowDeltas[receiverAddress] = toBN(0);
            }
        });
        await Promise.all(
            Object.keys(mfa.receivers).map(async receiverAlias => {
                const mfaReceiverName = "mfa.receiver." + receiverAlias;
                const mfaFlowName = "mfa.flow." + receiverAlias;
                const receiverAddress = testenv.getAddress(receiverAlias);
                const notTouched =
                    mfa.receivers[receiverAlias].proportion === 0;

                // skip if it's been deleted by one of the mfa receivers
                if (receiverAddress == roles.receiver) return;

                await addFlowInfo1(mfaFlowName, {
                    sender: roles.mfa,
                    receiver: roles[mfaReceiverName],
                    notTouched
                });

                const mfaFlowDepositAllowance = clipDepositNumber(
                    toBN(depositAllowance)
                        .mul(toBN(mfa.receivers[receiverAlias].proportion))
                        .mul(toBN(mfa.ratioPct))
                        .divn(100)
                        .div(toBN(totalProportions)),
                    true /* rounding down */
                );

                const mfaFlowRate = toBN(mfaFlowDepositAllowance).div(
                    toBN(testenv.configs.LIQUIDATION_PERIOD)
                );

                if (!notTouched) {
                    const mfaFlowRateDelta = toBN(mfaFlowRate).sub(
                        toBN(getAccountFlowInfo1(mfaReceiverName).flowRate)
                    );
                    expectedNetFlowDeltas[receiverAddress].iadd(
                        mfaFlowRateDelta
                    );
                    expectedNetFlowDeltas[roles.mfa].isub(mfaFlowRateDelta);
                }

                expectedFlowInfo[mfaFlowName] = {
                    flowRate: mfaFlowRate,
                    deposit: mfaFlowDepositAllowance,
                    owedDeposit: toBN(0)
                };

                // console.log("!!!! mfa flow",
                //     mfaFlowName,
                //     notTouched,
                //     mfaFlowRate.toString(),
                //     mfaFlowDepositAllowance.toString());
            })
        );

        // expected unwindng of mfa sender flow if the flow being deleted is not sending to the mfa
        if (roles.mfa != roles.receiver) {
            const mfaSenderFlow = getFlowInfo({
                testenv,
                superToken: superToken.address,
                sender: roles.mfaSender,
                receiver: roles.mfa
            });
            //console.log("!!!!", mfaSenderFlow);
            if (!(roles.mfaSender in expectedNetFlowDeltas))
                expectedNetFlowDeltas[roles.mfaSender] = toBN(0);
            if (!(roles.mfa in expectedNetFlowDeltas))
                expectedNetFlowDeltas[roles.mfa] = toBN(0);
            expectedNetFlowDeltas[roles.mfaSender].iadd(
                toBN(mfaSenderFlow.flowRate)
            );
            expectedNetFlowDeltas[roles.mfa].isub(toBN(mfaSenderFlow.flowRate));
            await addFlowInfo1("mfa.sender", {
                sender: roles.mfaSender,
                receiver: roles.mfa
            });
            expectedFlowInfo["mfa.sender"] = {
                flowRate: "0",
                deposit: toBN(0),
                owedDeposit: toBN(0)
            };
        }
    }

    static async postCheck({ testenv, roles }) {
        assert.isFalse(
            await testenv.contracts.superfluid.isAppJailed(roles.mfa),
            "MFA app was jailed"
        );
    }
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
    userData,
    by
}) {
    console.log(`======== ${fn} begins ========`);
    console.log(`${sender} -> ${receiver} ${flowRate}`, by ? `by ${by}` : "");
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

    const addRole = (role, alias) => {
        roles[role] = testenv.getAddress(alias);
        console.log(`${role} account address ${roles[role]} (${alias})`);
    };

    const addToBalanceSnapshots1 = role => {
        _balanceSnapshots1[roles[role]] = testenv.getAccountBalanceSnapshot(
            superToken.address,
            roles[role]
        );
        testenv.printRealtimeBalance(
            `${role} balance snapshot before`,
            getBalanceSnapshots1(role)
        );
    };
    const getBalanceSnapshots1 = role => _balanceSnapshots1[roles[role]];

    const updateAccountExpectedBalanceDelta = (role, expectedBalanceDelta) => {
        testenv.updateAccountExpectedBalanceDelta(
            superToken.address,
            roles[role],
            expectedBalanceDelta
        );
    };
    const getAccountExpectedBalanceDelta = role => {
        return testenv.getAccountExpectedBalanceDelta(
            superToken.address,
            roles[role]
        );
    };

    const addAccountFlowInfo1 = role => {
        _accountFlowInfo1[roles[role]] = getAccountFlowInfo({
            testenv,
            superToken: superToken.address,
            account: roles[role]
        });
        _printFlowInfo(
            `${role} account flow info snapshot before`,
            getAccountFlowInfo1(role)
        );
    };
    const getAccountFlowInfo1 = role => _accountFlowInfo1[roles[role]];

    const addAccountFlowInfo2 = async role => {
        _accountFlowInfo2[
            roles[role]
        ] = await testenv.sf.cfa.getAccountFlowInfo({
            superToken: superToken.address,
            account: roles[role]
        });
        _printFlowInfo(
            `${role} account flow info after`,
            getAccountFlowInfo2(role)
        );
    };
    const getAccountFlowInfo2 = role => _accountFlowInfo2[roles[role]];

    const addToBalances1 = async role => {
        _balances1[roles[role]] = await superToken.realtimeBalanceOfNow(
            roles[role]
        );
        testenv.printRealtimeBalance(
            `${role} balance before`,
            getBalances1(role)
        );
    };
    const getBalances1 = role => _balances1[roles[role]];

    const addToBalances2 = async role => {
        _balances2[roles[role]] = await superToken.realtimeBalanceOfNow(
            roles[role]
        );
        testenv.printRealtimeBalance(
            `${role} balance after`,
            getBalances2(role)
        );
    };
    const getBalances2 = role => _balances2[roles[role]];

    const addFlowInfo1 = async (flowName, flowParams) => {
        const flowId = {
            superToken: superToken.address,
            sender: flowParams.sender,
            receiver: flowParams.receiver
        };
        const flowInfo = await testenv.sf.cfa.getFlow(flowId);
        flows[flowName] = {
            flowId,
            notTouched: flowParams.notTouched,
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

    const validateFlowInfoChange = flowName => {
        console.log(`validating ${flowName} flow change...`);
        const flowData = flows[flowName];

        if (!flowData.notTouched) {
            // validate flow info
            if (flowData.flowInfo2.flowRate.toString() !== "0") {
                assert.equal(
                    flowData.flowInfo2.timestamp.getTime() / 1000,
                    txBlock.timestamp,
                    `wrong flow timestamp of the ${flowName} flow`
                );
            } else {
                assert.equal(flowData.flowInfo2.timestamp.getTime(), 0);
            }
            assert.equal(
                flowData.flowInfo2.flowRate,
                expectedFlowInfo[flowName].flowRate.toString(),
                `wrong flow rate of the ${flowName} flow`
            );
            assert.equal(
                flowData.flowInfo2.owedDeposit,
                expectedFlowInfo[flowName].owedDeposit.toString(),
                `wrong owed deposit amount of the ${flowName} flow`
            );
            assert.equal(
                flowData.flowInfo2.deposit,
                expectedFlowInfo[flowName].deposit.toString(),
                `wrong deposit amount of the ${flowName} flow`
            );
        } else {
            assert.equal(
                flowData.flowInfo2.flowRate,
                flowData.flowInfo1.flowRate,
                `flow rate of the ${flowName} flow should not change`
            );
            assert.equal(
                flowData.flowInfo2.owedDeposit,
                flowData.flowInfo1.owedDeposit,
                `owed deposit amount of the ${flowName} flow should not change`
            );
            assert.equal(
                flowData.flowInfo2.deposit,
                flowData.flowInfo1.deposit,
                `deposit amount of the ${flowName} flow should not change`
            );
        }
    };

    const validateAccountFlowInfoChange = role => {
        console.log(`validating ${role} account deposit changes...`);

        const inFlowNames = Object.keys(flows).filter(
            i => flows[i].flowId.receiver === roles[role]
        );
        const outFlowNames = Object.keys(flows).filter(
            i => flows[i].flowId.sender === roles[role]
        );
        console.log("in flows", inFlowNames);
        console.log("out flows", outFlowNames);

        let expectedDepositDelta = toBN(0);
        let expectedOwedDepositDelta = toBN(0);

        inFlowNames.forEach(flowName => {
            const flowData = flows[flowName];
            expectedOwedDepositDelta = expectedOwedDepositDelta
                .add(toBN(flowData.flowInfo2.owedDeposit))
                .sub(toBN(flowData.flowInfo1.owedDeposit));
        });
        outFlowNames.forEach(flowName => {
            const flowData = flows[flowName];
            expectedDepositDelta = expectedDepositDelta
                .add(toBN(flowData.flowInfo2.deposit))
                .sub(toBN(flowData.flowInfo1.deposit));
        });

        const flowRateDelta = toBN(getAccountFlowInfo2(role).flowRate).sub(
            toBN(getAccountFlowInfo1(role).flowRate)
        );
        assert.equal(
            flowRateDelta.toString(),
            expectedNetFlowDeltas[roles[role]].toString(),
            `wrong netflow delta of ${role}`
        );

        const depositDelta = toBN(getBalances2(role).deposit).sub(
            toBN(getBalances1(role).deposit)
        );
        assert.equal(
            depositDelta.toString(),
            expectedDepositDelta.toString(),
            `wrong deposit delta amount of ${role}`
        );

        const owedDepositDelta = toBN(getBalances2(role).owedDeposit).sub(
            toBN(getBalances1(role).owedDeposit)
        );
        assert.equal(
            owedDepositDelta.toString(),
            expectedOwedDepositDelta.toString(),
            `wrong owed deposit delta amount of ${role}`
        );
    };

    /**************************************************************************
     * Load current state and calculate expectations
     **************************************************************************/

    // add all roles
    addRole("sender", sender);
    addRole("receiver", receiver);
    if (fn === "deleteFlow") {
        assert.isDefined(by);
        const agentAddress = testenv.getAddress(by);
        let rewardAddress = await governance.getRewardAddress();
        if (rewardAddress === testenv.constants.ZERO_ADDRESS) {
            rewardAddress = agentAddress;
        }
        addRole("agent", by);
        addRole("reward", testenv.toAlias(rewardAddress));
    }
    if (mfa) {
        ({ userData } = await MFASupport.setup({ testenv, mfa, roles }));
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
        sender: roles.sender,
        receiver: roles.receiver
    });
    console.log("--------");

    // calculate expected net flow changes
    expectedNetFlowDeltas[roles.sender] = toBN(
        flows.main.flowInfo1.flowRate
    ).sub(toBN(flowRate));
    expectedNetFlowDeltas[roles.receiver] = toBN(flowRate).sub(
        toBN(flows.main.flowInfo1.flowRate)
    );
    if (fn === "deleteFlow") {
        if (!(roles.agent in expectedNetFlowDeltas)) {
            expectedNetFlowDeltas[roles.agent] = toBN(0);
        }
        if (!(roles.reward in expectedNetFlowDeltas)) {
            expectedNetFlowDeltas[roles.reward] = toBN(0);
        }
    }

    // mfa support
    if (mfa) {
        await MFASupport.updateFlowExpectations({
            testenv,
            superToken,
            mfa,
            flowRate,
            roles,
            getAccountFlowInfo1,
            addFlowInfo1,
            expectedNetFlowDeltas,
            expectedFlowInfo
        });
        console.log("--------");
    }

    // calculate main flow expectations
    {
        const mainFlowDepositUnclipped = toBN(flowRate).mul(
            toBN(testenv.configs.LIQUIDATION_PERIOD)
        );
        const mainFlowDeposit = clipDepositNumber(
            mainFlowDepositUnclipped,
            false /* rounding up */
        );
        const mainFlowAppAllowance = clipDepositNumber(
            mainFlowDepositUnclipped,
            false /* rounding up */
        );
        const newAppAllowanceUsed = Object.values(expectedFlowInfo)
            .map(i => i.deposit)
            .reduce((acc, cur) => {
                return acc.add(cur);
            }, toBN(0));
        const mainFlowAllowanceUsed = adjustNewAppAllowanceUsed(
            mainFlowAppAllowance,
            mainFlowDeposit, // appAllowanceUsed
            newAppAllowanceUsed
        );
        // adjust deposit allowance refunds to refund less
        // if (mfaAllowanceUsedDelta.ltn(0)) {
        //     mfaAllowanceUsedAdjusted = mfaAllowanceUsedAdjusted.add(
        //         clipDepositNumber(mfaAllowanceUsedDelta.mul(toBN(-1)), true /* rounding down */)
        //             .add(mfaAllowanceUsedDelta)
        //     );
        // }
        expectedFlowInfo.main = {
            flowRate: toBN(flowRate),
            deposit: mainFlowDeposit.add(mainFlowAllowanceUsed),
            owedDeposit: mainFlowAllowanceUsed
        };
        // console.log("!!!! main",
        //     flowRate.toString(),
        //     mainFlowDeposit.toString(),
        //     mainFlowAppAllowance.toString(),
        //     newAppAllowanceUsed.toString(),
        //     mainFlowAllowanceUsed.toString());
    }

    // load balance before flow change
    await Promise.all(Object.keys(roles).map(addToBalances1));
    const isSenderCritical = await superToken.isAccountCriticalNow(
        roles.sender
    );
    const isSenderSolvent = await superToken.isAccountSolventNow(roles.sender);
    console.log("Is sender critical before: ", isSenderCritical);
    console.log("Is sender solvent before: ", isSenderSolvent);
    console.log("--------");

    /**************************************************************************
     * apply change
     **************************************************************************/

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
                userData
            });
            break;
        case "deleteFlow":
            tx = await web3tx(
                testenv.sf.cfa[fn].bind(testenv.sf.cfa),
                `${fn} from ${sender} to ${receiver}`
            )({
                ...flows.main.flowId,
                by: roles.agent,
                userData
            });
            break;
        default:
            assert(false);
    }
    txBlock = await web3.eth.getBlock(tx.receipt.blockNumber);
    console.log("--------");

    /**************************************************************************
     * load results and test expectations
     **************************************************************************/

    // make sure app is not jailed
    if (mfa) {
        await MFASupport.postCheck({ testenv, roles });
    }

    // caculate additional expected balance changes per liquidation rules
    if (fn === "deleteFlow") {
        // console.log("!!!!",
        //     senderBalance1.timestamp.toString(),
        //     txBlock.timestamp,
        //     senderBalance2.timestamp.toString());
        if (isSenderCritical) {
            console.log("validating liquidation rules...");
            // the tx itself may move the balance more
            const adjustedRewardAmount = toBN(
                flows.main.flowInfo1.flowRate
            ).mul(
                toBN(txBlock.timestamp).sub(
                    toBN(getBalances1("sender").timestamp)
                )
            );
            if (isSenderSolvent) {
                const expectedRewardAmount = toBN(
                    getBalances1("sender").availableBalance /* is negative */
                )
                    .add(toBN(flows.main.flowInfo1.deposit))
                    .sub(adjustedRewardAmount);
                testenv.printSingleBalance(
                    "expected reward amount (to reward account)",
                    expectedRewardAmount
                );
                updateAccountExpectedBalanceDelta(
                    "reward",
                    getAccountExpectedBalanceDelta("reward").add(
                        expectedRewardAmount
                    )
                );
                updateAccountExpectedBalanceDelta(
                    "sender",
                    getAccountExpectedBalanceDelta("sender").sub(
                        expectedRewardAmount
                    )
                );
            } else {
                const expectedRewardAmount = toBN(flows.main.flowInfo1.deposit);
                const expectedBailoutAmount = toBN(
                    getBalances1("sender").availableBalance /* is negative */
                )
                    .add(toBN(flows.main.flowInfo1.deposit))
                    .mul(toBN(-1))
                    .add(adjustedRewardAmount);
                testenv.printSingleBalance(
                    "expected reward amount (to agent)",
                    expectedRewardAmount
                );
                testenv.printSingleBalance(
                    "expected bailout amount (from reward account)",
                    expectedBailoutAmount
                );
                updateAccountExpectedBalanceDelta(
                    "agent",
                    getAccountExpectedBalanceDelta("agent").add(
                        expectedRewardAmount
                    )
                );
                updateAccountExpectedBalanceDelta(
                    "reward",
                    getAccountExpectedBalanceDelta("reward")
                        .sub(expectedRewardAmount)
                        .sub(expectedBailoutAmount)
                );
                updateAccountExpectedBalanceDelta(
                    "sender",
                    getAccountExpectedBalanceDelta("sender").add(
                        expectedBailoutAmount
                    )
                );
            }
            console.log("--------");
        }
    }

    await Promise.all(
        Object.keys(roles).map(async role => {
            await addToBalances2(role);
            assert.equal(getBalances2(role).timestamp, txBlock.timestamp);
        })
    );
    console.log("--------");

    await Promise.all(Object.keys(roles).map(addAccountFlowInfo2));
    console.log("--------");

    await Promise.all(Object.keys(flows).map(addFlowInfo2));
    console.log("--------");

    Object.keys(expectedFlowInfo).forEach(validateFlowInfoChange);
    console.log("--------");

    Object.keys(roles).forEach(validateAccountFlowInfoChange);
    console.log("--------");

    await testenv.validateExpectedBalances(() => {
        syncAccountExpectedBalanceDeltas({
            testenv,
            superToken: superToken.address,
            timestamp: txBlock.timestamp
        });
    });
    console.log("--------");

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

    // update account flow info
    Object.keys(roles).forEach(role => {
        //console.log(`saving ${role} account flow info...`);
        if (getAccountFlowInfo2(role)) {
            _updateAccountFlowInfo({
                testenv,
                superToken: superToken.address,
                account: roles[role],
                flowInfo: getAccountFlowInfo2(role)
            });
        }
    });

    Object.keys(roles).forEach(role =>
        validateAccountNetFlow({
            testenv,
            superToken: superToken.address,
            account: roles[role]
        })
    );
    console.log("--------");

    //console.log("!!! 2", JSON.stringify(testenv.data, null, 4));
    console.log(`======== ${fn} ends ========`);
}

async function shouldCreateFlow({ testenv, sender, receiver, flowRate, mfa }) {
    await _shouldChangeFlow({
        fn: "createFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        mfa
    });
}

async function shouldUpdateFlow({ testenv, sender, receiver, flowRate, mfa }) {
    await _shouldChangeFlow({
        fn: "updateFlow",
        testenv,
        sender,
        receiver,
        flowRate,
        mfa
    });
}

async function shouldDeleteFlow({ testenv, sender, receiver, mfa, by }) {
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
    adjustNewAppAllowanceUsed,
    getFlowInfo,
    getAccountFlowInfo,
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
    syncAccountExpectedBalanceDeltas
};
