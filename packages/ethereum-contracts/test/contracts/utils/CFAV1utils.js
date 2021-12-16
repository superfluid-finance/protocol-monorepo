const _ = require("lodash");
const {toBN} = require("@decentral.ee/web3-helpers");
const {BN} = require("@openzeppelin/test-helpers");

function getAccountFlowInfo({testenv, superToken, account}) {
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
                            },
                        },
                    },
                },
            },
        },
    });
    return _.clone(
        testenv.data.tokens[superToken].accounts[account].cfa.flowInfo
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

function clipDepositNumber(deposit, roundingDown = false) {
    // last 32 bites of the deposit (96 bites) is clipped off
    const rounding = roundingDown
        ? 0
        : deposit.and(toBN(0xffffffff)).isZero()
        ? 0
        : 1;
    return deposit.shrn(32).addn(rounding).shln(32);
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
function _updateFlowInfo({testenv, superToken, sender, receiver, flowInfo}) {
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
                            owedDeposit: flowInfo.owedDeposit,
                        },
                    },
                },
            },
        },
    });
}

function getFlowInfo({testenv, superToken, sender, receiver}) {
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
                            owedDeposit: 0,
                        },
                    },
                },
            },
        },
    });
    return _.clone(
        testenv.data.tokens[superToken].cfa.flows[`${sender}:${receiver}`]
    );
}

//
// Account flow info test data operations
//
function _updateAccountFlowInfo({testenv, superToken, account, flowInfo}) {
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
                                owedDeposit: flowInfo.owedDeposit,
                            },
                        },
                    },
                },
            },
        },
    });
}

function validateAccountNetFlow({testenv, superToken, account}) {
    const alias = testenv.toAlias(account);
    console.log(`validating ${alias} account net flow ...`);

    // ${sender}:${receiver} => flowInfo
    const flows = testenv.data.tokens[superToken].cfa.flows;

    const inFlows = Object.keys(flows)
        .filter((i) => i.endsWith(`:${account}`))
        .map((i) => flows[i]);
    const outFlows = Object.keys(flows)
        .filter((i) => i.startsWith(`${account}:`))
        .map((i) => flows[i]);
    console.log(
        "in flows",
        inFlows.map((i) => testenv.toAlias(i.sender))
    );
    console.log(
        "out flows",
        outFlows.map((i) => testenv.toAlias(i.receiver))
    );

    let actualNetFlow = toBN(0);

    inFlows.forEach((flowInfo) => {
        actualNetFlow.iadd(toBN(flowInfo.flowRate));
    });
    outFlows.forEach((flowInfo) => {
        actualNetFlow.isub(toBN(flowInfo.flowRate));
    });

    const accountFlowInfo = getAccountFlowInfo({
        testenv,
        superToken,
        account,
    });

    assert.equal(
        accountFlowInfo.flowRate.toString(),
        actualNetFlow.toString(),
        `unexpected netflow of ${alias}`
    );
}

function syncAccountExpectedBalanceDeltas({testenv, superToken, timestamp}) {
    console.log("syncing accounting expected balance deltas due to flows...");

    testenv.listAddresses().forEach((account) => {
        const accountFlowInfo = getAccountFlowInfo({
            testenv,
            superToken,
            account,
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
            toBN(accountFlowInfo.flowRate).mul(
                toBN(timestamp).sub(toBN(balanceSnapshot.timestamp))
            )
        );
        testenv.updateAccountExpectedBalanceDelta(
            superToken,
            account,
            expectedBalanceDelta2
        );
    });
}

module.exports = {
    getAccountFlowInfo,
    _printFlowInfo,
    clipDepositNumber,
    adjustNewAppAllowanceUsed,
    _updateFlowInfo,
    getFlowInfo,
    _updateAccountFlowInfo,
    validateAccountNetFlow,
    syncAccountExpectedBalanceDeltas,
};
