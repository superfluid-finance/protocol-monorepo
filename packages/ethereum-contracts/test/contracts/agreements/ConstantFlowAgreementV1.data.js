const _ = require("lodash");
const {toBN, max, min} = require("../utils/helpers");

/**
 * @dev CFA Data Model which contains the relevant CFA state,
 * updater functions and also validation functions.
 *
 */
module.exports = class CFADataModel {
    constructor(testenv, superToken) {
        this.testenv = testenv;
        this.superToken = superToken;
        this.roles = {};
        this._balanceSnapshotsBefore = {};
        this._accountFlowInfoBefore = {};
        this._accountFlowInfoAfter = {};
        this._balancesBefore = {};
        this._balancesAfter = {};
        this.flows = {};
        this.expectedFlowInfo = {};
        this.expectedNetFlowDeltas = {};
    }

    /** CFADataModel Class Data Specific Updaters */
    addRole(role, alias) {
        this.roles[role] = this.testenv.getAddress(alias);
        console.log(`${role} account address ${this.roles[role]} (${alias})`);
    }

    addToBalanceSnapshotsBefore(role) {
        this._balanceSnapshotsBefore[this.roles[role]] =
            this.testenv.getAccountBalanceSnapshot(
                this.superToken.address,
                this.roles[role]
            );
        this.testenv.printRealtimeBalance(
            `${role} balance snapshot before`,
            this.getBalanceSnapshotsBefore(role)
        );
    }
    getBalanceSnapshotsBefore(role) {
        return this._balanceSnapshotsBefore[this.roles[role]];
    }

    addAccountFlowInfoBefore(role) {
        this._accountFlowInfoBefore[this.roles[role]] = this.getAccountFlowInfo(
            {
                testenv: this.testenv,
                superToken: this.superToken.address,
                account: this.roles[role],
            }
        );
        CFADataModel._printFlowInfo(
            `${role} account flow info snapshot before`,
            this.getAccountFlowInfoBefore(role)
        );
    }
    getAccountFlowInfoBefore(role) {
        return this._accountFlowInfoBefore[this.roles[role]];
    }

    async addAccountFlowInfoAfter(role) {
        this._accountFlowInfoAfter[this.roles[role]] =
            await this.testenv.sf.cfa.getAccountFlowInfo({
                superToken: this.superToken.address,
                account: this.roles[role],
            });
        CFADataModel._printFlowInfo(
            `${role} account flow info after`,
            this.getAccountFlowInfoAfter(role)
        );
    }
    getAccountFlowInfoAfter(role) {
        return this._accountFlowInfoAfter[this.roles[role]];
    }

    async addToBalancesBefore(role) {
        this._balancesBefore[this.roles[role]] =
            await this.superToken.realtimeBalanceOfNow(this.roles[role]);
        this.testenv.printRealtimeBalance(
            `${role} balance before`,
            this.getBalancesBefore(role)
        );
    }
    getBalancesBefore(role) {
        return this._balancesBefore[this.roles[role]];
    }

    async addToBalancesAfter(role) {
        this._balancesAfter[this.roles[role]] =
            await this.superToken.realtimeBalanceOfNow(this.roles[role]);
        this.testenv.printRealtimeBalance(
            `${role} balance after`,
            this.getBalancesAfter(role)
        );
    }
    getBalancesAfter(role) {
        return this._balancesAfter[this.roles[role]];
    }

    async addFlowInfoBefore(flowName, flowParams) {
        const flowId = {
            superToken: this.superToken.address,
            sender: flowParams.sender,
            receiver: flowParams.receiver,
        };
        const flowInfo = await this.testenv.sf.cfa.getFlow(flowId);
        this.flows[flowName] = {
            flowId,
            notTouched: flowParams.notTouched, // only used in MFA test case
            flowInfoBefore: flowInfo,
        };
        CFADataModel._printFlowInfo(`${flowName} flow info before`, flowInfo);
    }
    async addFlowInfoAfter(flowName) {
        const flowData = this.flows[flowName];
        const flowInfo = await this.testenv.sf.cfa.getFlow(flowData.flowId);
        CFADataModel._printFlowInfo(`${flowName} flow info after`, flowInfo);
        flowData.flowInfoAfter = flowInfo;
    }

    // function to validate single flow info change
    validateFlowInfoChange(flowName, txnTimestamp) {
        console.log(`validating ${flowName} flow change...`);
        const flowData = this.flows[flowName];

        if (!flowData.notTouched) {
            // validate flow info
            if (flowData.flowInfoAfter.flowRate.toString() !== "0") {
                assert.equal(
                    flowData.flowInfoAfter.timestamp.getTime() / 1000,
                    txnTimestamp,
                    `wrong flow timestamp of the ${flowName} flow`
                );
            } else {
                assert.equal(flowData.flowInfoAfter.timestamp.getTime(), 0);
            }
            assert.equal(
                flowData.flowInfoAfter.flowRate,
                this.expectedFlowInfo[flowName].flowRate.toString(),
                `wrong flow rate of the ${flowName} flow`
            );
            assert.equal(
                flowData.flowInfoAfter.owedDeposit,
                this.expectedFlowInfo[flowName].owedDeposit.toString(),
                `wrong owed deposit amount of the ${flowName} flow`
            );
            assert.equal(
                flowData.flowInfoAfter.deposit,
                this.expectedFlowInfo[flowName].deposit.toString(),
                `wrong deposit amount of the ${flowName} flow`
            );
        } else {
            assert.equal(
                flowData.flowInfoAfter.flowRate,
                flowData.flowInfoBefore.flowRate,
                `flow rate of the ${flowName} flow should not change`
            );
            assert.equal(
                flowData.flowInfoAfter.owedDeposit,
                flowData.flowInfoBefore.owedDeposit,
                `owed deposit amount of the ${flowName} flow should not change`
            );
            assert.equal(
                flowData.flowInfoAfter.deposit,
                flowData.flowInfoBefore.deposit,
                `deposit amount of the ${flowName} flow should not change`
            );
        }
    }

    // function to validate overall account flow info change
    validateAccountFlowInfoChange(role) {
        console.log(`validating ${role} account deposit changes...`);

        const inFlowNames = Object.keys(this.flows).filter(
            (i) => this.flows[i].flowId.receiver === this.roles[role]
        );
        const outFlowNames = Object.keys(this.flows).filter(
            (i) => this.flows[i].flowId.sender === this.roles[role]
        );
        console.log("in flows", inFlowNames);
        console.log("out flows", outFlowNames);

        let expectedDepositDelta = toBN(0);
        let expectedOwedDepositDelta = toBN(0);

        inFlowNames.forEach((flowName) => {
            const flowData = this.flows[flowName];
            expectedOwedDepositDelta = expectedOwedDepositDelta
                .add(toBN(flowData.flowInfoAfter.owedDeposit))
                .sub(toBN(flowData.flowInfoBefore.owedDeposit));
        });
        outFlowNames.forEach((flowName) => {
            const flowData = this.flows[flowName];
            expectedDepositDelta = expectedDepositDelta
                .add(toBN(flowData.flowInfoAfter.deposit))
                .sub(toBN(flowData.flowInfoBefore.deposit));
        });

        const flowRateDelta = toBN(
            this.getAccountFlowInfoAfter(role).flowRate
        ).sub(toBN(this.getAccountFlowInfoBefore(role).flowRate));
        assert.equal(
            flowRateDelta.toString(),
            this.expectedNetFlowDeltas[this.roles[role]].toString(),
            `wrong netflow delta of ${role}`
        );

        const depositDelta = toBN(
            this.getBalancesAfter(role).deposit.toString()
        ).sub(toBN(this.getBalancesBefore(role).deposit.toString()));
        assert.equal(
            depositDelta.toString(),
            expectedDepositDelta.toString(),
            `wrong deposit delta amount of ${role}`
        );

        const owedDepositDelta = toBN(
            this.getBalancesAfter(role).owedDeposit.toString()
        ).sub(toBN(this.getBalancesBefore(role).owedDeposit.toString()));
        assert.equal(
            owedDepositDelta.toString(),
            expectedOwedDepositDelta.toString(),
            `wrong owed deposit delta amount of ${role}`
        );
    }

    /** TestEnvironment Data Specific Updaters */
    //
    // Account flow info test data operations
    //
    updateAccountFlowInfo({superToken, account, flowInfo}) {
        _.merge(this.testenv.data, {
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
    getAccountFlowInfo({superToken, account}) {
        _.defaultsDeep(this.testenv.data, {
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
            this.testenv.data.tokens[superToken].accounts[account].cfa.flowInfo
        );
    }
    //
    // Flow info test data operations
    //
    updateFlowInfo({superToken, sender, receiver, flowInfo}) {
        _.merge(this.testenv.data, {
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

    getFlowInfo({superToken, sender, receiver}) {
        _.defaultsDeep(this.testenv.data, {
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
            this.testenv.data.tokens[superToken].cfa.flows[
                `${sender}:${receiver}`
            ]
        );
    }

    validateAccountNetFlow({superToken, account}) {
        const alias = this.testenv.toAlias(account);
        console.log(`validating ${alias} account net flow ...`);

        // ${sender}:${receiver} => flowInfo
        const flows = this.testenv.data.tokens[superToken].cfa.flows;

        const inFlows = Object.keys(flows)
            .filter((i) => i.endsWith(`:${account}`))
            .map((i) => flows[i]);
        const outFlows = Object.keys(flows)
            .filter((i) => i.startsWith(`${account}:`))
            .map((i) => flows[i]);
        console.log(
            "in flows",
            inFlows.map((i) => this.testenv.toAlias(i.sender))
        );
        console.log(
            "out flows",
            outFlows.map((i) => this.testenv.toAlias(i.receiver))
        );

        let actualNetFlow = toBN(0);

        inFlows.forEach((flowInfo) => {
            actualNetFlow = actualNetFlow.add(toBN(flowInfo.flowRate));
        });
        outFlows.forEach((flowInfo) => {
            actualNetFlow = actualNetFlow.sub(toBN(flowInfo.flowRate));
        });

        const accountFlowInfo = this.getAccountFlowInfo({
            superToken,
            account,
        });

        assert.equal(
            accountFlowInfo.flowRate.toString(),
            actualNetFlow.toString(),
            `unexpected netflow of ${alias}`
        );
    }

    syncAccountExpectedBalanceDeltas({superToken, timestamp}) {
        console.log(
            "syncing accounting expected balance deltas due to flows..."
        );

        this.testenv.listAddresses().forEach((account) => {
            const accountFlowInfo = this.getAccountFlowInfo({
                superToken,
                account,
            });
            const balanceSnapshot = this.testenv.getAccountBalanceSnapshot(
                superToken,
                account
            );
            const expectedBalanceDelta1 =
                this.testenv.getAccountExpectedBalanceDelta(
                    superToken,
                    account
                );
            const expectedBalanceDelta2 = expectedBalanceDelta1.add(
                toBN(accountFlowInfo.flowRate.toString()).mul(
                    toBN(timestamp.toString()).sub(
                        toBN(balanceSnapshot.timestamp.toString())
                    )
                )
            );
            this.testenv.updateAccountExpectedBalanceDelta(
                superToken,
                account,
                expectedBalanceDelta2
            );
        });
    }

    /** Static Functions */
    static _printFlowInfo(title, flowInfo) {
        console.log(
            title,
            flowInfo.timestamp.getTime(),
            flowInfo.flowRate.toString(),
            flowInfo.deposit.toString(),
            flowInfo.owedDeposit.toString()
        );
    }
    static clipDepositNumber(deposit, roundingDown = false) {
        // last 32 bites of the deposit (96 bites) is clipped off
        const rounding = roundingDown
            ? 0
            : deposit.and(toBN(0xffffffff)).isZero()
            ? 0
            : 1;
        return deposit.shr(32).add(rounding).shl(32);
    }

    static adjustNewAppCreditUsed(appCreditGranted, appCreditUsed) {
        // [appCreditUsed...appCreditGranted];
        return max(toBN(0), min(appCreditGranted, appCreditUsed));
    }
};
