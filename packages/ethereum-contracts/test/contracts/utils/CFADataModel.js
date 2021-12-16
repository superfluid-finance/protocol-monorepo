const {getAccountFlowInfo, _printFlowInfo} = require("./CFAV1utils");
const {toBN} = require("@decentral.ee/web3-helpers");

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
        this._accountFlowInfoBefore[this.roles[role]] = getAccountFlowInfo({
            testenv: this.testenv,
            superToken: this.superToken.address,
            account: this.roles[role],
        });
        _printFlowInfo(
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
        _printFlowInfo(
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
        _printFlowInfo(`${flowName} flow info before`, flowInfo);
    }
    async addFlowInfoAfter(flowName) {
        const flowData = this.flows[flowName];
        const flowInfo = await this.testenv.sf.cfa.getFlow(flowData.flowId);
        _printFlowInfo(`${flowName} flow info after`, flowInfo);
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

        const depositDelta = toBN(this.getBalancesAfter(role).deposit).sub(
            toBN(this.getBalancesBefore(role).deposit)
        );
        assert.equal(
            depositDelta.toString(),
            expectedDepositDelta.toString(),
            `wrong deposit delta amount of ${role}`
        );

        const owedDepositDelta = toBN(
            this.getBalancesAfter(role).owedDeposit
        ).sub(toBN(this.getBalancesBefore(role).owedDeposit));
        assert.equal(
            owedDepositDelta.toString(),
            expectedOwedDepositDelta.toString(),
            `wrong owed deposit delta amount of ${role}`
        );
    }
};
