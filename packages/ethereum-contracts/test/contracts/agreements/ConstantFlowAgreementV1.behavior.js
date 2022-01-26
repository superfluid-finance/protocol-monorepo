const {expectEvent} = require("@openzeppelin/test-helpers");
const {web3tx, toBN} = require("@decentral.ee/web3-helpers");
const CFADataModel = require("./ConstantFlowAgreementV1.data.js");
const MFASupport = require("../utils/MFASupport");

//
// test functions
//
async function _shouldChangeFlow({
    fn,
    testenv,
    superToken,
    sender,
    receiver,
    flowRate,
    mfa,
    userData,
    by,
    accountFlowInfo,
}) {
    console.log(`======== ${fn} begins ========`);
    console.log(`${sender} -> ${receiver} ${flowRate}`, by ? `by ${by}` : "");
    //console.log("!!! 1", JSON.stringify(testenv.data, null, 4));
    const {governance} = testenv.contracts;

    const cfaDataModel = new CFADataModel(testenv, superToken);

    let txBlock;

    const updateAccountExpectedBalanceDelta = (role, expectedBalanceDelta) => {
        testenv.updateAccountExpectedBalanceDelta(
            superToken.address,
            cfaDataModel.roles[role],
            expectedBalanceDelta
        );
    };
    const getAccountExpectedBalanceDelta = (role) => {
        return testenv.getAccountExpectedBalanceDelta(
            superToken.address,
            cfaDataModel.roles[role]
        );
    };

    // actual loading of data with the helper functions

    /**************************************************************************
     * Load current state and calculate expectations
     **************************************************************************/

    // add all roles
    cfaDataModel.addRole("sender", sender);
    cfaDataModel.addRole("receiver", receiver);
    if (fn === "deleteFlow") {
        assert.isDefined(by);
        const agentAddress = testenv.getAddress(by);
        let rewardAddress = await governance.getRewardAddress(
            testenv.sf.host.address,
            testenv.constants.ZERO_ADDRESS
        );
        if (rewardAddress === testenv.constants.ZERO_ADDRESS) {
            rewardAddress = agentAddress;
        }
        // agent is the liquidator (executor of deleteFlow)
        cfaDataModel.addRole("agent", by);
        cfaDataModel.addRole("reward", testenv.toAlias(rewardAddress));
    }
    if (mfa) {
        ({userData} = await MFASupport.setup({
            testenv,
            mfa,
            roles: cfaDataModel.roles,
        }));
    }
    console.log("--------");

    // load current balance snapshot
    Object.keys(cfaDataModel.roles).forEach((role) =>
        cfaDataModel.addToBalanceSnapshotsBefore(role)
    );
    console.log("--------");

    // load account flow info before
    Object.keys(cfaDataModel.roles).forEach((role) =>
        cfaDataModel.addAccountFlowInfoBefore(role)
    );
    console.log("--------");

    // load flow info before
    await cfaDataModel.addFlowInfoBefore("main", {
        sender: cfaDataModel.roles.sender,
        receiver: cfaDataModel.roles.receiver,
    });
    console.log("--------");

    // calculate expected net flow changes
    cfaDataModel.expectedNetFlowDeltas[cfaDataModel.roles.sender] = toBN(
        cfaDataModel.flows.main.flowInfoBefore.flowRate
    ).sub(toBN(flowRate));
    cfaDataModel.expectedNetFlowDeltas[cfaDataModel.roles.receiver] = toBN(
        flowRate
    ).sub(toBN(cfaDataModel.flows.main.flowInfoBefore.flowRate));
    if (fn === "deleteFlow") {
        if (!(cfaDataModel.roles.agent in cfaDataModel.expectedNetFlowDeltas)) {
            cfaDataModel.expectedNetFlowDeltas[cfaDataModel.roles.agent] =
                toBN(0);
        }
        if (
            !(cfaDataModel.roles.reward in cfaDataModel.expectedNetFlowDeltas)
        ) {
            cfaDataModel.expectedNetFlowDeltas[cfaDataModel.roles.reward] =
                toBN(0);
        }
    }

    // mfa support
    if (mfa) {
        await MFASupport.updateFlowExpectations({
            testenv,
            superToken,
            mfa,
            flowRate,
            cfaDataModel,
        });
        console.log("--------");
    }

    // calculate main flow expectations
    {
        const mainFlowDepositUnclipped = toBN(flowRate).mul(
            toBN(testenv.configs.LIQUIDATION_PERIOD)
        );
        // Aren't these two the exact same?
        const mainFlowDeposit = CFADataModel.clipDepositNumber(
            mainFlowDepositUnclipped,
            false /* rounding up */
        );
        const mainFlowAppAllowance = CFADataModel.clipDepositNumber(
            mainFlowDepositUnclipped,
            false /* rounding up */
        );
        const newAppAllowanceUsed = Object.values(cfaDataModel.expectedFlowInfo)
            .map((i) => i.deposit)
            .reduce((acc, cur) => {
                return acc.add(cur);
            }, toBN(0));
        const mainFlowAllowanceUsed = CFADataModel.adjustNewAppAllowanceUsed(
            mainFlowAppAllowance,
            mainFlowDeposit, // appAllowanceUsed
            newAppAllowanceUsed
        );

        cfaDataModel.expectedFlowInfo.main = {
            flowRate: toBN(flowRate),
            deposit:
                mainFlowDeposit
                    .add(mainFlowAllowanceUsed)
                    .lt(testenv.configs.MINIMUM_DEPOSIT) &&
                toBN(flowRate).gt(toBN(0))
                    ? testenv.configs.MINIMUM_DEPOSIT
                    : mainFlowDeposit.add(mainFlowAllowanceUsed),
            owedDeposit: mainFlowAllowanceUsed,
        };
    }

    // load balance before flow change
    await Promise.all(
        Object.keys(cfaDataModel.roles).map((roles) =>
            cfaDataModel.addToBalancesBefore(roles)
        )
    );
    const isSenderCritical = await superToken.isAccountCriticalNow(
        cfaDataModel.roles.sender
    );
    const isSenderSolvent = await superToken.isAccountSolventNow(
        cfaDataModel.roles.sender
    );
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
                testenv.sf.cfa[fn],
                `${fn} from ${sender} to ${receiver}`
            )({
                ...cfaDataModel.flows.main.flowId,
                flowRate: flowRate.toString(),
                userData,
            });
            break;
        case "deleteFlow":
            tx = await web3tx(
                testenv.sf.cfa[fn],
                `${fn} from ${sender} to ${receiver}`
            )({
                ...cfaDataModel.flows.main.flowId,
                by: cfaDataModel.roles.agent,
                userData,
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
        await MFASupport.postCheck({testenv, roles: cfaDataModel.roles});
    }

    // caculate additional expected balance changes per liquidation rules
    if (fn === "deleteFlow") {
        if (isSenderCritical) {
            console.log("validating liquidation rules...");
            // the tx itself may move the balance more
            // the time between the realtimeBalanceOfNow call
            // and the liquidation is time that the available
            // balance continues to decrease
            const netFlowRate = toBN(accountFlowInfo.flowRate);
            const adjustedRewardAmount = toBN(netFlowRate).mul(
                toBN(txBlock.timestamp).sub(
                    toBN(cfaDataModel.getBalancesBefore("sender").timestamp)
                )
            );
            if (isSenderSolvent) {
                // the reward recipient role depends on whether the time is still
                // in the patrician period, if it is, the reward recipient is the
                // rewardAccount, otherwise it is the "agent" or the person who
                // executes the liquidation

                // deposit = signedTotalDeposit
                const totalRewardLeft = cfaDataModel
                    .getBalancesBefore("sender")
                    .availableBalance.add(toBN(accountFlowInfo.deposit))
                    .add(adjustedRewardAmount);
                const expectedRewardAmount = toBN(
                    cfaDataModel.flows.main.flowInfoBefore.deposit
                )
                    .mul(totalRewardLeft)
                    .div(toBN(accountFlowInfo.deposit));
                const totalCFAOutflowRate = toBN(accountFlowInfo.deposit).div(
                    toBN(testenv.configs.LIQUIDATION_PERIOD)
                );
                const isPatricianPeriod = totalRewardLeft
                    .div(totalCFAOutflowRate)
                    .gt(
                        toBN(testenv.configs.LIQUIDATION_PERIOD).sub(
                            toBN(testenv.configs.PATRICIAN_PERIOD)
                        )
                    );
                const rewardRecipientRole = isPatricianPeriod
                    ? "reward"
                    : "agent";
                testenv.printSingleBalance(
                    `expected reward amount (to ${rewardRecipientRole} account)`,
                    expectedRewardAmount
                );

                updateAccountExpectedBalanceDelta(
                    rewardRecipientRole,
                    getAccountExpectedBalanceDelta(rewardRecipientRole).add(
                        expectedRewardAmount
                    )
                );
                updateAccountExpectedBalanceDelta(
                    "sender",
                    getAccountExpectedBalanceDelta("sender").sub(
                        expectedRewardAmount
                    )
                );
                const liquidationTypeData = web3.eth.abi.encodeParameters(
                    ["uint256", "uint8"],
                    [1, isPatricianPeriod ? 0 : 1]
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    testenv.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {
                        agreementClass: testenv.sf.agreements.cfa.address,
                        liquidatorAccount: cfaDataModel.roles.agent,
                        targetAccount: cfaDataModel.roles.sender,
                        rewardAccount: isPatricianPeriod
                            ? cfaDataModel.roles.reward
                            : cfaDataModel.roles.agent,
                        rewardAmount: expectedRewardAmount.toString(),
                        targetAccountBalanceDelta: expectedRewardAmount
                            .mul(toBN(-1))
                            .toString(),
                        liquidationTypeData,
                    }
                );
            } else {
                const expectedRewardAmount = toBN(
                    cfaDataModel.flows.main.flowInfoBefore.deposit
                );
                const expectedBailoutAmount = toBN(
                    cfaDataModel.getBalancesBefore("sender")
                        .availableBalance /* is negative */
                )
                    .add(toBN(accountFlowInfo.deposit))
                    .mul(toBN(-1))
                    // adjustedRewardAmount is negative, we want to add it
                    // to the overall amount
                    .sub(adjustedRewardAmount);
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
                const liquidationTypeData = web3.eth.abi.encodeParameters(
                    ["uint256", "uint8"],
                    [1, 2]
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    testenv.sf.contracts.ISuperToken,
                    "AgreementLiquidatedV2",
                    {
                        agreementClass: testenv.sf.agreements.cfa.address,
                        liquidatorAccount: cfaDataModel.roles.agent,
                        targetAccount: cfaDataModel.roles.sender,
                        rewardAccount: cfaDataModel.roles.agent,
                        rewardAmount: expectedRewardAmount.toString(),
                        targetAccountBalanceDelta:
                            expectedBailoutAmount.toString(),
                        liquidationTypeData,
                    }
                );
            }
            console.log("--------");
        }
    }

    await Promise.all(
        Object.keys(cfaDataModel.roles).map(async (role) => {
            await cfaDataModel.addToBalancesAfter(role);
            assert.equal(
                cfaDataModel.getBalancesAfter(role).timestamp,
                txBlock.timestamp
            );
        })
    );
    console.log("--------");

    await Promise.all(
        Object.keys(cfaDataModel.roles).map((role) =>
            cfaDataModel.addAccountFlowInfoAfter(role)
        )
    );
    console.log("--------");

    await Promise.all(
        Object.keys(cfaDataModel.flows).map((flow) =>
            cfaDataModel.addFlowInfoAfter(flow)
        )
    );
    console.log("--------");

    Object.keys(cfaDataModel.expectedFlowInfo).forEach((flowInfo) =>
        cfaDataModel.validateFlowInfoChange(flowInfo, txBlock.timestamp)
    );
    console.log("--------");

    Object.keys(cfaDataModel.roles).forEach((role) =>
        cfaDataModel.validateAccountFlowInfoChange(role)
    );
    console.log("--------");

    await testenv.validateExpectedBalances(() => {
        cfaDataModel.syncAccountExpectedBalanceDeltas({
            superToken: superToken.address,
            timestamp: txBlock.timestamp,
        });
    });
    console.log("--------");

    // update flow info
    Object.keys(cfaDataModel.flows).forEach((flowName) => {
        const flowData = cfaDataModel.flows[flowName];
        if (flowData.flowInfoAfter) {
            //console.log(`saving ${flowName} flow info...`);
            //console.log("!!!", flowName, flowData.flowInfoAfter);
            cfaDataModel.updateFlowInfo({
                superToken: superToken.address,
                sender: flowData.flowId.sender,
                receiver: flowData.flowId.receiver,
                flowInfo: flowData.flowInfoAfter,
            });
        }
    });

    // update account flow info
    Object.keys(cfaDataModel.roles).forEach((role) => {
        //console.log(`saving ${role} account flow info...`);
        if (cfaDataModel.getAccountFlowInfoAfter(role)) {
            cfaDataModel.updateAccountFlowInfo({
                superToken: superToken.address,
                account: cfaDataModel.roles[role],
                flowInfo: cfaDataModel.getAccountFlowInfoAfter(role),
            });
        }
    });

    Object.keys(cfaDataModel.roles).forEach((role) =>
        cfaDataModel.validateAccountNetFlow({
            superToken: superToken.address,
            account: cfaDataModel.roles[role],
        })
    );
    console.log("--------");

    // validate FlowUpdated event
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.agreements.cfa.contract,
        "FlowUpdated",
        {
            token: superToken.address,
            sender: cfaDataModel.roles.sender,
            receiver: cfaDataModel.roles.receiver,
            flowRate: flowRate.toString(),
            // we don't test total flow rates when using mfa
            // since mfa mangles with flows in callbacks
            ...(!mfa
                ? {
                      totalSenderFlowRate: cfaDataModel
                          .getAccountFlowInfo({
                              superToken: superToken.address,
                              account: cfaDataModel.roles.sender,
                          })
                          .flowRate.toString(),
                      totalReceiverFlowRate: cfaDataModel
                          .getAccountFlowInfo({
                              superToken: superToken.address,
                              account: mfa
                                  ? cfaDataModel.roles.mfa
                                  : cfaDataModel.roles.receiver,
                          })
                          .flowRate.toString(),
                  }
                : {}),
            userData: userData ? userData : null,
        }
    );
    console.log("--------");

    //console.log("!!! 2", JSON.stringify(testenv.data, null, 4));
    console.log(`======== ${fn} ends ========`);
}

async function shouldCreateFlow({
    testenv,
    superToken,
    sender,
    receiver,
    flowRate,
    mfa,
}) {
    await _shouldChangeFlow({
        fn: "createFlow",
        testenv,
        superToken,
        sender,
        receiver,
        flowRate,
        mfa,
    });
}

async function shouldUpdateFlow({
    testenv,
    superToken,
    sender,
    receiver,
    flowRate,
    mfa,
}) {
    await _shouldChangeFlow({
        fn: "updateFlow",
        testenv,
        superToken,
        sender,
        receiver,
        flowRate,
        mfa,
    });
}

async function shouldDeleteFlow({
    testenv,
    superToken,
    sender,
    receiver,
    mfa,
    by,
    accountFlowInfo,
}) {
    await _shouldChangeFlow({
        fn: "deleteFlow",
        testenv,
        superToken,
        sender,
        receiver,
        flowRate: 0,
        mfa,
        by,
        accountFlowInfo,
    });
}

module.exports = {
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
};
