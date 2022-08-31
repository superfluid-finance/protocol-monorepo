const {expectCustomError} = require("../../utils/expectRevert");
const {toBN} = require("../utils/helpers");
const CFADataModel = require("./ConstantFlowAgreementV1.data.js");
const MFASupport = require("../utils/MFASupport");
const {ethers} = require("hardhat");
const {expect} = require("chai");
const expectEvent = require("@openzeppelin/test-helpers/src/expectEvent");
const {web3tx} = require("@decentral.ee/web3-helpers");

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
    const isDeleteFlow = ["deleteFlow", "deleteFlowByOperator"].includes(fn);
    if (isDeleteFlow) {
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
        // or agent is the flowOperator
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
    if (isDeleteFlow) {
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
        const mainFlowDeposit = CFADataModel.clipDepositNumber(
            mainFlowDepositUnclipped,
            false /* rounding up */
        );
        let mainFlowAppCreditGranted = CFADataModel.clipDepositNumber(
            mainFlowDepositUnclipped,
            false /* rounding up */
        );
        // @note - add minimum deposit amount to appCreditGranted when
        // sending to an app (mfa)
        mainFlowAppCreditGranted =
            mfa && toBN(flowRate).gt(toBN(0))
                ? mainFlowAppCreditGranted.add(testenv.configs.MINIMUM_DEPOSIT)
                : mainFlowAppCreditGranted;
        const appCreditUsed = Object.entries(cfaDataModel.expectedFlowInfo)
            .map((x) => {
                const depositBefore =
                    cfaDataModel.flows[x[0]].flowInfoBefore.deposit;
                return x[1].deposit.sub(toBN(depositBefore));
            })
            .reduce((acc, cur) => acc.add(cur), toBN(0))
            .add(toBN(cfaDataModel.flows.main.flowInfoBefore.owedDeposit));

        const mainFlowCreditUsed = CFADataModel.adjustNewAppCreditUsed(
            mainFlowAppCreditGranted,
            appCreditUsed
        );

        cfaDataModel.expectedFlowInfo.main = {
            flowRate: toBN(flowRate),
            deposit:
                mainFlowDeposit
                    .add(mainFlowCreditUsed)
                    .lt(testenv.configs.MINIMUM_DEPOSIT) &&
                toBN(flowRate).gt(toBN(0))
                    ? testenv.configs.MINIMUM_DEPOSIT
                    : mainFlowDeposit.add(mainFlowCreditUsed),
            owedDeposit: mfa ? mainFlowCreditUsed : toBN(0),
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
    let superfluid;
    let cfa;
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
        case "createFlowByOperator":
        case "updateFlowByOperator":
            cfa = await testenv.sf.contracts.IConstantFlowAgreementV1.at(
                testenv.contracts.cfa.address
            );
            superfluid = await testenv.sf.contracts.ISuperfluid.at(
                testenv.contracts.superfluid.address
            );
            tx = await superfluid.callAgreement(
                testenv.contracts.cfa.address,
                cfa.contract.methods[fn](
                    cfaDataModel.flows.main.flowId.superToken,
                    cfaDataModel.flows.main.flowId.sender,
                    cfaDataModel.flows.main.flowId.receiver,
                    flowRate.toString(),
                    "0x"
                ).encodeABI(),
                "0x",
                {from: testenv.getAddress(by)}
            );
            break;
        case "deleteFlowByOperator":
            cfa = await testenv.sf.contracts.IConstantFlowAgreementV1.at(
                testenv.contracts.cfa.address
            );
            superfluid = await testenv.sf.contracts.ISuperfluid.at(
                testenv.contracts.superfluid.address
            );
            tx = await superfluid.callAgreement(
                testenv.contracts.cfa.address,
                cfa.contract.methods[fn](
                    cfaDataModel.flows.main.flowId.superToken,
                    cfaDataModel.flows.main.flowId.sender,
                    cfaDataModel.flows.main.flowId.receiver,
                    "0x"
                ).encodeABI(),
                "0x",
                {from: cfaDataModel.roles.agent}
            );
            break;
        default:
            assert(false);
    }
    txBlock = await ethers.provider.getBlock(tx.blockNumber);
    console.log("--------");

    /**************************************************************************
     * load results and test expectations
     **************************************************************************/

    // make sure app is not jailed
    if (mfa) {
        await MFASupport.postCheck({testenv, roles: cfaDataModel.roles});
    }

    // calculate additional expected balance changes per liquidation rules
    if (isDeleteFlow) {
        if (isSenderCritical) {
            console.log("validating liquidation rules...");
            // the tx itself may move the balance more
            // the time between the realtimeBalanceOfNow call
            // and the liquidation is time that the available
            // balance continues to decrease
            const netFlowRate = toBN(accountFlowInfo.flowRate);
            const adjustedRewardAmount = toBN(netFlowRate).mul(
                toBN(txBlock.timestamp).sub(
                    toBN(
                        cfaDataModel
                            .getBalancesBefore("sender")
                            .timestamp.toString()
                    )
                )
            );
            if (isSenderSolvent) {
                // the rewardAmountReceiver depends on whether the time is still
                // in the patrician period, if it is, the rewardAmountReceiver is the
                // rewardAccount, otherwise it is the "agent" or the person who
                // executes the liquidation

                // deposit = signedTotalDeposit
                const totalRewardLeft = toBN(
                    cfaDataModel
                        .getBalancesBefore("sender")
                        .availableBalance.toString()
                )
                    .add(toBN(accountFlowInfo.deposit.toString()))
                    .add(adjustedRewardAmount);
                const expectedRewardAmount = toBN(
                    cfaDataModel.flows.main.flowInfoBefore.deposit.toString()
                )
                    .mul(totalRewardLeft)
                    .div(toBN(accountFlowInfo.deposit.toString()));
                const totalCFAOutflowRate = toBN(
                    accountFlowInfo.deposit.toString()
                ).div(toBN(testenv.configs.LIQUIDATION_PERIOD));
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
                        rewardAmountReceiver: isPatricianPeriod
                            ? cfaDataModel.roles.reward
                            : cfaDataModel.roles.agent,
                        rewardAmount: expectedRewardAmount.toString(),
                        targetAccountBalanceDelta: expectedRewardAmount
                            .mul(toBN(-1))
                            .toString(),
                        liquidationTypeData,
                    }
                );

                // targetAccount (sender) transferring remaining deposit to
                // rewardAccount / liquidatorAccount depending on isPatricianPeriod
                await expectEvent.inTransaction(
                    tx.tx,
                    testenv.sf.contracts.ISuperToken,
                    "Transfer",
                    {
                        from: cfaDataModel.roles.sender,
                        to: isPatricianPeriod
                            ? cfaDataModel.roles.reward
                            : cfaDataModel.roles.agent,
                        value: expectedRewardAmount.toString(),
                    }
                );
            } else {
                const expectedRewardAmount = toBN(
                    cfaDataModel.flows.main.flowInfoBefore.deposit
                );
                const expectedBailoutAmount = toBN(
                    cfaDataModel
                        .getBalancesBefore("sender")
                        .availableBalance.toString() /* is negative */
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
                        rewardAmountReceiver: cfaDataModel.roles.agent,
                        rewardAmount: expectedRewardAmount.toString(),
                        targetAccountBalanceDelta:
                            expectedBailoutAmount.toString(),
                        liquidationTypeData,
                    }
                );

                // reward account transferring the single flow deposit to the
                // liquidator (agent)
                await expectEvent.inTransaction(
                    tx.tx,
                    testenv.sf.contracts.ISuperToken,
                    "Transfer",
                    {
                        from: cfaDataModel.roles.reward,
                        to: cfaDataModel.roles.agent,
                        value: expectedRewardAmount.toString(),
                    }
                );

                // reward account bailing out the targetAccount (sender)
                await expectEvent.inTransaction(
                    tx.tx,
                    testenv.sf.contracts.ISuperToken,
                    "Transfer",
                    {
                        from: cfaDataModel.roles.reward,
                        to: cfaDataModel.roles.sender,
                        value: expectedBailoutAmount.toString(),
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
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.agreements.cfa.contract,
        "FlowUpdatedExtension",
        {
            flowOperator: testenv.getAddress(by) || cfaDataModel.roles.sender,
            // we don't test total flow rates when using mfa
            // since mfa mangles with flows in callbacks
            // similarly with deposit, we can't get the expected deposit
            // from main.deposit
            ...(!mfa
                ? {
                      deposit: cfaDataModel.expectedFlowInfo.main.deposit,
                  }
                : {}),
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

async function shouldCreateFlowByOperator({
    testenv,
    superToken,
    sender,
    receiver,
    flowRate,
    mfa,
    flowOperator,
}) {
    await _shouldChangeFlow({
        fn: "createFlowByOperator",
        testenv,
        superToken,
        sender,
        receiver,
        flowRate,
        mfa,
        by: flowOperator,
    });
}

async function shouldUpdateFlowByOperator({
    testenv,
    superToken,
    sender,
    receiver,
    flowRate,
    mfa,
    flowOperator,
}) {
    await _shouldChangeFlow({
        fn: "updateFlowByOperator",
        testenv,
        superToken,
        sender,
        receiver,
        flowRate,
        mfa,
        by: flowOperator,
    });
}
async function shouldDeleteFlowByOperator({
    testenv,
    superToken,
    sender,
    receiver,
    mfa,
    flowOperator,
    accountFlowInfo,
}) {
    await _shouldChangeFlow({
        fn: "deleteFlowByOperator",
        testenv,
        superToken,
        sender,
        receiver,
        flowRate: 0,
        mfa,
        by: flowOperator,
        accountFlowInfo,
    });
}

function getUpdateFlowOperatorPermissionsPromise({
    testenv,
    token,
    flowOperator,
    permissions,
    flowRateAllowance,
    ctx,
    signer,
}) {
    const {cfa, superfluid} = testenv.contracts;
    return superfluid
        .connect(signer)
        .callAgreement(
            cfa.address,
            testenv.agreementHelper.cfaInterface.encodeFunctionData(
                "updateFlowOperatorPermissions",
                [
                    token,
                    flowOperator,
                    permissions,
                    flowRateAllowance.toString(),
                    ctx,
                ]
            ),
            "0x"
        );
}

function getAuthorizeOrRevokeFlowOperatorWithFullControlPromise({
    testenv,
    token,
    flowOperator,
    ctx,
    isRevokeFullControl,
    signer,
}) {
    const {cfa, superfluid} = testenv.contracts;
    const methodSignature = isRevokeFullControl
        ? "revokeFlowOperatorWithFullControl"
        : "authorizeFlowOperatorWithFullControl";
    return superfluid
        .connect(signer)
        .callAgreement(
            cfa.address,
            testenv.agreementHelper.cfaInterface.encodeFunctionData(
                methodSignature,
                [token, flowOperator, ctx]
            ),
            "0x"
        );
}

async function getChangeFlowByFlowOperatorPromise({
    testenv,
    methodSignature,
    token,
    sender,
    receiver,
    signer,
    flowRate,
}) {
    const {cfa, superfluid} = testenv.contracts;
    if (methodSignature === "deleteFlowByOperator") {
        return superfluid
            .connect(signer)
            .callAgreement(
                cfa.address,
                testenv.agreementHelper.cfaInterface.encodeFunctionData(
                    "deleteFlowByOperator",
                    [token, sender, receiver, "0x"]
                ),
                "0x"
            );
    }
    return superfluid
        .connect(signer)
        .callAgreement(
            cfa.address,
            testenv.agreementHelper.cfaInterface.encodeFunctionData(
                methodSignature,
                [token, sender, receiver, flowRate.toString(), "0x"]
            ),
            "0x"
        );
}

async function shouldRevertUpdateFlowOperatorPermissions({
    testenv,
    token,
    flowOperator,
    permissions,
    flowRateAllowance,
    ctx,
    signer,
    expectedCustomError,
}) {
    console.log("\n[EXPECT UPDATE FLOW OPERATOR PERMISSIONS REVERT]");
    console.log(
        `${signer.address} granting ${permissions} to ${flowOperator} with ${flowRateAllowance} flow rate allowance`
    );
    await expectCustomError(
        getUpdateFlowOperatorPermissionsPromise({
            testenv,
            token,
            flowOperator,
            permissions,
            flowRateAllowance,
            ctx,
            signer,
        }),
        testenv.contracts.cfa,
        expectedCustomError
    );
}

/**
 * @description Updates the flow operator permissions and validates that the
 * event emits the correct values (newly defined values) and that the
 * agreementData was properly updated.
 */
async function shouldUpdateFlowOperatorPermissionsAndValidateEvent({
    testenv,
    token,
    flowOperator,
    permissions,
    flowRateAllowance,
    ctx,
    signer,
    isFullControl,
    isFullControlRevoke,
}) {
    const {cfa} = testenv.contracts;
    const {MAXIMUM_FLOW_RATE} = testenv.constants;
    if (isFullControl && isFullControlRevoke) {
        throw new Error("You cannot grant full control and revoke it.");
    }

    const expectedPermissions = isFullControl
        ? 7 // 1 1 1
        : isFullControlRevoke
        ? 0 // 0 0 0
        : Number(permissions);
    const expectedFlowRateAllowance = isFullControl
        ? MAXIMUM_FLOW_RATE
        : isFullControlRevoke
        ? "0"
        : flowRateAllowance;

    // updateFlowOperatorPermissions &&
    // validate event was emitted with correct values
    if (!isFullControl && !isFullControlRevoke) {
        await expect(
            getUpdateFlowOperatorPermissionsPromise({
                testenv,
                token,
                flowOperator,
                permissions,
                flowRateAllowance,
                ctx,
                signer,
            })
        )
            .to.emit(cfa, "FlowOperatorUpdated")
            .withArgs(
                token,
                signer.address,
                flowOperator,
                expectedPermissions,
                expectedFlowRateAllowance
            );
    }

    if (isFullControl || isFullControlRevoke) {
        await expect(
            getAuthorizeOrRevokeFlowOperatorWithFullControlPromise({
                testenv,
                token,
                flowOperator,
                ctx,
                signer,
                isRevokeFullControl: isFullControlRevoke,
            })
        )
            .to.emit(cfa, "FlowOperatorUpdated")
            .withArgs(
                token,
                signer.address,
                flowOperator,
                expectedPermissions,
                expectedFlowRateAllowance
            );
    }

    // validate agreementData was properly updated
    const data = await cfa.getFlowOperatorData(
        token,
        signer.address,
        flowOperator
    );

    const expectedFlowOperatorId = testenv.getFlowOperatorId(
        signer.address,
        flowOperator
    );
    assert.equal(data.flowOperatorId, expectedFlowOperatorId);
    assert.equal(data.permissions.toString(), expectedPermissions);
    assert.equal(data.flowRateAllowance.toString(), expectedFlowRateAllowance);
}

async function shouldRevertChangeFlowByOperator({
    testenv,
    methodSignature,
    token,
    sender,
    receiver,
    flowOperator,
    flowRate,
    ctx,
    expectedCustomError,
}) {
    console.log("\n[EXPECT CHANGE FLOW BY OPERATOR REVERT]");
    console.log(
        `${methodSignature}: ${sender} to ${receiver} at ${flowRate}/s by ${flowOperator}`
    );
    const signer = await ethers.getSigner(flowOperator);
    await expectCustomError(
        getChangeFlowByFlowOperatorPromise({
            testenv,
            methodSignature,
            token,
            sender,
            receiver,
            signer,
            flowRate,
            ctx,
        }),
        testenv.contracts.cfa,
        expectedCustomError
    );
}

async function expectNetFlow({testenv, account, superToken, value}) {
    const actualNetFlowRate = await testenv.contracts.cfa.getNetFlow(
        superToken.address,
        testenv.getAddress(account)
    );
    console.log(`expected net flow for ${account}: ${value.toString()}`);
    assert.equal(
        actualNetFlowRate.toString(),
        value.toString(),
        `Unexpected net flow for ${account}`
    );
}

async function expectFlow({
    testenv,
    sender,
    receiver,
    superToken,
    flowRate,
    deposit,
    owedDeposit,
}) {
    const flowData = await testenv.contracts.cfa.getFlow(
        superToken.address,
        testenv.getAddress(sender),
        testenv.getAddress(receiver)
    );
    console.log(
        `expected flow rate for ${sender}->${receiver} flow: ${flowRate.toString()}`
    );
    assert.equal(
        flowData.flowRate.toString(),
        flowRate.toString(),
        "Unexpected flowRate"
    );
    console.log(
        `expected deposit for ${sender}->${receiver} flow: ${deposit.toString()}`
    );
    assert.equal(
        flowData.deposit.toString(),
        deposit.toString(),
        "Unexpected deposit"
    );
    console.log(
        `expected owedDeposit for ${sender}->${receiver} flow: ${owedDeposit.toString()}`
    );
    assert.equal(
        flowData.owedDeposit.toString(),
        owedDeposit.toString(),
        "Unexpected owedDeposit"
    );
}

async function expectDepositAndOwedDeposit({
    testenv,
    account,
    superToken,
    deposit,
    owedDeposit,
}) {
    const flowData = await testenv.contracts.cfa.getAccountFlowInfo(
        superToken.address,
        testenv.getAddress(account)
    );

    console.log(`expected deposit for ${account}: ${deposit.toString()}`);
    assert.equal(
        flowData.deposit.toString(),
        deposit.toString(),
        "Unexpected deposit"
    );

    console.log(
        `expected owedDeposit for ${account}: ${owedDeposit.toString()}`
    );
    assert.equal(
        flowData.owedDeposit.toString(),
        owedDeposit.toString(),
        "Unexpected owedDeposit"
    );
}

/**
 * Gets the clipped deposit given a flowRate and test environment
 * @returns
 */
function getDeposit({testenv, flowRate}) {
    return CFADataModel.clipDepositNumber(
        flowRate.mul(toBN(testenv.configs.LIQUIDATION_PERIOD))
    );
}

module.exports = {
    shouldCreateFlow,
    shouldUpdateFlow,
    shouldDeleteFlow,
    shouldCreateFlowByOperator,
    shouldUpdateFlowByOperator,
    shouldDeleteFlowByOperator,
    shouldRevertUpdateFlowOperatorPermissions,
    shouldUpdateFlowOperatorPermissionsAndValidateEvent,
    shouldRevertChangeFlowByOperator,
    expectNetFlow,
    expectFlow,
    expectDepositAndOwedDeposit,
    getDeposit,
};
