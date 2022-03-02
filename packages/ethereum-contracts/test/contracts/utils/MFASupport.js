const {toBN} = require("@decentral.ee/web3-helpers");
const CFADataModel = require("../agreements/ConstantFlowAgreementV1.data.js");

module.exports = class MFASupport {
    static async setup({testenv, mfa, roles}) {
        roles.mfaSender = testenv.getAddress(mfa.sender);
        roles.mfa = testenv.getAddress("mfa");

        Object.keys(mfa.receivers).forEach(async (receiverAlias) => {
            const mfaReceiverName = "mfa.receiver." + receiverAlias;
            roles[mfaReceiverName] = testenv.getAddress(receiverAlias);
            console.log(
                `${receiverAlias} account address ${roles[mfaReceiverName]} (${receiverAlias})`
            );
        });

        const receivers = Object.keys(mfa.receivers).filter(
            (i) => mfa.receivers[i].proportion > 0
        );
        return {
            userData: web3.eth.abi.encodeParameters(
                ["address", "uint256", "address[]", "uint256[]"],
                [
                    testenv.getAddress(mfa.sender),
                    mfa.ratioPct,
                    receivers.map((i) => testenv.getAddress(i)),
                    receivers.map((i) => mfa.receivers[i].proportion),
                ]
            ),
        };
    }

    static async updateFlowExpectations({
        testenv,
        superToken,
        mfa,
        flowRate,
        cfaDataModel,
    }) {
        const roles = cfaDataModel.roles;
        const expectedNetFlowDeltas = cfaDataModel.expectedNetFlowDeltas;
        const expectedFlowInfo = cfaDataModel.expectedFlowInfo;
        let totalProportions = Object.values(mfa.receivers)
            .map((i) => i.proportion)
            .reduce((acc, cur) => acc + cur, 0);

        const depositAllowance = CFADataModel.clipDepositNumber(
            toBN(flowRate).mul(toBN(testenv.configs.LIQUIDATION_PERIOD)),
            true /* rounding down */
        );

        // expected unwindng of mfa receiver flows
        Object.keys(mfa.receivers).forEach((receiverAlias) => {
            const receiverAddress = testenv.getAddress(receiverAlias);
            if (!(receiverAddress in expectedNetFlowDeltas)) {
                expectedNetFlowDeltas[receiverAddress] = toBN(0);
            }
        });
        await Promise.all(
            Object.keys(mfa.receivers).map(async (receiverAlias) => {
                const mfaReceiverName = "mfa.receiver." + receiverAlias;
                const mfaFlowName = "mfa.flow." + receiverAlias;
                const receiverAddress = testenv.getAddress(receiverAlias);
                const notTouched =
                    mfa.receivers[receiverAlias].proportion === 0;

                // skip if it's been deleted by one of the mfa receivers
                if (receiverAddress == roles.receiver) return;

                await cfaDataModel.addFlowInfoBefore(mfaFlowName, {
                    sender: roles.mfa,
                    receiver: roles[mfaReceiverName],
                    notTouched,
                });

                const originalMfaFlowDepositAllowance =
                    CFADataModel.clipDepositNumber(
                        toBN(depositAllowance)
                            .mul(toBN(mfa.receivers[receiverAlias].proportion))
                            .mul(toBN(mfa.ratioPct))
                            .divn(100)
                            .div(toBN(totalProportions)),
                        true /* rounding down */
                    );
                const mfaFlowDepositAllowance =
                    originalMfaFlowDepositAllowance.lt(
                        testenv.configs.MINIMUM_DEPOSIT
                    ) && toBN(flowRate).gt(toBN(0))
                        ? testenv.configs.MINIMUM_DEPOSIT
                        : originalMfaFlowDepositAllowance;

                const mfaFlowRate = toBN(originalMfaFlowDepositAllowance).div(
                    toBN(testenv.configs.LIQUIDATION_PERIOD)
                );

                if (!notTouched) {
                    const mfaFlowRateDelta = toBN(mfaFlowRate).sub(
                        toBN(
                            cfaDataModel.getAccountFlowInfoBefore(
                                mfaReceiverName
                            ).flowRate
                        )
                    );
                    expectedNetFlowDeltas[receiverAddress].iadd(
                        mfaFlowRateDelta
                    );
                    expectedNetFlowDeltas[roles.mfa].isub(mfaFlowRateDelta);
                }

                expectedFlowInfo[mfaFlowName] = {
                    flowRate: mfaFlowRate,
                    deposit: mfaFlowDepositAllowance,
                    owedDeposit: toBN(0),
                };
            })
        );

        // expected unwindng of mfa sender flow if the flow being deleted is not sending to the mfa
        if (roles.mfa != roles.receiver) {
            const mfaSenderFlow = cfaDataModel.getFlowInfo({
                superToken: superToken.address,
                sender: roles.mfaSender,
                receiver: roles.mfa,
            });
            if (!(roles.mfaSender in expectedNetFlowDeltas))
                expectedNetFlowDeltas[roles.mfaSender] = toBN(0);
            if (!(roles.mfa in expectedNetFlowDeltas))
                expectedNetFlowDeltas[roles.mfa] = toBN(0);
            expectedNetFlowDeltas[roles.mfaSender].iadd(
                toBN(mfaSenderFlow.flowRate)
            );
            expectedNetFlowDeltas[roles.mfa].isub(toBN(mfaSenderFlow.flowRate));
            await cfaDataModel.addFlowInfoBefore("mfa.sender", {
                sender: roles.mfaSender,
                receiver: roles.mfa,
            });
            expectedFlowInfo["mfa.sender"] = {
                flowRate: "0",
                deposit: toBN(0),
                owedDeposit: toBN(0),
            };
        }
    }

    static async postCheck({testenv, roles}) {
        assert.isFalse(
            await testenv.contracts.superfluid.isAppJailed(roles.mfa),
            "MFA app was jailed"
        );
    }
};
