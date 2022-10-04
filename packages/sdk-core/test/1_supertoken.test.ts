import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import hre from "hardhat";
import {
    AUTHORIZE_FLOW_OPERATOR_CREATE,
    getFlowOperatorId,
    NativeAssetSuperToken,
    SuperToken,
    toBN,
} from "../src";
import { getPerSecondFlowRateByMonth } from "../src";
import { BigNumber, ethers } from "ethers";
import { AUTHORIZE_FULL_CONTROL } from "../src";
import { TestEnvironment, _makeSuite } from "./TestEnvironment";

export const clipDepositNumber = (deposit: BigNumber, roundingDown = false) => {
    // last 32 bits of the deposit (96 bits) is clipped off
    const rounding = roundingDown
        ? 0
        : deposit.and(toBN(0xffffffff)).isZero()
        ? 0
        : 1;
    return deposit.shr(32).add(toBN(rounding)).shl(32);
};

_makeSuite("SuperToken Tests", (testEnv: TestEnvironment) => {
    async function createFlowWithOperator(
        flowOperator: SignerWithAddress,
        sender: SignerWithAddress,
        receiver: SignerWithAddress
    ) {
        const flowRateAllowance = getPerSecondFlowRateByMonth("100");
        let permissions = AUTHORIZE_FULL_CONTROL;

        const updateFlowOperatorPermissionsOperation =
            testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                flowRateAllowance,
                flowOperator: flowOperator.address,
                permissions,
            });
        await updateFlowOperatorPermissionsOperation.exec(sender);

        let flowRate = getPerSecondFlowRateByMonth("100");

        await testEnv.wrapperSuperToken
            .createFlowByOperator({
                flowRate,
                sender: sender.address,
                receiver: receiver.address,
            })
            .exec(flowOperator);
    }
    async function approveAndDowngrade(
        signer: SignerWithAddress,
        amount: string = "2000"
    ) {
        amount = ethers.utils.parseUnits(amount).toString();
        await expect(
            testEnv.wrapperSuperToken
                .approve({
                    receiver: testEnv.wrapperSuperToken.address,
                    amount,
                })
                .exec(signer)
        )
            .to.emit(
                testEnv.wrapperSuperToken.contract.connect(
                    testEnv.alice.provider!
                ),
                "Approval"
            )
            .withArgs(
                signer.address,
                testEnv.wrapperSuperToken.address,
                amount
            );
        await expect(
            testEnv.wrapperSuperToken.downgrade({ amount }).exec(signer)
        )
            .to.emit(
                testEnv.wrapperSuperToken.contract.connect(
                    testEnv.alice.provider!
                ),
                "TokenDowngraded"
            )
            .withArgs(signer.address, amount);
    }

    describe("ERC20 and SuperToken Basic Functionality Tests", () => {
        it("Should throw an error if SuperToken isn't initialized properly.", async () => {
            try {
                await SuperToken.create({
                    address: testEnv.wrapperSuperToken.address,
                    provider: testEnv.alice.provider!,
                    config: {
                        resolverAddress: "",
                        hostAddress: "",
                        cfaV1Address: "",
                        idaV1Address: "",
                        governanceAddress: "",
                    },
                });
            } catch (err: any) {
                expect(err.message).to.equal(
                    "SuperToken Initialization Error: You must input chainId or networkName."
                );
                expect(err.cause).to.be.undefined;
            }

            try {
                await SuperToken.create({
                    address: testEnv.wrapperSuperToken.address,
                    provider: "" as any,
                    networkName: "custom",
                    config: {
                        resolverAddress: "",
                        hostAddress: "",
                        cfaV1Address: "",
                        idaV1Address: "",
                        governanceAddress: "",
                    },
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Initialization Error: There was an error initializing the SuperToken"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }
        });

        it("Should throw an error on SuperToken read operations when incorrect input is passed", async () => {
            try {
                await testEnv.wrapperSuperToken.realtimeBalanceOf({
                    providerOrSigner: testEnv.alice,
                    account: testEnv.bob.address,
                    timestamp: -1,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error: There was an error getting realtimeBalanceOf"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }
        });

        it("Should throw an error on Token read operations when incorrect input is passed", async () => {
            // NOTE: testEnv.alice.provider! is string as any to get this to throw an error on read
            try {
                await testEnv.wrapperSuperToken.allowance({
                    owner: testEnv.alice.address,
                    spender: testEnv.bob.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error: There was an error getting allowance"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }

            try {
                await testEnv.wrapperSuperToken.balanceOf({
                    account: testEnv.alice.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error: There was an error getting balanceOf"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }

            try {
                await testEnv.wrapperSuperToken.name({
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error: There was an error getting name"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }

            try {
                await testEnv.wrapperSuperToken.symbol({
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error: There was an error getting symbol"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }

            try {
                await testEnv.wrapperSuperToken.totalSupply({
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error: There was an error getting totalSupply"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }
        });

        it("Should properly return totalSupply", async () => {
            const totalSupply = await testEnv.wrapperSuperToken.totalSupply({
                providerOrSigner: testEnv.alice,
            });
            expect(totalSupply).to.equal(
                testEnv.constants.INITIAL_TOKEN_BALANCE.mul(
                    toBN(testEnv.users.length)
                ).toString()
            );
        });

        it("Should properly return balanceOf", async () => {
            const balance = await testEnv.wrapperSuperToken.balanceOf({
                account: testEnv.charlie.address,
                providerOrSigner: testEnv.alice,
            });
            expect(balance).to.equal(
                testEnv.constants.INITIAL_TOKEN_BALANCE.toString()
            );
        });

        it("Should properly return allowance", async () => {
            const initialAllowance = await testEnv.wrapperSuperToken.allowance({
                owner: testEnv.alice.address,
                spender: testEnv.bob.address,
                providerOrSigner: testEnv.alice,
            });
            expect(initialAllowance).to.equal("0");
            const allowanceAmount = ethers.utils.parseUnits("100").toString();
            const operation = testEnv.wrapperSuperToken.approve({
                receiver: testEnv.bob.address,
                amount: allowanceAmount,
            });
            await operation.exec(testEnv.alice);
            const approvedAllowance = await testEnv.wrapperSuperToken.allowance(
                {
                    owner: testEnv.alice.address,
                    spender: testEnv.bob.address,
                    providerOrSigner: testEnv.alice,
                }
            );
            expect(approvedAllowance).to.equal(allowanceAmount);
        });

        it("Should properly return name", async () => {
            const name = await testEnv.wrapperSuperToken.name({
                providerOrSigner: testEnv.alice,
            });
            expect(name).to.equal("Super fDAI");
        });

        it("Should properly return symbol", async () => {
            const symbol = await testEnv.wrapperSuperToken.symbol({
                providerOrSigner: testEnv.alice,
            });
            expect(symbol).to.equal("fDAIx");
        });

        it("Should properly initialize SuperToken", async () => {
            const daixTest = await testEnv.sdkFramework.loadSuperToken(
                testEnv.wrapperSuperToken.address
            );
            expect(testEnv.wrapperSuperToken.address).to.equal(
                daixTest.settings.address
            );
        });

        it("Should be able to initialize SuperToken with networkName.", () => {
            SuperToken.create({
                address: testEnv.wrapperSuperToken.address,
                provider: testEnv.alice.provider!,
                config: testEnv.sdkFramework.settings.config,
                networkName: "custom",
            });
        });

        it("Should be able to get realtimeBalanceOf", async () => {
            await testEnv.wrapperSuperToken.realtimeBalanceOf({
                providerOrSigner: testEnv.alice,
                account: testEnv.alice.address,
            });
        });

        it("Should be able to approve + downgrade", async () => {
            await approveAndDowngrade(testEnv.alice);
        });

        it("Should be able to transfer downgraded tokens", async () => {
            await approveAndDowngrade(testEnv.alice);
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                testEnv.wrapperSuperToken.underlyingToken
                    .transfer({ receiver: testEnv.bob.address, amount })
                    .exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.token.connect(testEnv.alice.provider!),
                    "Transfer"
                )
                .withArgs(testEnv.alice.address, testEnv.bob.address, amount);
        });

        it("Should be able to approve + upgrade", async () => {
            await approveAndDowngrade(testEnv.alice);
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                testEnv.token
                    .connect(testEnv.alice)
                    .approve(testEnv.wrapperSuperToken.address, amount)
            )
                .to.emit(
                    testEnv.token.connect(testEnv.alice.provider!),
                    "Approval"
                )
                .withArgs(
                    testEnv.alice.address,
                    testEnv.wrapperSuperToken.address,
                    amount
                );
            await expect(
                testEnv.wrapperSuperToken
                    .upgrade({ amount })
                    .exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(
                        testEnv.alice.provider!
                    ),
                    "TokenUpgraded"
                )
                .withArgs(testEnv.alice.address, amount);
        });

        it("Should be able to approve + upgrade to", async () => {
            await approveAndDowngrade(testEnv.bob);
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                testEnv.token
                    .connect(testEnv.bob)
                    .approve(testEnv.wrapperSuperToken.address, amount)
            )
                .to.emit(
                    testEnv.token.connect(testEnv.alice.provider!),
                    "Approval"
                )
                .withArgs(
                    testEnv.bob.address,
                    testEnv.wrapperSuperToken.address,
                    amount
                );

            await expect(
                testEnv.wrapperSuperToken
                    .upgradeTo({ amount, to: testEnv.alice.address })
                    .exec(testEnv.bob)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(
                        testEnv.alice.provider!
                    ),
                    "TokenUpgraded"
                )
                .withArgs(testEnv.alice.address, amount);
        });

        it("Should be able to approve + transfer", async () => {
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                testEnv.wrapperSuperToken
                    .approve({ receiver: testEnv.bob.address, amount })
                    .exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(
                        testEnv.alice.provider!
                    ),
                    "Approval"
                )
                .withArgs(testEnv.alice.address, testEnv.bob.address, amount);
            await expect(
                testEnv.wrapperSuperToken
                    .transfer({ receiver: testEnv.bob.address, amount })
                    .exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(
                        testEnv.alice.provider!
                    ),
                    "Transfer"
                )
                .withArgs(testEnv.alice.address, testEnv.bob.address, amount);
        });

        it("Should be able to approve + transferFrom", async () => {
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                testEnv.wrapperSuperToken
                    .approve({ receiver: testEnv.bob.address, amount })
                    .exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(
                        testEnv.alice.provider!
                    ),
                    "Approval"
                )
                .withArgs(testEnv.alice.address, testEnv.bob.address, amount);
            await expect(
                testEnv.wrapperSuperToken
                    .transferFrom({
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                        amount,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(
                        testEnv.alice.provider!
                    ),
                    "Transfer"
                )
                .withArgs(testEnv.alice.address, testEnv.bob.address, amount);
        });
    });

    describe("SuperToken Initialization Tests", () => {
        let nativeAssetSuperToken: NativeAssetSuperToken;
        it("Should load SuperToken base class for any testEnv.token type", async () => {
            // load testEnv.wrapperSuperToken
            await testEnv.sdkFramework.loadSuperToken(
                testEnv.wrapperSuperToken.address
            );

            // load NativeAssetSuperToken
            await testEnv.sdkFramework.loadSuperToken("ETHx");

            // load PureSuperToken
            await testEnv.sdkFramework.loadSuperToken("MRx");
        });

        it("Should be able to create a WrapperSuperToken", async () => {
            await testEnv.sdkFramework.loadWrapperSuperToken(
                testEnv.wrapperSuperToken.address
            );
        });

        it("Should throw if trying to load a non-WrapperSuperToken", async () => {
            try {
                await testEnv.sdkFramework.loadWrapperSuperToken("ETHx");
            } catch (err: any) {
                expect(err.message).to.eql(
                    "SuperToken Initialization Error: The token is not a wrapper supertoken."
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should be able to create a NativeAssetSuperToken", async () => {
            nativeAssetSuperToken =
                await testEnv.sdkFramework.loadNativeAssetSuperToken("ETHx");
        });

        it("Should throw if trying to load a non-NativeAssetSuperToken", async () => {
            try {
                await testEnv.sdkFramework.loadNativeAssetSuperToken(
                    testEnv.wrapperSuperToken.address
                );
            } catch (err: any) {
                expect(err.message).to.eql(
                    "SuperToken Initialization Error: The token is not a native asset supertoken."
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should be able to create a PureSuperToken", async () => {
            await testEnv.sdkFramework.loadPureSuperToken("MRx");
        });

        it("Should throw if trying to load a non-PureSuperToken", async () => {
            try {
                await testEnv.sdkFramework.loadPureSuperToken(
                    testEnv.wrapperSuperToken.address
                );
            } catch (err: any) {
                expect(err.message).to.eql(
                    "SuperToken Initialization Error: The token is not a pure supertoken."
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should be able to upgrade native asset", async () => {
            const upgradeOperation = nativeAssetSuperToken.upgrade({
                amount: ethers.utils.parseUnits("1").toString(),
            });
            await upgradeOperation.exec(testEnv.alice);
        });

        it("Should be able to upgrade native asset to", async () => {
            const upgradeOperation = nativeAssetSuperToken.upgradeTo({
                amount: ethers.utils.parseUnits("1").toString(),
                to: testEnv.bob.address,
            });
            await upgradeOperation.exec(testEnv.alice);
        });

        it("Should be able to downgrade native asset", async () => {
            await nativeAssetSuperToken
                .upgrade({
                    amount: ethers.utils.parseUnits("1").toString(),
                })
                .exec(testEnv.alice);

            const downgradeOperation = nativeAssetSuperToken.downgrade({
                amount: ethers.utils.parseUnits("1").toString(),
            });
            await downgradeOperation.exec(testEnv.alice);
        });
    });

    describe("Token-CFA Tests", () => {
        describe("Revert cases", () => {
            it("Should throw an error if one of the input addresses is invalid", async () => {
                const flowRate = getPerSecondFlowRateByMonth("100");
                try {
                    testEnv.wrapperSuperToken.createFlow({
                        flowRate,
                        receiver: testEnv.bob.address + "0",
                    });
                } catch (err: any) {
                    expect(err.message).to.eql(
                        "Invalid Address Error: The address you have entered is not a valid ethereum address"
                    );
                    expect(err.cause).to.be.undefined;
                }
            });

            it("Should throw an error on getFlow functions as expected", async () => {
                // NOTE: using casting to pass in wrong input to force error
                // get flow throw
                try {
                    await testEnv.wrapperSuperToken.getFlow({
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "ConstantFlowAgreementV1 Read Error: There was an error getting the flow"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }

                // get account flow info throw
                try {
                    await testEnv.wrapperSuperToken.getAccountFlowInfo({
                        account: testEnv.alice.address,
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "ConstantFlowAgreementV1 Read Error: There was an error getting the account flow information"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }

                // get net flow throw
                try {
                    await testEnv.wrapperSuperToken.getNetFlow({
                        account: testEnv.alice.address,
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "ConstantFlowAgreementV1 Read Error: There was an error getting net flow"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }
            });

            it("Should throw an error on getFlowOperatorData functions as expected", async () => {
                // NOTE: using casting to pass in wrong input to force error
                // get flowOperatorData throw
                try {
                    await testEnv.wrapperSuperToken.getFlowOperatorData({
                        sender: testEnv.alice.address,
                        flowOperator: testEnv.bob.address,
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "ConstantFlowAgreementV1 Read Error: There was an error getting flow operator data"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }
                try {
                    await testEnv.wrapperSuperToken.getFlowOperatorDataByID({
                        flowOperatorId: "",
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "ConstantFlowAgreementV1 Read Error: There was an error getting flow operator data"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }
            });
        });

        // CFA Functions
        it("Should have eip155 protection check", async () => {
            const flowRate = getPerSecondFlowRateByMonth("1000");
            const txnResponse = await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);
            expect(txnResponse.v).to.not.be.undefined;
        });

        it("Should be able to create flow", async () => {
            const flowRate = getPerSecondFlowRateByMonth("1000");
            await expect(
                testEnv.wrapperSuperToken
                    .createFlow({
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                        flowRate,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.cfaV1, "FlowUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    Number(flowRate),
                    Number(flowRate) * -1,
                    Number(flowRate),
                    "0x"
                );

            // get flow check
            const flow = await testEnv.wrapperSuperToken.getFlow({
                sender: testEnv.alice.address,
                receiver: testEnv.bob.address,
                providerOrSigner: testEnv.alice,
            });
            expect(flow.flowRate).to.equal(flowRate);

            // get account flow info check
            const deployerAccountFlowInfo =
                await testEnv.wrapperSuperToken.getAccountFlowInfo({
                    account: testEnv.alice.address,
                    providerOrSigner: testEnv.alice,
                });
            const alphaAccountFlowInfo =
                await testEnv.wrapperSuperToken.getAccountFlowInfo({
                    account: testEnv.bob.address,
                    providerOrSigner: testEnv.bob,
                });
            expect(Number(deployerAccountFlowInfo.flowRate)).to.equal(
                Number(flowRate) * -1
            );
            expect(Number(alphaAccountFlowInfo.flowRate)).to.equal(
                Number(flowRate)
            );

            // get net flow check
            const deployerNetFlow = await testEnv.wrapperSuperToken.getNetFlow({
                account: testEnv.alice.address,
                providerOrSigner: testEnv.alice,
            });
            const alphaNetFlow = await testEnv.wrapperSuperToken.getNetFlow({
                account: testEnv.bob.address,
                providerOrSigner: testEnv.bob,
            });
            expect(Number(deployerNetFlow)).to.equal(Number(flowRate) * -1);
            expect(Number(alphaNetFlow)).to.equal(Number(flowRate));
        });

        it("Should be able to update flow", async () => {
            let flowRate = getPerSecondFlowRateByMonth("1000");
            await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);
            flowRate = getPerSecondFlowRateByMonth("1200");
            await expect(
                testEnv.wrapperSuperToken
                    .updateFlow({
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                        flowRate,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.cfaV1, "FlowUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    Number(flowRate),
                    Number(flowRate) * -1,
                    Number(flowRate),
                    "0x"
                );
        });

        it("Should be able to delete flow (by sender)", async () => {
            let flowRate = getPerSecondFlowRateByMonth("1000");
            await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);

            await expect(
                testEnv.wrapperSuperToken
                    .deleteFlow({
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.cfaV1, "FlowUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    0,
                    0,
                    "0x"
                );
        });

        it("Should be able to delete flow (by receiver)", async () => {
            let flowRate = getPerSecondFlowRateByMonth("1000");
            await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);

            await expect(
                testEnv.wrapperSuperToken
                    .deleteFlow({
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.cfaV1, "FlowUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    0,
                    0,
                    "0x"
                );
        });

        it("Should not be able to delete flow (by wrong person)", async () => {
            let flowRate = getPerSecondFlowRateByMonth("1000");
            await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);
            try {
                await testEnv.wrapperSuperToken
                    .deleteFlow({
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                    })
                    .exec(testEnv.charlie);
            } catch (err: any) {
                expect(err.message).to.include("cannot estimate gas;");
            }
        });
    });

    describe("Token-CFA-Operator Tests", () => {
        let sender: SignerWithAddress;
        let flowOperator: SignerWithAddress;
        let receiver: SignerWithAddress;

        before(() => {
            sender = testEnv.bob;
            flowOperator = testEnv.charlie;
            receiver = testEnv.users[5];
        });

        it("Should throw when passing in unclean permissions", async () => {
            const flowRateAllowance = getPerSecondFlowRateByMonth("100");
            try {
                const permissions = AUTHORIZE_FULL_CONTROL + 1;
                const operation =
                    testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                        flowRateAllowance,
                        flowOperator: flowOperator.address,
                        permissions,
                    });
                await operation.exec(sender);
            } catch (err: any) {
                expect(err.message).to.eql(
                    "Unclean Permissions Error: The desired permissions are unclean"
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should throw when attempting to update flow operator permissions with negative flow rate", async () => {
            const flowRateAllowance = "-1000";
            try {
                const permissions = AUTHORIZE_FULL_CONTROL;
                const operation =
                    testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                        flowRateAllowance,
                        flowOperator: flowOperator.address,
                        permissions,
                    });
                await operation.exec(sender);
            } catch (err: any) {
                expect(err.message).to.eql(
                    "Negative Flow Rate Allowance Error: No negative flow allowance allowed"
                );
                expect(err.cause).to.be.undefined;
            }
        });

        // CFA Functions
        it("Should be able to update flow operator permissions", async () => {
            const flowRateAllowance = getPerSecondFlowRateByMonth("100");
            let permissions = AUTHORIZE_FLOW_OPERATOR_CREATE; // ALLOW_CREATE

            const flowOperatorId = getFlowOperatorId(
                sender.address,
                flowOperator.address
            );

            // Update Flow Operator Permissions
            const updateFlowOperatorPermissionsOperation =
                testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                    flowRateAllowance,
                    flowOperator: flowOperator.address,
                    permissions,
                });
            await expect(updateFlowOperatorPermissionsOperation.exec(sender))
                .to.emit(testEnv.cfaV1, "FlowOperatorUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    sender.address,
                    flowOperator.address,
                    permissions,
                    Number(flowRateAllowance)
                );

            // getFlowOperatorData test
            let flowOperatorData =
                await testEnv.wrapperSuperToken.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
            expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
            expect(flowOperatorData.flowRateAllowance).equals(
                flowRateAllowance
            );
            expect(flowOperatorData.permissions).equals(permissions.toString());

            // Revoke Flow Operator With Full Control Permissions
            permissions = 0; // no permissions
            const revokeFlowOperatorWithFullControlOperation =
                testEnv.wrapperSuperToken.revokeFlowOperatorWithFullControl({
                    flowOperator: flowOperator.address,
                });
            await expect(
                revokeFlowOperatorWithFullControlOperation.exec(sender)
            )
                .to.emit(testEnv.cfaV1, "FlowOperatorUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    sender.address,
                    flowOperator.address,
                    permissions,
                    0
                );
            // getFlowOperatorDataByID test
            flowOperatorData =
                await testEnv.wrapperSuperToken.getFlowOperatorDataByID({
                    flowOperatorId,
                    providerOrSigner: sender,
                });
            expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
            expect(flowOperatorData.flowRateAllowance).equals("0");
            expect(flowOperatorData.permissions).equals(permissions.toString());

            // Authorize Flow Operator With Full Control
            permissions = AUTHORIZE_FULL_CONTROL; // all permissions
            const authorizeFlowOperatorWithFullControlOperation =
                testEnv.wrapperSuperToken.authorizeFlowOperatorWithFullControl({
                    flowOperator: flowOperator.address,
                });
            await expect(
                authorizeFlowOperatorWithFullControlOperation.exec(sender)
            )
                .to.emit(testEnv.cfaV1, "FlowOperatorUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    sender.address,
                    flowOperator.address,
                    permissions,
                    testEnv.constants.MAX_FLOW_RATE.toString() // max flow rate ((2 ** 95) - 1)
                );

            // getFlowOperatorDataByID test
            flowOperatorData =
                await testEnv.wrapperSuperToken.getFlowOperatorDataByID({
                    flowOperatorId,
                    providerOrSigner: sender,
                });
            expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
            expect(flowOperatorData.flowRateAllowance).equals(
                testEnv.constants.MAX_FLOW_RATE.toString()
            );
            expect(flowOperatorData.permissions).equals(permissions.toString());
        });

        it("Should be able to create flow by operator", async () => {
            const flowRateAllowance = getPerSecondFlowRateByMonth("100");
            let permissions = AUTHORIZE_FLOW_OPERATOR_CREATE; // ALLOW_CREATE

            const updateFlowOperatorPermissionsOperation =
                testEnv.wrapperSuperToken.updateFlowOperatorPermissions({
                    flowRateAllowance,
                    flowOperator: flowOperator.address,
                    permissions,
                });

            await updateFlowOperatorPermissionsOperation.exec(sender);
            const flowRate = getPerSecondFlowRateByMonth("100");
            const operation = testEnv.wrapperSuperToken.createFlowByOperator({
                flowRate,
                sender: sender.address,
                receiver: receiver.address,
            });
            const deposit = clipDepositNumber(toBN(flowRate).mul(toBN(14400)));
            await expect(operation.exec(flowOperator))
                .to.emit(testEnv.cfaV1, "FlowUpdatedExtension")
                .withArgs(flowOperator.address, deposit.toString());
        });

        it("Should be able to update flow by operator", async () => {
            await createFlowWithOperator(flowOperator, sender, receiver);
            const flowRate = getPerSecondFlowRateByMonth("70");
            const operation = testEnv.wrapperSuperToken.updateFlowByOperator({
                flowRate,
                sender: sender.address,
                receiver: receiver.address,
            });
            const deposit = clipDepositNumber(toBN(flowRate).mul(toBN(14400)));
            await expect(operation.exec(flowOperator))
                .to.emit(testEnv.cfaV1, "FlowUpdatedExtension")
                .withArgs(flowOperator.address, deposit.toString());
        });

        it("Should be able to delete flow by operator", async () => {
            await createFlowWithOperator(flowOperator, sender, receiver);

            const operation = testEnv.wrapperSuperToken.deleteFlowByOperator({
                sender: sender.address,
                receiver: receiver.address,
            });
            await expect(operation.exec(flowOperator))
                .to.emit(testEnv.cfaV1, "FlowUpdatedExtension")
                .withArgs(flowOperator.address, "0");
        });
    });

    // Note: Alpha will create the Index which Deployer and Bravo
    describe("Token-IDA Tests", () => {
        describe("Revert cases", () => {
            it("Should throw an error if one of the input addresses is invalid", async () => {
                try {
                    testEnv.sdkFramework.idaV1.createIndex({
                        indexId: "0",
                        superToken: testEnv.wrapperSuperToken.address + "z",
                    });
                } catch (err: any) {
                    expect(err.message).to.eql(
                        "Invalid Address Error: The address you have entered is not a valid ethereum address"
                    );
                    expect(err.cause).to.be.undefined;
                }
            });

            it("Should throw an error on the reads as expected", async () => {
                // NOTE: using casting to pass in wrong input to force error
                try {
                    await testEnv.sdkFramework.idaV1.getIndex({
                        superToken: testEnv.wrapperSuperToken.address,
                        publisher: testEnv.alice.address,
                        indexId: "0",
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "InstantDistributionAgreementV1 Read Error: There was an error getting the index"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }

                try {
                    await testEnv.sdkFramework.idaV1.getSubscription({
                        superToken: testEnv.wrapperSuperToken.address,
                        publisher: testEnv.alice.address,
                        indexId: "0",
                        subscriber: testEnv.bob.address,
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "InstantDistributionAgreementV1 Read Error: There was an error getting the subscription"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }
            });
        });
        // IDA Functions
        it("Should be able to create an index and get the newly created index", async () => {
            await expect(
                testEnv.wrapperSuperToken
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexCreated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    "0x"
                );

            const index = await testEnv.wrapperSuperToken.getIndex({
                publisher: testEnv.bob.address,
                indexId: "0",
                providerOrSigner: testEnv.bob,
            });
            expect(index.exist).to.equal(true);
            expect(index.indexValue).to.equal("0");
            expect(index.totalUnitsApproved).to.equal("0");
            expect(index.totalUnitsPending).to.equal("0");
        });

        it("Should be able to update subscription units and get newly created subscriptions", async () => {
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            const units = ethers.utils.parseUnits("0.001").toString();
            await expect(
                testEnv.wrapperSuperToken
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: testEnv.alice.address,
                        units,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "SubscriptionUnitsUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    units,
                    "0x"
                );

            const deployerSubscription =
                await testEnv.wrapperSuperToken.getSubscription({
                    publisher: testEnv.bob.address,
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    providerOrSigner: testEnv.bob,
                });

            expect(deployerSubscription.exist).to.equal(true);
            expect(deployerSubscription.approved).to.equal(false);
            expect(deployerSubscription.units).to.equal(units);
            expect(deployerSubscription.pendingDistribution).to.equal("0");

            await expect(
                testEnv.wrapperSuperToken
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: testEnv.charlie.address,
                        units,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUnitsUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.charlie.address,
                    units,
                    "0x"
                );

            const bravoSubscription =
                await testEnv.wrapperSuperToken.getSubscription({
                    publisher: testEnv.bob.address,
                    indexId: "0",
                    subscriber: testEnv.charlie.address,
                    providerOrSigner: testEnv.charlie,
                });

            expect(bravoSubscription.exist).to.equal(true);
            expect(bravoSubscription.approved).to.equal(false);
            expect(bravoSubscription.units).to.equal(units);
            expect(bravoSubscription.pendingDistribution).to.equal("0");
        });

        it("Should be able to distribute (simple case)", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();

            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .distribute({
                        indexId: "0",
                        amount: ethers.utils.parseUnits("1").toString(),
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    "0",
                    "1000",
                    ethers.utils.parseUnits("0.001").toString(),
                    ethers.utils.parseUnits("0").toString(),
                    "0x"
                );
        });

        it("Should be able to distribute (multiple subs)", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();

            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.charlie.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .distribute({
                        indexId: "0",
                        amount: ethers.utils.parseUnits("1").toString(),
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    "0",
                    "500",
                    ethers.utils.parseUnits("0.002").toString(),
                    ethers.utils.parseUnits("0").toString(),
                    "0x"
                );
        });

        it("Should be able to approve subscription", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();

            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .approveSubscription({
                        indexId: "0",
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.idaV1, "SubscriptionApproved")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    "0x"
                );

            await expect(
                testEnv.wrapperSuperToken
                    .approveSubscription({
                        indexId: "0",
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.charlie)
            )
                .to.emit(testEnv.idaV1, "IndexSubscribed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.charlie.address,
                    "0x"
                );
        });

        it("Should be able to update index value", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();
            const updatedIndexValue = ethers.utils
                .parseUnits("0.000000002")
                .toString();
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .updateIndexValue({
                        indexId: "0",
                        indexValue: updatedIndexValue,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUpdated")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    "0",
                    updatedIndexValue,
                    units,
                    "0",
                    "0x"
                );
        });

        it("Should be able to revoke subscription", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.bob.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .approveSubscription({
                    indexId: "0",
                    publisher: testEnv.bob.address,
                })
                .exec(testEnv.alice);

            await testEnv.wrapperSuperToken
                .approveSubscription({
                    indexId: "0",
                    publisher: testEnv.bob.address,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .revokeSubscription({
                        indexId: "0",
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.idaV1, "SubscriptionRevoked")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    "0x"
                );

            await expect(
                testEnv.wrapperSuperToken
                    .revokeSubscription({
                        indexId: "0",
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUnsubscribed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.bob.address,
                    "0x"
                );
        });

        it("Should be able to claim pending units", async () => {
            const units = ethers.utils.parseUnits("0.002").toString();
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .distribute({
                    indexId: "0",
                    amount: units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .claim({
                        indexId: "0",
                        subscriber: testEnv.alice.address,
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(testEnv.idaV1, "SubscriptionDistributionClaimed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    units
                )
                .and.to.emit(testEnv.idaV1, "IndexDistributionClaimed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.alice.address,
                    units
                );
        });

        it("Should be able to delete subscription", async () => {
            const units = ethers.utils.parseUnits("0.001").toString();
            await testEnv.wrapperSuperToken
                .createIndex({
                    indexId: "0",
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.alice.address,
                    units,
                })
                .exec(testEnv.bob);

            await testEnv.wrapperSuperToken
                .updateSubscriptionUnits({
                    indexId: "0",
                    subscriber: testEnv.charlie.address,
                    units,
                })
                .exec(testEnv.bob);

            await expect(
                testEnv.wrapperSuperToken
                    .deleteSubscription({
                        indexId: "0",
                        subscriber: testEnv.alice.address,
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "SubscriptionRevoked")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.alice.address,
                    testEnv.bob.address,
                    0,
                    "0x"
                );

            await expect(
                testEnv.wrapperSuperToken
                    .deleteSubscription({
                        indexId: "0",
                        subscriber: testEnv.charlie.address,
                        publisher: testEnv.bob.address,
                    })
                    .exec(testEnv.bob)
            )
                .to.emit(testEnv.idaV1, "IndexUnsubscribed")
                .withArgs(
                    testEnv.wrapperSuperToken.address,
                    testEnv.bob.address,
                    0,
                    testEnv.charlie.address,
                    "0x"
                );
        });
    });
});
