import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import {
    IConstantFlowAgreementV1,
    IInstantDistributionAgreementV1,
    SuperToken as SuperTokenType,
    TestToken,
} from "../src/typechain";
import { SuperToken } from "../src";
import { getFlowOperatorId, getPerSecondFlowRateByMonth } from "../src";
import { setup } from "../scripts/setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { BigNumber, BigNumberish, ethers } from "ethers";
import { NativeAssetSuperToken, WrapperSuperToken } from "../src";
import { AUTHORIZE_FLOW_OPERATOR_CREATE, AUTHORIZE_FULL_CONTROL } from "../src";
import hre from "hardhat";

const INITIAL_AMOUNT_PER_USER = "10000000000";
const toBN = (x: BigNumberish) => ethers.BigNumber.from(x);
const MAX_FLOW_RATE = toBN(2).pow(toBN(95)).sub(toBN(1)).toString();

export const clipDepositNumber = (deposit: BigNumber, roundingDown = false) => {
    // last 32 bits of the deposit (96 bits) is clipped off
    const rounding = roundingDown
        ? 0
        : deposit.and(toBN(0xffffffff)).isZero()
        ? 0
        : 1;
    return deposit.shr(32).add(toBN(rounding)).shl(32);
};

let evmSnapshotId: string;
let framework: Framework;
let cfaV1: IConstantFlowAgreementV1;
let idaV1: IInstantDistributionAgreementV1;
let deployer: SignerWithAddress;
let alpha: SignerWithAddress;
let superToken: SuperTokenType;
let token: TestToken;
let daix: WrapperSuperToken;
let bravo: SignerWithAddress;
let charlie: SignerWithAddress;
let signerCount: number;

describe("SuperToken Tests", () => {
    before(async () => {
        const {
            frameworkClass,
            CFAV1,
            IDAV1,
            Deployer,
            Alpha,
            Bravo,
            Charlie,
            SuperToken,
            Token,
            SignerCount,
        } = await setup({
            amount: "10000000000",
            subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
        });
        framework = frameworkClass;
        cfaV1 = CFAV1;
        idaV1 = IDAV1;
        deployer = Deployer;
        alpha = Alpha;
        bravo = Bravo;
        superToken = SuperToken;
        daix = await framework.loadWrapperSuperToken(superToken.address);
        token = Token;
        charlie = Charlie;
        signerCount = SignerCount;

        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    beforeEach(async () => {
        await hre.network.provider.send("evm_revert", [evmSnapshotId]);
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    async function createFlowWithOperator(
        flowOperator: SignerWithAddress,
        sender: SignerWithAddress,
        receiver: SignerWithAddress
    ) {
        const flowRateAllowance = getPerSecondFlowRateByMonth("100");
        let permissions = AUTHORIZE_FULL_CONTROL;

        const updateFlowOperatorPermissionsOperation =
            daix.updateFlowOperatorPermissions({
                flowRateAllowance,
                flowOperator: flowOperator.address,
                permissions,
            });
        await updateFlowOperatorPermissionsOperation.exec(sender);

        let flowRate = getPerSecondFlowRateByMonth("100");

        await daix
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
            daix
                .approve({ receiver: daix.settings.address, amount })
                .exec(signer)
        )
            .to.emit(superToken, "Approval")
            .withArgs(signer.address, daix.settings.address, amount);
        await expect(daix.downgrade({ amount }).exec(signer))
            .to.emit(superToken, "TokenDowngraded")
            .withArgs(signer.address, amount);
    }

    describe("SuperToken Tests", () => {
        describe("SuperToken Tests", () => {
            it("Should throw an error if SuperToken isn't initialized properly.", async () => {
                try {
                    await SuperToken.create({
                        address: superToken.address,
                        provider: deployer.provider!,
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
                        address: superToken.address,
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
                    await daix.realtimeBalanceOf({
                        providerOrSigner: deployer,
                        account: alpha.address,
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
                // NOTE: provider is string as any to get this to throw an error on read
                try {
                    await daix.allowance({
                        owner: deployer.address,
                        spender: alpha.address,
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "SuperToken Read Error: There was an error getting allowance"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }

                try {
                    await daix.balanceOf({
                        account: deployer.address,
                        providerOrSigner: "" as any,
                    });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "SuperToken Read Error: There was an error getting balanceOf"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }

                try {
                    await daix.name({ providerOrSigner: "" as any });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "SuperToken Read Error: There was an error getting name"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }

                try {
                    await daix.symbol({ providerOrSigner: "" as any });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "SuperToken Read Error: There was an error getting symbol"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }

                try {
                    await daix.totalSupply({ providerOrSigner: "" as any });
                } catch (err: any) {
                    expect(err.message).to.contain(
                        "SuperToken Read Error: There was an error getting totalSupply"
                    );
                    expect(err.cause).to.be.instanceOf(Error);
                }
            });

            it("Should properly return totalSupply", async () => {
                const totalSupply = await daix.totalSupply({
                    providerOrSigner: deployer,
                });
                expect(totalSupply).to.equal(
                    ethers.utils
                        .parseUnits(
                            (
                                Number(INITIAL_AMOUNT_PER_USER) * signerCount
                            ).toString()
                        )
                        .toString()
                );
            });

            it("Should properly return balanceOf", async () => {
                const balance = await daix.balanceOf({
                    account: charlie.address,
                    providerOrSigner: deployer,
                });
                expect(balance).to.equal(
                    ethers.utils.parseUnits(INITIAL_AMOUNT_PER_USER).toString()
                );
            });

            it("Should properly return allowance", async () => {
                const initialAllowance = await daix.allowance({
                    owner: deployer.address,
                    spender: alpha.address,
                    providerOrSigner: deployer,
                });
                expect(initialAllowance).to.equal("0");
                const allowanceAmount = ethers.utils
                    .parseUnits("100")
                    .toString();
                const operation = daix.approve({
                    receiver: alpha.address,
                    amount: allowanceAmount,
                });
                await operation.exec(deployer);
                const approvedAllowance = await daix.allowance({
                    owner: deployer.address,
                    spender: alpha.address,
                    providerOrSigner: deployer,
                });
                expect(approvedAllowance).to.equal(allowanceAmount);
            });

            it("Should properly return name", async () => {
                const name = await daix.name({
                    providerOrSigner: deployer,
                });
                expect(name).to.equal("Super fDAI");
            });

            it("Should properly return symbol", async () => {
                const symbol = await daix.symbol({
                    providerOrSigner: deployer,
                });
                expect(symbol).to.equal("fDAIx");
            });

            it("Should properly initialize SuperToken", async () => {
                const daixTest = await framework.loadSuperToken(
                    superToken.address
                );
                expect(superToken.address).to.equal(daixTest.settings.address);
            });

            it("Should be able to initialize SuperToken with networkName.", () => {
                SuperToken.create({
                    address: superToken.address,
                    provider: deployer.provider!,
                    config: framework.settings.config,
                    networkName: "custom",
                });
            });

            it("Should be able to get realtimeBalanceOf", async () => {
                await daix.realtimeBalanceOf({
                    providerOrSigner: deployer,
                    account: deployer.address,
                });
            });

            it("Should be able to approve + downgrade", async () => {
                await approveAndDowngrade(deployer);
            });

            it("Should be able to transfer downgraded tokens", async () => {
                await approveAndDowngrade(deployer);
                const amount = ethers.utils.parseUnits("1000").toString();
                await expect(
                    daix.underlyingToken
                        .transfer({ receiver: alpha.address, amount })
                        .exec(deployer)
                )
                    .to.emit(token, "Transfer")
                    .withArgs(deployer.address, alpha.address, amount);
            });

            it("Should be able to approve + upgrade", async () => {
                await approveAndDowngrade(deployer);
                const amount = ethers.utils.parseUnits("1000").toString();
                await expect(
                    token
                        .connect(deployer)
                        .approve(daix.settings.address, amount)
                )
                    .to.emit(token, "Approval")
                    .withArgs(deployer.address, daix.settings.address, amount);
                await expect(daix.upgrade({ amount }).exec(deployer))
                    .to.emit(superToken, "TokenUpgraded")
                    .withArgs(deployer.address, amount);
            });

            it("Should be able to approve + upgrade to", async () => {
                await approveAndDowngrade(alpha);
                const amount = ethers.utils.parseUnits("1000").toString();
                await expect(
                    token.connect(alpha).approve(daix.settings.address, amount)
                )
                    .to.emit(token, "Approval")
                    .withArgs(alpha.address, daix.settings.address, amount);

                await expect(
                    daix.upgradeTo({ amount, to: deployer.address }).exec(alpha)
                )
                    .to.emit(superToken, "TokenUpgraded")
                    .withArgs(deployer.address, amount);
            });

            it("Should be able to approve + transfer", async () => {
                const amount = ethers.utils.parseUnits("1000").toString();
                await expect(
                    daix
                        .approve({ receiver: alpha.address, amount })
                        .exec(deployer)
                )
                    .to.emit(superToken, "Approval")
                    .withArgs(deployer.address, alpha.address, amount);
                await expect(
                    daix
                        .transfer({ receiver: alpha.address, amount })
                        .exec(deployer)
                )
                    .to.emit(superToken, "Transfer")
                    .withArgs(deployer.address, alpha.address, amount);
            });

            it("Should be able to approve + transferFrom", async () => {
                const amount = ethers.utils.parseUnits("1000").toString();
                await expect(
                    daix
                        .approve({ receiver: alpha.address, amount })
                        .exec(deployer)
                )
                    .to.emit(superToken, "Approval")
                    .withArgs(deployer.address, alpha.address, amount);
                await expect(
                    daix
                        .transferFrom({
                            sender: deployer.address,
                            receiver: alpha.address,
                            amount,
                        })
                        .exec(deployer)
                )
                    .to.emit(superToken, "Transfer")
                    .withArgs(deployer.address, alpha.address, amount);
            });
        });

        describe("SuperToken Initialization Tests", () => {
            let nativeAssetSuperToken: NativeAssetSuperToken;
            it("Should load SuperToken base class for any token type", async () => {
                // load WrapperSuperToken
                await framework.loadSuperToken(daix.address);

                // load NativeAssetSuperToken
                await framework.loadSuperToken("ETHx");

                // load PureSuperToken
                await framework.loadSuperToken("MRx");
            });

            it("Should be able to create a WrapperSuperToken", async () => {
                await framework.loadWrapperSuperToken(daix.address);
            });

            it("Should throw if trying to load a non-WrapperSuperToken", async () => {
                try {
                    await framework.loadWrapperSuperToken("ETHx");
                } catch (err: any) {
                    expect(err.message).to.eql(
                        "SuperToken Initialization Error: The token is not a wrapper supertoken."
                    );
                    expect(err.cause).to.be.undefined;
                }
            });

            it("Should be able to create a NativeAssetSuperToken", async () => {
                nativeAssetSuperToken =
                    await framework.loadNativeAssetSuperToken("ETHx");
            });

            it("Should throw if trying to load a non-NativeAssetSuperToken", async () => {
                try {
                    await framework.loadNativeAssetSuperToken(daix.address);
                } catch (err: any) {
                    expect(err.message).to.eql(
                        "SuperToken Initialization Error: The token is not a native asset supertoken."
                    );
                    expect(err.cause).to.be.undefined;
                }
            });

            it("Should be able to create a PureSuperToken", async () => {
                await framework.loadPureSuperToken("MRx");
            });

            it("Should throw if trying to load a non-PureSuperToken", async () => {
                try {
                    await framework.loadPureSuperToken(daix.address);
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
                await upgradeOperation.exec(deployer);
            });

            it("Should be able to upgrade native asset to", async () => {
                const upgradeOperation = nativeAssetSuperToken.upgradeTo({
                    amount: ethers.utils.parseUnits("1").toString(),
                    to: alpha.address,
                });
                await upgradeOperation.exec(deployer);
            });

            it("Should be able to downgrade native asset", async () => {
                await nativeAssetSuperToken
                    .upgrade({
                        amount: ethers.utils.parseUnits("1").toString(),
                    })
                    .exec(deployer);

                const downgradeOperation = nativeAssetSuperToken.downgrade({
                    amount: ethers.utils.parseUnits("1").toString(),
                });
                await downgradeOperation.exec(deployer);
            });
        });

        describe("Token-CFA Tests", () => {
            describe("Revert cases", () => {
                it("Should throw an error if one of the input addresses is invalid", async () => {
                    const flowRate = getPerSecondFlowRateByMonth("100");
                    try {
                        daix.createFlow({
                            flowRate,
                            receiver: alpha.address + "0",
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
                        await daix.getFlow({
                            sender: deployer.address,
                            receiver: alpha.address,
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
                        await daix.getAccountFlowInfo({
                            account: deployer.address,
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
                        await daix.getNetFlow({
                            account: deployer.address,
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
                        await daix.getFlowOperatorData({
                            sender: deployer.address,
                            flowOperator: alpha.address,
                            providerOrSigner: "" as any,
                        });
                    } catch (err: any) {
                        expect(err.message).to.contain(
                            "ConstantFlowAgreementV1 Read Error: There was an error getting flow operator data"
                        );
                        expect(err.cause).to.be.instanceOf(Error);
                    }
                    try {
                        await daix.getFlowOperatorDataByID({
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
                const txnResponse = await daix
                    .createFlow({
                        sender: deployer.address,
                        receiver: alpha.address,
                        flowRate,
                    })
                    .exec(deployer);
                expect(txnResponse.v).to.not.be.undefined;
            });

            it("Should be able to create flow", async () => {
                const flowRate = getPerSecondFlowRateByMonth("1000");
                await expect(
                    daix
                        .createFlow({
                            sender: deployer.address,
                            receiver: alpha.address,
                            flowRate,
                        })
                        .exec(deployer)
                )
                    .to.emit(cfaV1, "FlowUpdated")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        Number(flowRate),
                        Number(flowRate) * -1,
                        Number(flowRate),
                        "0x"
                    );

                // get flow check
                const flow = await daix.getFlow({
                    sender: deployer.address,
                    receiver: alpha.address,
                    providerOrSigner: deployer,
                });
                expect(flow.flowRate).to.equal(flowRate);

                // get account flow info check
                const deployerAccountFlowInfo = await daix.getAccountFlowInfo({
                    account: deployer.address,
                    providerOrSigner: deployer,
                });
                const alphaAccountFlowInfo = await daix.getAccountFlowInfo({
                    account: alpha.address,
                    providerOrSigner: alpha,
                });
                expect(Number(deployerAccountFlowInfo.flowRate)).to.equal(
                    Number(flowRate) * -1
                );
                expect(Number(alphaAccountFlowInfo.flowRate)).to.equal(
                    Number(flowRate)
                );

                // get net flow check
                const deployerNetFlow = await daix.getNetFlow({
                    account: deployer.address,
                    providerOrSigner: deployer,
                });
                const alphaNetFlow = await daix.getNetFlow({
                    account: alpha.address,
                    providerOrSigner: alpha,
                });
                expect(Number(deployerNetFlow)).to.equal(Number(flowRate) * -1);
                expect(Number(alphaNetFlow)).to.equal(Number(flowRate));
            });

            it("Should be able to update flow", async () => {
                let flowRate = getPerSecondFlowRateByMonth("1000");
                await daix
                    .createFlow({
                        sender: deployer.address,
                        receiver: alpha.address,
                        flowRate,
                    })
                    .exec(deployer);
                flowRate = getPerSecondFlowRateByMonth("1200");
                await expect(
                    daix
                        .updateFlow({
                            sender: deployer.address,
                            receiver: alpha.address,
                            flowRate,
                        })
                        .exec(deployer)
                )
                    .to.emit(cfaV1, "FlowUpdated")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        Number(flowRate),
                        Number(flowRate) * -1,
                        Number(flowRate),
                        "0x"
                    );
            });

            it("Should be able to delete flow (by sender)", async () => {
                let flowRate = getPerSecondFlowRateByMonth("1000");
                await daix
                    .createFlow({
                        sender: deployer.address,
                        receiver: alpha.address,
                        flowRate,
                    })
                    .exec(deployer);

                await expect(
                    daix
                        .deleteFlow({
                            sender: deployer.address,
                            receiver: alpha.address,
                        })
                        .exec(deployer)
                )
                    .to.emit(cfaV1, "FlowUpdated")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        0,
                        0,
                        0,
                        "0x"
                    );
            });

            it("Should be able to delete flow (by receiver)", async () => {
                let flowRate = getPerSecondFlowRateByMonth("1000");
                await daix
                    .createFlow({
                        sender: deployer.address,
                        receiver: alpha.address,
                        flowRate,
                    })
                    .exec(deployer);

                await expect(
                    daix
                        .deleteFlow({
                            sender: deployer.address,
                            receiver: alpha.address,
                        })
                        .exec(alpha)
                )
                    .to.emit(cfaV1, "FlowUpdated")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        0,
                        0,
                        0,
                        "0x"
                    );
            });

            it("Should not be able to delete flow (by wrong person)", async () => {
                let flowRate = getPerSecondFlowRateByMonth("1000");
                await daix
                    .createFlow({
                        sender: deployer.address,
                        receiver: alpha.address,
                        flowRate,
                    })
                    .exec(deployer);
                try {
                    await daix
                        .deleteFlow({
                            sender: deployer.address,
                            receiver: alpha.address,
                        })
                        .exec(bravo);
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
                sender = alpha;
                flowOperator = bravo;
                receiver = charlie;
            });

            it("Should throw when passing in unclean permissions", async () => {
                const flowRateAllowance = getPerSecondFlowRateByMonth("100");
                try {
                    const permissions = AUTHORIZE_FULL_CONTROL + 1;
                    const operation = daix.updateFlowOperatorPermissions({
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
                    const operation = daix.updateFlowOperatorPermissions({
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
                    daix.updateFlowOperatorPermissions({
                        flowRateAllowance,
                        flowOperator: flowOperator.address,
                        permissions,
                    });
                await expect(
                    updateFlowOperatorPermissionsOperation.exec(sender)
                )
                    .to.emit(cfaV1, "FlowOperatorUpdated")
                    .withArgs(
                        superToken.address,
                        sender.address,
                        flowOperator.address,
                        permissions,
                        Number(flowRateAllowance)
                    );

                // getFlowOperatorData test
                let flowOperatorData = await daix.getFlowOperatorData({
                    sender: sender.address,
                    flowOperator: flowOperator.address,
                    providerOrSigner: sender,
                });
                expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
                expect(flowOperatorData.flowRateAllowance).equals(
                    flowRateAllowance
                );
                expect(flowOperatorData.permissions).equals(
                    permissions.toString()
                );

                // Revoke Flow Operator With Full Control Permissions
                permissions = 0; // no permissions
                const revokeFlowOperatorWithFullControlOperation =
                    daix.revokeFlowOperatorWithFullControl({
                        flowOperator: flowOperator.address,
                    });
                await expect(
                    revokeFlowOperatorWithFullControlOperation.exec(sender)
                )
                    .to.emit(cfaV1, "FlowOperatorUpdated")
                    .withArgs(
                        superToken.address,
                        sender.address,
                        flowOperator.address,
                        permissions,
                        0
                    );
                // getFlowOperatorDataByID test
                flowOperatorData = await daix.getFlowOperatorDataByID({
                    flowOperatorId,
                    providerOrSigner: sender,
                });
                expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
                expect(flowOperatorData.flowRateAllowance).equals("0");
                expect(flowOperatorData.permissions).equals(
                    permissions.toString()
                );

                // Authorize Flow Operator With Full Control
                permissions = AUTHORIZE_FULL_CONTROL; // all permissions
                const authorizeFlowOperatorWithFullControlOperation =
                    daix.authorizeFlowOperatorWithFullControl({
                        flowOperator: flowOperator.address,
                    });
                await expect(
                    authorizeFlowOperatorWithFullControlOperation.exec(sender)
                )
                    .to.emit(cfaV1, "FlowOperatorUpdated")
                    .withArgs(
                        superToken.address,
                        sender.address,
                        flowOperator.address,
                        permissions,
                        MAX_FLOW_RATE // max flow rate ((2 ** 95) - 1)
                    );

                // getFlowOperatorDataByID test
                flowOperatorData = await daix.getFlowOperatorDataByID({
                    flowOperatorId,
                    providerOrSigner: sender,
                });
                expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
                expect(flowOperatorData.flowRateAllowance).equals(
                    MAX_FLOW_RATE
                );
                expect(flowOperatorData.permissions).equals(
                    permissions.toString()
                );
            });

            it("Should be able to create flow by operator", async () => {
                const flowRateAllowance = getPerSecondFlowRateByMonth("100");
                let permissions = AUTHORIZE_FLOW_OPERATOR_CREATE; // ALLOW_CREATE

                const updateFlowOperatorPermissionsOperation =
                    daix.updateFlowOperatorPermissions({
                        flowRateAllowance,
                        flowOperator: flowOperator.address,
                        permissions,
                    });

                await updateFlowOperatorPermissionsOperation.exec(sender);
                const flowRate = getPerSecondFlowRateByMonth("100");
                const operation = daix.createFlowByOperator({
                    flowRate,
                    sender: sender.address,
                    receiver: receiver.address,
                });
                const deposit = clipDepositNumber(
                    toBN(flowRate).mul(toBN(14400))
                );
                await expect(operation.exec(flowOperator))
                    .to.emit(cfaV1, "FlowUpdatedExtension")
                    .withArgs(flowOperator.address, deposit.toString());
            });

            it("Should be able to update flow by operator", async () => {
                await createFlowWithOperator(flowOperator, sender, receiver);
                const flowRate = getPerSecondFlowRateByMonth("70");
                const operation = daix.updateFlowByOperator({
                    flowRate,
                    sender: sender.address,
                    receiver: receiver.address,
                });
                const deposit = clipDepositNumber(
                    toBN(flowRate).mul(toBN(14400))
                );
                await expect(operation.exec(flowOperator))
                    .to.emit(cfaV1, "FlowUpdatedExtension")
                    .withArgs(flowOperator.address, deposit.toString());
            });

            it("Should be able to delete flow by operator", async () => {
                await createFlowWithOperator(flowOperator, sender, receiver);

                const operation = daix.deleteFlowByOperator({
                    sender: sender.address,
                    receiver: receiver.address,
                });
                await expect(operation.exec(flowOperator))
                    .to.emit(cfaV1, "FlowUpdatedExtension")
                    .withArgs(flowOperator.address, "0");
            });
        });

        // Note: Alpha will create the Index which Deployer and Bravo
        describe("Token-IDA Tests", () => {
            describe("Revert cases", () => {
                it("Should throw an error if one of the input addresses is invalid", async () => {
                    try {
                        framework.idaV1.createIndex({
                            indexId: "0",
                            superToken: superToken.address + "z",
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
                        await framework.idaV1.getIndex({
                            superToken: superToken.address,
                            publisher: deployer.address,
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
                        await framework.idaV1.getSubscription({
                            superToken: superToken.address,
                            publisher: deployer.address,
                            indexId: "0",
                            subscriber: alpha.address,
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
                    daix
                        .createIndex({
                            indexId: "0",
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "IndexCreated")
                    .withArgs(superToken.address, alpha.address, 0, "0x");

                const index = await daix.getIndex({
                    publisher: alpha.address,
                    indexId: "0",
                    providerOrSigner: alpha,
                });
                expect(index.exist).to.equal(true);
                expect(index.indexValue).to.equal("0");
                expect(index.totalUnitsApproved).to.equal("0");
                expect(index.totalUnitsPending).to.equal("0");
            });

            it("Should be able to update subscription units and get newly created subscriptions", async () => {
                await daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha);

                const units = ethers.utils.parseUnits("0.001").toString();
                await expect(
                    daix
                        .updateSubscriptionUnits({
                            indexId: "0",
                            subscriber: deployer.address,
                            units,
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "SubscriptionUnitsUpdated")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        0,
                        units,
                        "0x"
                    );

                const deployerSubscription = await daix.getSubscription({
                    publisher: alpha.address,
                    indexId: "0",
                    subscriber: deployer.address,
                    providerOrSigner: alpha,
                });

                expect(deployerSubscription.exist).to.equal(true);
                expect(deployerSubscription.approved).to.equal(false);
                expect(deployerSubscription.units).to.equal(units);
                expect(deployerSubscription.pendingDistribution).to.equal("0");

                await expect(
                    daix
                        .updateSubscriptionUnits({
                            indexId: "0",
                            subscriber: bravo.address,
                            units,
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "IndexUnitsUpdated")
                    .withArgs(
                        superToken.address,
                        alpha.address,
                        0,
                        bravo.address,
                        units,
                        "0x"
                    );

                const bravoSubscription = await daix.getSubscription({
                    publisher: alpha.address,
                    indexId: "0",
                    subscriber: bravo.address,
                    providerOrSigner: bravo,
                });

                expect(bravoSubscription.exist).to.equal(true);
                expect(bravoSubscription.approved).to.equal(false);
                expect(bravoSubscription.units).to.equal(units);
                expect(bravoSubscription.pendingDistribution).to.equal("0");
            });

            it("Should be able to distribute (simple case)", async () => {
                const units = ethers.utils.parseUnits("0.001").toString();

                await daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: deployer.address,
                        units,
                    })
                    .exec(alpha);

                await expect(
                    daix
                        .distribute({
                            indexId: "0",
                            amount: ethers.utils.parseUnits("1").toString(),
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "IndexUpdated")
                    .withArgs(
                        superToken.address,
                        alpha.address,
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

                await daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: deployer.address,
                        units,
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: bravo.address,
                        units,
                    })
                    .exec(alpha);

                await expect(
                    daix
                        .distribute({
                            indexId: "0",
                            amount: ethers.utils.parseUnits("1").toString(),
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "IndexUpdated")
                    .withArgs(
                        superToken.address,
                        alpha.address,
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

                await daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: deployer.address,
                        units,
                    })
                    .exec(alpha);

                await expect(
                    daix
                        .approveSubscription({
                            indexId: "0",
                            publisher: alpha.address,
                        })
                        .exec(deployer)
                )
                    .to.emit(idaV1, "SubscriptionApproved")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        0,
                        "0x"
                    );

                await expect(
                    daix
                        .approveSubscription({
                            indexId: "0",
                            publisher: alpha.address,
                        })
                        .exec(bravo)
                )
                    .to.emit(idaV1, "IndexSubscribed")
                    .withArgs(
                        superToken.address,
                        alpha.address,
                        0,
                        bravo.address,
                        "0x"
                    );
            });

            it("Should be able to update index value", async () => {
                const units = ethers.utils.parseUnits("0.001").toString();
                const updatedIndexValue = ethers.utils
                    .parseUnits("0.000000002")
                    .toString();
                await daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: deployer.address,
                        units,
                    })
                    .exec(alpha);

                await expect(
                    daix
                        .updateIndexValue({
                            indexId: "0",
                            indexValue: updatedIndexValue,
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "IndexUpdated")
                    .withArgs(
                        superToken.address,
                        alpha.address,
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
                await daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: deployer.address,
                        units,
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: alpha.address,
                        units,
                    })
                    .exec(alpha);

                await daix
                    .approveSubscription({
                        indexId: "0",
                        publisher: alpha.address,
                    })
                    .exec(deployer);

                await daix
                    .approveSubscription({
                        indexId: "0",
                        publisher: alpha.address,
                    })
                    .exec(alpha);

                await expect(
                    daix
                        .revokeSubscription({
                            indexId: "0",
                            publisher: alpha.address,
                        })
                        .exec(deployer)
                )
                    .to.emit(idaV1, "SubscriptionRevoked")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        0,
                        "0x"
                    );

                await expect(
                    daix
                        .revokeSubscription({
                            indexId: "0",
                            publisher: alpha.address,
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "IndexUnsubscribed")
                    .withArgs(
                        superToken.address,
                        alpha.address,
                        0,
                        alpha.address,
                        "0x"
                    );
            });

            it("Should be able to claim pending units", async () => {
                const units = ethers.utils.parseUnits("0.002").toString();
                await daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: deployer.address,
                        units,
                    })
                    .exec(alpha);

                await daix
                    .distribute({
                        indexId: "0",
                        amount: units,
                    })
                    .exec(alpha);

                await expect(
                    daix
                        .claim({
                            indexId: "0",
                            subscriber: deployer.address,
                            publisher: alpha.address,
                        })
                        .exec(deployer)
                )
                    .to.emit(idaV1, "SubscriptionDistributionClaimed")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        0,
                        units
                    )
                    .and.to.emit(idaV1, "IndexDistributionClaimed")
                    .withArgs(
                        superToken.address,
                        alpha.address,
                        0,
                        deployer.address,
                        units
                    );
            });

            it("Should be able to delete subscription", async () => {
                const units = ethers.utils.parseUnits("0.001").toString();
                await daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: deployer.address,
                        units,
                    })
                    .exec(alpha);

                await daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: bravo.address,
                        units,
                    })
                    .exec(alpha);

                await expect(
                    daix
                        .deleteSubscription({
                            indexId: "0",
                            subscriber: deployer.address,
                            publisher: alpha.address,
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "SubscriptionRevoked")
                    .withArgs(
                        superToken.address,
                        deployer.address,
                        alpha.address,
                        0,
                        "0x"
                    );

                await expect(
                    daix
                        .deleteSubscription({
                            indexId: "0",
                            subscriber: bravo.address,
                            publisher: alpha.address,
                        })
                        .exec(alpha)
                )
                    .to.emit(idaV1, "IndexUnsubscribed")
                    .withArgs(
                        superToken.address,
                        alpha.address,
                        0,
                        bravo.address,
                        "0x"
                    );
            });
        });
    });
});
