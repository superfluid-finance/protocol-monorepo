import hre from "hardhat";
import { expect } from "chai";
import { NativeAssetSuperToken, SuperToken, toBN } from "../src";
import { ethers } from "ethers";
import { TestEnvironment, makeSuite } from "./TestEnvironment";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";

makeSuite("SuperToken Tests", (testEnv: TestEnvironment) => {
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
                        cfaV1ForwarderAddress: "",
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
                        cfaV1ForwarderAddress: "",
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

        it("Should support SuperToken without getUnderlyingToken() function implemented",async () => {
            const noGetUnderlyingTokenFactory = await hre.ethers.getContractFactory("NoGetUnderlyingToken");
            const noGetUnderlyingToken = await noGetUnderlyingTokenFactory.deploy();
            await SuperToken.create({
                address: noGetUnderlyingToken.address,
                provider: testEnv.alice.provider!,
                config: testEnv.sdkFramework.settings.config,
                networkName: "custom",
            });
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

        it("Should be able to downgrade", async () => {
            await shouldDowngrade(testEnv, testEnv.alice);
        });

        it("Should be able to use downgradeTo", async () => {
            const amount = ethers.utils.parseUnits("2000").toString();
            const aliceBalanceBefore =
                await testEnv.wrapperSuperToken.balanceOf({
                    account: testEnv.alice.address,
                    providerOrSigner: testEnv.alice,
                });
            const bobBalanceBefore = await testEnv.wrapperSuperToken.underlyingToken.balanceOf({
                account: testEnv.bob.address,
                providerOrSigner: testEnv.bob,
            });
            await expect(
                testEnv.wrapperSuperToken
                    .downgradeTo({ to: testEnv.bob.address, amount })
                    .exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(
                        testEnv.alice.provider!
                    ),
                    "TokenDowngraded"
                )
                .withArgs(testEnv.alice.address, amount);

            const aliceBalanceAfter = await testEnv.wrapperSuperToken.balanceOf(
                {
                    account: testEnv.alice.address,
                    providerOrSigner: testEnv.alice,
                }
            );
            const bobBalanceAfter = await testEnv.wrapperSuperToken.underlyingToken.balanceOf({
                account: testEnv.bob.address,
                providerOrSigner: testEnv.bob,
            });

            // alice new super token balance = prev balance - amount
            expect(toBN(aliceBalanceBefore).sub(toBN(amount))).to.equal(aliceBalanceAfter.toString());
            
            // bob new erc20 token balance = prev balance + amount
            expect(toBN(bobBalanceBefore).add(toBN(amount))).to.equal(bobBalanceAfter.toString());
        });
        

        it("Should be able to transfer downgraded tokens", async () => {
            await shouldDowngrade(testEnv, testEnv.alice);
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

        it("Should be able to increaseAllowance", async () => {
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                testEnv.wrapperSuperToken.increaseAllowance({
                    spender: testEnv.bob.address,
                    amount,
                }).exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(
                        testEnv.alice
                    ),
                    "Approval"
                )
                .withArgs(
                    testEnv.alice.address,
                    testEnv.bob.address,
                    amount
                );
        });

        it("Should be able to decreaseAllowance", async () => {
            const amount = ethers.utils.parseUnits("1000").toString();
            await testEnv.wrapperSuperToken
                .increaseAllowance({
                    spender: testEnv.bob.address,
                    amount,
                })
                .exec(testEnv.alice);
            await expect(
                testEnv.wrapperSuperToken
                    .decreaseAllowance({
                        spender: testEnv.bob.address,
                        amount,
                    })
                    .exec(testEnv.alice)
            )
                .to.emit(
                    testEnv.wrapperSuperToken.contract.connect(testEnv.alice),
                    "Approval"
                )
                .withArgs(testEnv.alice.address, testEnv.bob.address, "0");
        });

        it("Should be able to approve + upgrade", async () => {
            await shouldDowngrade(testEnv, testEnv.alice);
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
            await shouldDowngrade(testEnv, testEnv.bob);
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
});

// Helper Functions

/**
 * Creates a ModifyFlowByOperator TxnResponse promise and checks if we are using host or forwarder
 * @param params
 * @returns
 */
async function shouldDowngrade(
    testEnv: TestEnvironment,
    signer: SignerWithAddress,
    amount: string = "2000"
) {
    amount = ethers.utils.parseUnits(amount).toString();
    await expect(testEnv.wrapperSuperToken.downgrade({ amount }).exec(signer))
        .to.emit(
            testEnv.wrapperSuperToken.contract.connect(testEnv.alice.provider!),
            "TokenDowngraded"
        )
        .withArgs(signer.address, amount);
}
