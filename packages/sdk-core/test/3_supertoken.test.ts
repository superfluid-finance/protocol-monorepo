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
import { getPerSecondFlowRateByMonth } from "../src/utils";
import { setup } from "../scripts/setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { ethers } from "ethers";

const INITIAL_AMOUNT_PER_USER = "10000000000";

describe("SuperToken Tests", () => {
    let framework: Framework;
    let cfaV1: IConstantFlowAgreementV1;
    let idaV1: IInstantDistributionAgreementV1;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperTokenType;
    let token: TestToken;
    let daix: SuperToken;
    let bravo: SignerWithAddress;
    let charlie: SignerWithAddress;
    let signerCount: number;

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
            subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
        });
        framework = frameworkClass;
        cfaV1 = CFAV1;
        idaV1 = IDAV1;
        deployer = Deployer;
        alpha = Alpha;
        bravo = Bravo;
        superToken = SuperToken;
        daix = await framework.loadSuperToken(superToken.address);
        token = Token;
        charlie = Charlie;
        signerCount = SignerCount;
    });

    describe("Pure Token Tests", () => {
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
                    },
                });
            } catch (err: any) {
                expect(err.message).to.equal(
                    "SuperToken Initialization Error - You must input chainId or networkName."
                );
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
                    },
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Initialization Error - There was an error initializing the SuperToken"
                );
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
                    "SuperToken Read Error - There was an error getting realtimeBalanceOf"
                );
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
                    "SuperToken Read Error - There was an error getting allowance"
                );
            }

            try {
                await daix.balanceOf({
                    account: deployer.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error - There was an error getting balanceOf"
                );
            }

            try {
                await daix.name({ providerOrSigner: "" as any });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error - There was an error getting name"
                );
            }

            try {
                await daix.symbol({ providerOrSigner: "" as any });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error - There was an error getting symbol"
                );
            }

            try {
                await daix.totalSupply({ providerOrSigner: "" as any });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error - There was an error getting totalSupply"
                );
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
            const allowanceAmount = ethers.utils.parseUnits("100").toString();
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
            expect(name).to.equal("Super fDAI Fake Token");
        });

        it("Should properly return symbol", async () => {
            const symbol = await daix.symbol({
                providerOrSigner: deployer,
            });
            expect(symbol).to.equal("fDAIx");
        });

        it("Should properly initialize SuperToken", async () => {
            const daixTest = await framework.loadSuperToken(superToken.address);
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
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                daix
                    .approve({ receiver: daix.settings.address, amount })
                    .exec(deployer)
            )
                .to.emit(superToken, "Approval")
                .withArgs(deployer.address, daix.settings.address, amount);
            await expect(daix.downgrade({ amount }).exec(deployer))
                .to.emit(superToken, "TokenDowngraded")
                .withArgs(deployer.address, amount);
        });

        it("Should be able to approve + upgrade", async () => {
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                token.connect(deployer).approve(daix.settings.address, amount)
            )
                .to.emit(token, "Approval")
                .withArgs(deployer.address, daix.settings.address, amount);
            await expect(daix.upgrade({ amount }).exec(deployer))
                .to.emit(superToken, "TokenUpgraded")
                .withArgs(deployer.address, amount);
        });

        it("Should be able to approve + transfer", async () => {
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                daix.approve({ receiver: alpha.address, amount }).exec(deployer)
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
                daix.approve({ receiver: alpha.address, amount }).exec(deployer)
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

    describe("Token-CFA Tests", () => {
        // CFA Functions
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
            const flowRate = getPerSecondFlowRateByMonth("1200");
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

        it("Should be able to delete flow", async () => {
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
    });

    // Note: Alpha will create the Index which Deployer and Bravo
    describe("Token-IDA Tests", () => {
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

        it("Should be able to distribute", async () => {
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
            await expect(
                daix
                    .updateIndexValue({
                        indexId: "0",
                        indexValue: ethers.utils
                            .parseUnits("0.000000002")
                            .toString(),
                    })
                    .exec(alpha)
            )
                .to.emit(idaV1, "IndexUpdated")
                .withArgs(
                    superToken.address,
                    alpha.address,
                    0,
                    "500",
                    ethers.utils.parseUnits("0.000000002").toString(),
                    "0",
                    "2000000000000000",
                    "0x"
                );
        });

        it("Should be able to revoke subscription", async () => {
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
                    .exec(bravo)
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

        it("Should be able to update index value", async () => {
            await expect(
                daix
                    .updateIndexValue({
                        indexId: "0",
                        indexValue: ethers.utils
                            .parseUnits("0.000000003")
                            .toString(),
                    })
                    .exec(alpha)
            )
                .to.emit(idaV1, "IndexUpdated")
                .withArgs(
                    superToken.address,
                    alpha.address,
                    0,
                    ethers.utils.parseUnits("0.000000002").toString(),
                    ethers.utils.parseUnits("0.000000003").toString(),
                    "2000000000000000",
                    "0",
                    "0x"
                );
        });

        it("Should be able to claim pending units", async () => {
            ("");
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
                    "1000000000000000000000000"
                );

            await expect(
                daix
                    .claim({
                        indexId: "0",
                        subscriber: bravo.address,
                        publisher: alpha.address,
                    })
                    .exec(bravo)
            )
                .to.emit(idaV1, "IndexDistributionClaimed")
                .withArgs(
                    superToken.address,
                    alpha.address,
                    0,
                    bravo.address,
                    "1000000000000000000000000"
                );
        });

        it("Should be able to delete subscription", async () => {
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
