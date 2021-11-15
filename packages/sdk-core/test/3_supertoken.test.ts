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
import { setup } from "./setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { ethers } from "ethers";

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

    before(async () => {
        const {
            frameworkClass,
            CFAV1,
            IDAV1,
            Deployer,
            Alpha,
            Bravo,
            SuperToken,
            Token,
        } = await setup({
            subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
            amount: "1000000",
        });
        framework = frameworkClass;
        cfaV1 = CFAV1;
        idaV1 = IDAV1;
        deployer = Deployer;
        alpha = Alpha;
        bravo = Bravo;
        superToken = SuperToken;
        daix = framework.loadSuperToken(superToken.address);
        token = Token;
    });

    describe("Pure Token Tests", () => {
        it("Should throw an error if SuperToken isn't initialized properly.", () => {
            try {
                new SuperToken({
                    address: superToken.address,
                    config: {
                        hostAddress: "",
                        cfaV1Address: "",
                        idaV1Address: "",
                        superTokenFactoryAddress: "",
                    },
                });
            } catch (err: any) {
                console.log(err.message);
                expect(err.message).to.equal(
                    "SuperToken Initialization Error - You must input chainId or networkName."
                );
            }
        });

        it("Should throw an error on SuperToken read operations when incorrect input is passed.", async () => {
            try {
                await daix.realtimeBalanceOf({
                    providerOrSigner: deployer,
                    address: alpha.address,
                    timestamp: "-1",
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "SuperToken Read Error - There was an error getting realtimeBalanceOf"
                );
            }
        });

        it("Should properly initialize SuperToken", () => {
            const daixTest = framework.loadSuperToken(superToken.address);
            expect(superToken.address).to.equal(daixTest.options.address);
        });

        it("Should be able to initialize SuperToken with networkName.", () => {
            new SuperToken({
                address: superToken.address,
                config: framework.settings.config,
                networkName: "custom",
            });
        });

        it("Should be able to get realtimeBalanceOf", async () => {
            await daix.realtimeBalanceOf({
                providerOrSigner: deployer,
                address: deployer.address,
            });
        });

        it("Should be able to approve + downgrade", async () => {
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                daix
                    .approve({ receiver: daix.options.address, amount })
                    .exec(deployer)
            )
                .to.emit(superToken, "Approval")
                .withArgs(deployer.address, daix.options.address, amount);
            await expect(daix.downgrade({ amount }).exec(deployer))
                .to.emit(superToken, "TokenDowngraded")
                .withArgs(deployer.address, amount);
        });

        it("Should be able to approve + upgrade", async () => {
            const amount = ethers.utils.parseUnits("1000").toString();
            await expect(
                token.connect(deployer).approve(daix.options.address, amount)
            )
                .to.emit(token, "Approval")
                .withArgs(deployer.address, daix.options.address, amount);
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
        it("Should be able to create an index", async () => {
            await expect(
                daix
                    .createIndex({
                        indexId: "0",
                    })
                    .exec(alpha)
            )
                .to.emit(idaV1, "IndexCreated")
                .withArgs(superToken.address, alpha.address, 0, "0x");
        });

        it("Should be able to update subscription units", async () => {
            await expect(
                daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: deployer.address,
                        units: ethers.utils.parseUnits("0.001").toString(),
                    })
                    .exec(alpha)
            )
                .to.emit(idaV1, "SubscriptionUnitsUpdated")
                .withArgs(
                    superToken.address,
                    deployer.address,
                    alpha.address,
                    0,
                    ethers.utils.parseUnits("0.001").toString(),
                    "0x"
                );

            await expect(
                daix
                    .updateSubscriptionUnits({
                        indexId: "0",
                        subscriber: bravo.address,
                        units: ethers.utils.parseUnits("0.001").toString(),
                    })
                    .exec(alpha)
            )
                .to.emit(idaV1, "IndexUnitsUpdated")
                .withArgs(
                    superToken.address,
                    alpha.address,
                    0,
                    bravo.address,
                    ethers.utils.parseUnits("0.001").toString(),
                    "0x"
                );
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
                        indexValue: ethers.utils.parseUnits("0.002").toString(),
                    })
                    .exec(alpha)
            )
                .to.emit(idaV1, "IndexUpdated")
                .withArgs(
                    superToken.address,
                    alpha.address,
                    0,
                    "500",
                    ethers.utils.parseUnits("0.002").toString(),
                    "0",
                    ethers.utils.parseUnits("0.002").toString(),
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
                        indexValue: ethers.utils.parseUnits("0.003").toString(),
                    })
                    .exec(alpha)
            )
                .to.emit(idaV1, "IndexUpdated")
                .withArgs(
                    superToken.address,
                    alpha.address,
                    0,
                    ethers.utils.parseUnits("0.002").toString(),
                    ethers.utils.parseUnits("0.003").toString(),
                    ethers.utils.parseUnits("0.002").toString(),
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
                    ethers.utils.parseUnits("1000000000000")
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
                    ethers.utils.parseUnits("1000000000000")
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
