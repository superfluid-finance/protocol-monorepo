import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { IInstantDistributionAgreementV1, SuperToken } from "../src/typechain";
import { setup } from "./setup";
import { ethers } from "ethers";

describe("IDA V1 Tests", () => {
    let idaV1: IInstantDistributionAgreementV1;
    let framework: Framework;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperToken;
    let bravo: SignerWithAddress;

    before(async () => {
        const { IDAV1, frameworkClass, Deployer, Alpha, Bravo, SuperToken } =
            await setup({ dataMode: "WEB3_ONLY" });
        idaV1 = IDAV1;
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        bravo = Bravo;
        superToken = SuperToken;
    });

    it("Should throw an error if one of the input addresses is invalid.", async () => {
        try {
            framework.idaV1.createIndex({
                indexId: "0",
                superToken: superToken.address + "z",
            });
        } catch (err: any) {
            expect(err.message).to.eql(
                "Invalid Address Error - The address you have entered is not a valid ethereum address."
            );
        }
    });

    it("Should create an index properly.", async () => {
        await expect(
            framework.idaV1
                .createIndex({
                    indexId: "0",
                    superToken: superToken.address,
                })
                .exec(deployer)
        )
            .to.emit(idaV1, "IndexCreated")
            .withArgs(superToken.address, deployer.address, 0, "0x");
    });

    it("Should be able to update subscription units.", async () => {
        await expect(
            framework.idaV1
                .updateSubscriptionUnits({
                    indexId: "0",
                    superToken: superToken.address,
                    subscriber: alpha.address,
                    units: ethers.utils.parseUnits("0.001").toString(),
                })
                .exec(deployer)
        )
            .to.emit(idaV1, "SubscriptionUnitsUpdated")
            .withArgs(
                superToken.address,
                alpha.address,
                deployer.address,
                0,
                ethers.utils.parseUnits("0.001").toString(),
                "0x"
            );

        await expect(
            framework.idaV1
                .updateSubscriptionUnits({
                    indexId: "0",
                    superToken: superToken.address,
                    subscriber: bravo.address,
                    units: ethers.utils.parseUnits("0.001").toString(),
                })
                .exec(deployer)
        )
            .to.emit(idaV1, "IndexUnitsUpdated")
            .withArgs(
                superToken.address,
                deployer.address,
                0,
                bravo.address,
                ethers.utils.parseUnits("0.001").toString(),
                "0x"
            );
    });

    it("Should be able to distribute to subscriptions", async () => {
        await expect(
            framework.idaV1
                .distribute({
                    indexId: "0",
                    superToken: superToken.address,
                    amount: ethers.utils.parseUnits("1").toString(),
                })
                .exec(deployer)
        )
            .to.emit(idaV1, "IndexUpdated")
            .withArgs(
                superToken.address,
                deployer.address,
                0,
                "0",
                "500",
                ethers.utils.parseUnits("0.002").toString(),
                ethers.utils.parseUnits("0").toString(),
                "0x"
            );
    });

    it("Should be able to approve subscriptions", async () => {
        await expect(
            framework.idaV1
                .approveSubscription({
                    indexId: "0",
                    superToken: superToken.address,
                    publisher: deployer.address,
                })
                .exec(alpha)
        )
            .to.emit(idaV1.connect(alpha), "SubscriptionApproved")
            .withArgs(
                superToken.address,
                alpha.address,
                deployer.address,
                0,
                "0x"
            );

        await expect(
            framework.idaV1
                .approveSubscription({
                    indexId: "0",
                    superToken: superToken.address,
                    publisher: deployer.address,
                })
                .exec(bravo)
        )
            .to.emit(idaV1.connect(bravo), "IndexSubscribed")
            .withArgs(
                superToken.address,
                deployer.address,
                0,
                bravo.address,
                "0x"
            );
    });

    it("Should be able to update index value", async () => {
        await expect(
            framework.idaV1
                .updateIndexValue({
                    indexId: "0",
                    indexValue: ethers.utils.parseUnits("0.002").toString(),
                    superToken: superToken.address,
                })
                .exec(deployer)
        )
            .to.emit(idaV1, "IndexUpdated")
            .withArgs(
                superToken.address,
                deployer.address,
                0,
                "500",
                ethers.utils.parseUnits("0.002").toString(),
                "0",
                ethers.utils.parseUnits("0.002").toString(),
                "0x"
            );
    });

    it("Should be able to revoke subscriptions", async () => {
        await expect(
            framework.idaV1
                .revokeSubscription({
                    indexId: "0",
                    superToken: superToken.address,
                    publisher: deployer.address,
                })
                .exec(alpha)
        )
            .to.emit(idaV1.connect(alpha), "SubscriptionRevoked")
            .withArgs(
                superToken.address,
                alpha.address,
                deployer.address,
                0,
                "0x"
            );

        await expect(
            framework.idaV1
                .revokeSubscription({
                    indexId: "0",
                    superToken: superToken.address,
                    publisher: deployer.address,
                })
                .exec(bravo)
        )
            .to.emit(idaV1.connect(bravo), "IndexUnsubscribed")
            .withArgs(
                superToken.address,
                deployer.address,
                0,
                bravo.address,
                "0x"
            );
    });

    it("Should be able to update index value", async () => {
        await expect(
            framework.idaV1
                .updateIndexValue({
                    indexId: "0",
                    indexValue: ethers.utils.parseUnits("0.003").toString(),
                    superToken: superToken.address,
                })
                .exec(deployer)
        )
            .to.emit(idaV1, "IndexUpdated")
            .withArgs(
                superToken.address,
                deployer.address,
                0,
                ethers.utils.parseUnits("0.002").toString(),
                ethers.utils.parseUnits("0.003").toString(),
                ethers.utils.parseUnits("0.002").toString(),
                "0",
                "0x"
            );
    });

    it("Should be able to claim", async () => {
        await expect(
            framework.idaV1
                .claim({
                    indexId: "0",
                    superToken: superToken.address,
                    subscriber: alpha.address,
                    publisher: deployer.address,
                })
                .exec(alpha)
        )
            .to.emit(idaV1.connect(alpha), "SubscriptionDistributionClaimed")
            .withArgs(
                superToken.address,
                alpha.address,
                deployer.address,
                0,
                ethers.utils.parseUnits("1000000000000")
            );

        await expect(
            framework.idaV1
                .claim({
                    indexId: "0",
                    superToken: superToken.address,
                    subscriber: bravo.address,
                    publisher: deployer.address,
                })
                .exec(bravo)
        )
            .to.emit(idaV1.connect(bravo), "IndexDistributionClaimed")
            .withArgs(
                superToken.address,
                deployer.address,
                0,
                bravo.address,
                ethers.utils.parseUnits("1000000000000")
            );
    });

    it("Should be able to delete subscription", async () => {
        await expect(
            framework.idaV1
                .deleteSubscription({
                    indexId: "0",
                    subscriber: alpha.address,
                    superToken: superToken.address,
                    publisher: deployer.address,
                })
                .exec(deployer)
        )
            .to.emit(idaV1, "SubscriptionRevoked")
            .withArgs(
                superToken.address,
                alpha.address,
                deployer.address,
                0,
                "0x"
            );

        await expect(
            framework.idaV1
                .deleteSubscription({
                    indexId: "0",
                    subscriber: bravo.address,
                    superToken: superToken.address,
                    publisher: deployer.address,
                })
                .exec(deployer)
        )
            .to.emit(idaV1, "IndexUnsubscribed")
            .withArgs(
                superToken.address,
                deployer.address,
                0,
                bravo.address,
                "0x"
            );
    });
});
