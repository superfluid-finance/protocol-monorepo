import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { IInstantDistributionAgreementV1, SuperToken } from "../src/typechain";
import { setup } from "./setup";

describe("CFA V1 Tests", () => {
    let idaV1: IInstantDistributionAgreementV1;
    let framework: Framework;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperToken;
    // let bravo: SignerWithAddress;

    before(async () => {
        const { IDAV1, frameworkClass, Deployer, Alpha, SuperToken } =
            await setup();
        idaV1 = IDAV1;
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        superToken = SuperToken;
    });

    it("Should throw an error if one of the input addresses is invalid.", async () => {});

    it("Should create an index properly.", async () => {});

    it("Should be able to update subscription units.", async () => {});

    it("Should be able to distribute to subscriptions", async () => {});

    it("Should be able to approve subscription", async () => {});

    it("Should be able to update index value for approved subscription", async () => {});

    it("Should be able to revoke subscription", async () => {});

    it("Should be able to update index value", async () => {});

    it("Should be able to claim", async () => {});

    it("Should be able to delete subscription", async () => {});
});
