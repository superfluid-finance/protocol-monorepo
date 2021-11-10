import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { SuperToken as SuperTokenType } from "../src/typechain";
import { SuperToken } from "../src";
import { setup } from "./setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";

describe.only("SuperToken Tests", () => {
    let framework: Framework;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperTokenType;
    // let bravo: SignerWithAddress;

    before(async () => {
        const { frameworkClass, Deployer, Alpha, SuperToken } = await setup({
            subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
        });
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        superToken = SuperToken;
        console.log(framework.settings.config.cfaV1Address);
        console.log(deployer.address);
        console.log(alpha.address);
    });

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

    it("Should properly initialize SuperToken", () => {
        const daix = framework.loadSuperToken(superToken.address);
        expect(superToken.address).to.equal(daix.options.address);
    });

    it("Should be able to get balanceOf, realtimeBalanceOf, realtimeBalanceOfNow", async () => {
        const daix = framework.loadSuperToken(superToken.address);
        const balance = await daix.balanceOf(deployer.address, deployer);
        const { availableBalance, timestamp } = await daix.realtimeBalanceOfNow(
            deployer.address,
            deployer
        );
        const realtimeBalanceOf = await daix.realtimeBalanceOf(
            deployer.address,
            timestamp.toString(),
            deployer
        );
        expect(balance.toString()).to.equal(realtimeBalanceOf.toString());
        expect(realtimeBalanceOf.toString()).to.equal(
            availableBalance.toString()
        );
    });

    it("Should be able to approve + downgrade", async () => {});

    it("Should be able to approve + upgrade", async () => {});

    it("Should be able to approve + transfer", async () => {});

    it("Should be able to approve + transferFrom", async () => {});

    // CFA Functions
    it("Should be able to create flow", async () => {});

    it("Should be able to update flow", async () => {});

    it("Should be able to delete flow", async () => {});

    // IDA Functions
    it("Should be able to create an index", async () => {});

    it("Should be able to update subscription units", async () => {});

    it("Should be able to update index value", async () => {});

    it("Should be able to approve subscription", async () => {});

    it("Should be able to revoke subscription", async () => {});

    it("Should be able to distribute", async () => {});

    it("Should be able to claim pending units", async () => {});

    it("Should be able to delete subscription", async () => {});
});
