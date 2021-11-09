import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { SuperToken as SuperTokenType } from "../src/typechain";
import { SuperToken } from "../src";
import { setup } from "./setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";

describe("Operation Tests", () => {
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

    it("Should throw an error when trying to execute a transaction with faulty callData", () => {});

    it("Should be able to get signed transaction", () => {});

    it("Should be able to get transaction hash and it should be equal to transaction hash once executed.", () => {});
});
