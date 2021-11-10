import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { SuperToken as SuperTokenType } from "../src/typechain";
import { setup } from "./setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { ethers } from "ethers";

describe("Batch Call Tests", () => {
    let framework: Framework;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperTokenType;

    before(async () => {
        const { frameworkClass, Deployer, Alpha, SuperToken } = await setup({
            subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
        });
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        superToken = SuperToken;
    });

    it("Should throw an error on unsupported operations", async () => {
        const daix = framework.loadSuperToken(superToken.address);
        const transferOp = daix.transfer(
            alpha.address,
            ethers.utils.parseUnits("1000").toString()
        );
        const batchCall = framework.batchCall([transferOp]);

        try {
            const promises = batchCall.getOperationStructArrayPromises;
            await Promise.all(promises);
        } catch (err: any) {
            expect(err.message).to.contain(
                "Unsupported Batch Call Operation Error - The operation at index 0 is unsupported"
            );
        }
    });

    it("Should be able to create and execute a batch call", async () => {
        const amount = ethers.utils.parseUnits("1000").toString();
        const daix = framework.loadSuperToken(superToken.address);
        const approveOp = daix.approve(alpha.address, amount);
        const transferFromOp = daix.transferFrom(
            deployer.address,
            alpha.address,
            amount
        );
        const batchCall = framework.batchCall([approveOp, transferFromOp]);
        await expect(batchCall.exec(deployer))
            .to.emit(superToken, "Transfer")
            .withArgs(deployer.address, alpha.address, amount);
    });

    // NOTE: this may be quite hard to test locally
    it("Should be able to execute a forward batch call", async () => {});
});
