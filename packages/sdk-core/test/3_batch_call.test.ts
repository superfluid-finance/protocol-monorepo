import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { getPerSecondFlowRateByMonth } from "../src";
import { SuperToken as SuperTokenType } from "../src/typechain";
import { setup } from "../scripts/setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { ethers } from "ethers";
import hre from "hardhat";
import { createCallAppActionOperation } from "./2_operation.test";

describe("Batch Call Tests", () => {
    let evmSnapshotId: string;
    let framework: Framework;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let bravo: SignerWithAddress;
    let charlie: SignerWithAddress;
    let superToken: SuperTokenType;

    before(async () => {
        const { frameworkClass, Deployer, Alpha, Bravo, Charlie, SuperToken } =
            await setup({
                subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
            });
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        bravo = Bravo;
        charlie = Charlie;
        superToken = SuperToken;
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    beforeEach(async () => {
        await hre.network.provider.send("evm_revert", [evmSnapshotId]);
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    it("Should throw an error on unsupported operations", async () => {
        const daix = await framework.loadSuperToken(superToken.address);
        const transferOp = daix.transfer({
            receiver: alpha.address,
            amount: ethers.utils.parseUnits("1000").toString(),
        });
        const batchCall = framework.batchCall([transferOp]);

        try {
            const promises = batchCall.getOperationStructArrayPromises;
            await Promise.all(promises);
        } catch (err: any) {
            expect(err.message).to.contain(
                "Unsupported Batch Call Operation Error: The operation at index 0 is unsupported"
            );
            expect(err.cause).to.be.undefined;
        }
    });

    it("Should throw an error if batch call fails", async () => {
        const daix = await framework.loadSuperToken(superToken.address);
        const createFlowOp = daix.createFlow({
            sender: alpha.address,
            receiver: deployer.address,
            flowRate: getPerSecondFlowRateByMonth("10000"),
        });
        try {
            await framework.batchCall([createFlowOp]).exec(deployer);
        } catch (err: any) {
            expect(err.message).to.contain(
                "cannot estimate gas;"
            );
        }
    });

    it("Should be able to create and execute a batch call (approve + transferFrom)", async () => {
        const amount = ethers.utils.parseUnits("1000").toString();
        const daix = await framework.loadSuperToken(superToken.address);
        const approveOp = daix.approve({ receiver: alpha.address, amount });
        const transferFromOp = daix.transferFrom({
            sender: deployer.address,
            receiver: alpha.address,
            amount,
        });
        const batchCall = framework.batchCall([approveOp, transferFromOp]);
        await expect(batchCall.exec(deployer))
            .to.emit(superToken, "Transfer")
            .withArgs(deployer.address, alpha.address, amount);
    });

    it("Should be able to batch create flows and batch update flows and batch delete flows", async () => {
        let flowRate = getPerSecondFlowRateByMonth("10000");
        const daix = await framework.loadSuperToken(superToken.address);
        const createFlow1 = daix.createFlow({
            sender: charlie.address,
            receiver: alpha.address,
            flowRate,
        });
        const createFlow2 = daix.createFlow({
            sender: charlie.address,
            receiver: bravo.address,
            flowRate,
        });
        await framework.batchCall([createFlow1, createFlow2]).exec(charlie);

        flowRate = getPerSecondFlowRateByMonth("12500");

        const updateFlow1 = daix.updateFlow({
            sender: charlie.address,
            receiver: alpha.address,
            flowRate,
        });
        const updateFlow2 = daix.updateFlow({
            sender: charlie.address,
            receiver: bravo.address,
            flowRate,
        });
        await framework.batchCall([updateFlow1, updateFlow2]).exec(charlie);
        const deleteFlow1 = daix.deleteFlow({
            sender: charlie.address,
            receiver: alpha.address,
        });
        const deleteFlow2 = daix.deleteFlow({
            sender: charlie.address,
            receiver: bravo.address,
        });
        await framework.batchCall([deleteFlow1, deleteFlow2]).exec(charlie);
    });

    it("Should be able to create a call app action operation and execute from batch call", async () => {
        const NEW_VAL = 69;
        const { superAppTester, operation } =
            await createCallAppActionOperation(deployer, framework, NEW_VAL);
        await framework.batchCall([operation]).exec(deployer);
        expect(await superAppTester.val()).to.equal("69");
    });

    // NOTE: this may be quite hard to test locally
    // it.skip("Should be able to execute a forward batch call", async () => {});
});
