import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import {
    IConstantFlowAgreementV1,
    SuperToken as SuperTokenType,
} from "../src/typechain";
import { getPerSecondFlowRateByMonth } from "../src";
import { HARDHAT_PRIVATE_KEY, setup } from "../scripts/setup";
import { abi as IConstantFlowAgreementV1ABI } from "../src/abi/IConstantFlowAgreementV1.json";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { ethers } from "ethers";
import Operation from "../src/Operation";
import hre from "hardhat";
const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);

describe("Operation Tests", () => {
    let evmSnapshotId: string;
    let framework: Framework;
    let cfaV1: IConstantFlowAgreementV1;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperTokenType;
    let bravo: SignerWithAddress;

    before(async () => {
        const { frameworkClass, CFAV1, Deployer, Alpha, Bravo, SuperToken } =
            await setup({
                subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
            });
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        bravo = Bravo;
        superToken = SuperToken;
        cfaV1 = CFAV1;
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    beforeEach(async () => {
        await hre.network.provider.send("evm_revert", [evmSnapshotId]);
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    it("Should be able to get transaction hash and it should be equal to transaction hash once executed", async () => {
        const revokeControlOp =
            framework.cfaV1.revokeFlowOperatorWithFullControl({
                superToken: superToken.address,
                flowOperator: alpha.address,
            });
        const signer = framework.createSigner({
            privateKey: HARDHAT_PRIVATE_KEY,
            provider: deployer.provider,
        });
        const opTxnHash = await revokeControlOp.getTransactionHash(signer);
        const executedTxn = await revokeControlOp.exec(signer);
        const receipt = await executedTxn.wait();
        expect(opTxnHash).to.equal(receipt.transactionHash);
    });

    it("Should throw an error when trying to execute a transaction with faulty callData", async () => {
        const callData = cfaInterface.encodeFunctionData("createFlow", [
            superToken.address,
            alpha.address,
            getPerSecondFlowRateByMonth("-100"),
            "0x",
        ]);
        const txn = framework.host.contract.populateTransaction.callAgreement(
            cfaV1.address,
            callData,
            "0x"
        );
        const operation = new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
        try {
            await operation.exec(deployer);
        } catch (err: any) {
            expect(err.message).to.contain("cannot estimate gas");
        }
    });

    it("Should throw error when trying to sign a transaction", async () => {
        const operation = framework.cfaV1.createFlow({
            flowRate: getPerSecondFlowRateByMonth("100"),
            receiver: bravo.address,
            superToken: superToken.address,
        });
        try {
            await operation.getSignedTransaction(alpha);
        } catch (err: any) {
            expect(err.message).to.contain(
                "signing transactions is unsupported"
            );
        }
    });

    it("Should be able to get signed transaction", async () => {
        const daix = await framework.loadSuperToken(superToken.address);
        const flowRate = getPerSecondFlowRateByMonth("100");
        // NOTE: the hardhat signer does not support signing transactions, therefore, we must create
        // our own signer with a custom private key
        const signer = framework.createSigner({
            privateKey: HARDHAT_PRIVATE_KEY,
            provider: deployer.provider,
        });
        const createFlowOp = daix.createFlow({
            sender: deployer.address,
            receiver: alpha.address,
            flowRate,
        });
        const signedTxn = await createFlowOp.getSignedTransaction(signer);
        await expect(deployer.provider!.sendTransaction(signedTxn))
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
});
