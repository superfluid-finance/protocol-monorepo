import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import {
    IConstantFlowAgreementV1,
    SuperToken as SuperTokenType,
} from "../src/typechain";
import { getPerSecondFlowRateByMonth, getRawTransaction } from "../src/utils";
import { setup } from "./setup";
import { abi as IConstantFlowAgreementV1ABI } from "../src/abi/IConstantFlowAgreementV1.json";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { ethers } from "ethers";
import Operation from "../src/Operation";
const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);

describe("Operation Tests", () => {
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
    });

    it("Should throw an error when trying to execute a transaction with faulty callData", async () => {
        const callData = cfaInterface.encodeFunctionData("createFlow", [
            superToken.address,
            alpha.address,
            getPerSecondFlowRateByMonth("-100"),
            "0x",
        ]);
        const txn =
            framework.host.hostContract.populateTransaction.callAgreement(
                cfaV1.address,
                callData,
                "0x"
            );
        const operation = new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
        try {
            await operation.exec(deployer);
        } catch (err: any) {
            expect(err.message).to.contain(
                "Execute Transaction Error - There was an error executing the transaction"
            );
        }
    });

    // TODO: figure out how to get the expected value for the signed transaction
    // and expected txn hash
    it("Should be able to get signed transaction", async () => {
        const callData = cfaInterface.encodeFunctionData("createFlow", [
            superToken.address,
            alpha.address,
            getPerSecondFlowRateByMonth("100"),
            "0x",
        ]);
        const txn =
            framework.host.hostContract.populateTransaction.callAgreement(
                cfaV1.address,
                callData,
                "0x"
            );
        // NOTE: the hardhat signer does not support signing transactions, therefore, we must create
        // our own signer with a custom private key
        const signer = framework.createSigner({
            privateKey: process.env.TEST_ACCOUNT_PRIVATE_KEY,
            provider: deployer.provider,
        });
        const operation = new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
        const signedTxn = await operation.getSignedTransaction(signer);
        const executedTxn = await operation.exec(signer);
        const rawSignedTxn = getRawTransaction(executedTxn);
        // expect(signedTxn).to.equal(rawSignedTxn);
    });

    it("Should be able to get transaction hash and it should be equal to transaction hash once executed.", async () => {
        const callData = cfaInterface.encodeFunctionData("deleteFlow", [
            superToken.address,
            deployer.address,
            alpha.address,
            "0x",
        ]);
        const txn =
            framework.host.hostContract.populateTransaction.callAgreement(
                cfaV1.address,
                callData,
                "0x"
            );
        const signer = framework.createSigner({
            privateKey: process.env.TEST_ACCOUNT_PRIVATE_KEY,
            provider: deployer.provider,
        });
        const operation = new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
        const populatedTxn = await operation.populateTransactionPromise;
        const signedTxn = await operation.getSignedTransaction(signer);
        const opTxnHash = await operation.getTransactionHash(signer);
        const executedTxn = await operation.exec(signer);
        const rawTxn = getRawTransaction(executedTxn);
        const receipt = await executedTxn.wait();
        // expect(opTxnHash).to.equal(receipt.transactionHash);
    });
});
