import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { getPerSecondFlowRateByMonth } from "../src";
import { abi as IConstantFlowAgreementV1ABI } from "../src/abi/IConstantFlowAgreementV1.json";
import { ethers } from "ethers";
import Operation from "../src/Operation";
import hre from "hardhat";
import { SuperAppTester } from "../typechain-types";
import { abi as SuperAppTesterABI } from "../artifacts/contracts/SuperAppTester.sol/SuperAppTester.json";
const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);
import { TestEnvironment, makeSuite } from "./TestEnvironment";

chai.use(spies);

/**
 * Create a simple call app action (setVal) operation with the SuperAppTester contract.
 * @param deployer
 * @param framework
 * @returns Operation
 */
export const createCallAppActionOperation = async (
    deployer: SignerWithAddress,
    framework: Framework,
    val: number
) => {
    const SuperAppTesterFactory = await hre.ethers.getContractFactory(
        "SuperAppTester",
        deployer
    );
    let superAppTester = (await SuperAppTesterFactory.deploy(
        framework.contracts.host.address
    )) as SuperAppTester;

    const superAppTesterInterface = new ethers.utils.Interface(
        SuperAppTesterABI
    );
    superAppTester = (await superAppTester.deployed()).connect(deployer);

    // initial val will be 0 when contract is initialized
    expect(await superAppTester.val()).to.equal("0");

    const callData = superAppTesterInterface.encodeFunctionData("setVal", [
        val,
        "0x",
    ]);
    return {
        operation: framework.host.callAppAction(
            superAppTester.address,
            callData
        ),
        superAppTester,
    };
};

makeSuite("Operation Tests", (testEnv: TestEnvironment) => {
    it("Should be able to get transaction hash and it should be equal to transaction hash once executed", async () => {
        const revokeControlOp =
            testEnv.sdkFramework.cfaV1.revokeFlowOperatorWithFullControl({
                superToken: testEnv.wrapperSuperToken.address,
                flowOperator: testEnv.bob.address,
            });
        const signer = testEnv.sdkFramework.createSigner({
            privateKey: testEnv.constants.HARDHAT_PRIVATE_KEY,
            provider: testEnv.alice.provider,
        });
        const opTxnHash = await revokeControlOp.getTransactionHash(signer);
        const executedTxn = await revokeControlOp.exec(signer);
        const receipt = await executedTxn.wait();
        expect(opTxnHash).to.equal(receipt.transactionHash);
    });

    it("Should call host.callAgreement when shouldUseCallAgreement is true while calling createFlow", () => {
        const callAgreementSpy = chai.spy.on(
            testEnv.sdkFramework.cfaV1.host,
            "callAgreement"
        );
        const forwarderCreateFlowSpy = chai.spy.on(
            testEnv.sdkFramework.cfaV1.forwarder.populateTransaction,
            "createFlow"
        );
        testEnv.sdkFramework.cfaV1.createFlow({
            flowRate: getPerSecondFlowRateByMonth("100"),
            receiver: testEnv.bob.address,
            superToken: testEnv.wrapperSuperToken.address,
            shouldUseCallAgreement: true,
        });
        expect(callAgreementSpy).to.have.been.called();
        expect(forwarderCreateFlowSpy).to.have.been.not.called();
    });

    // There is some context issue here
    it.skip("Should call forwarder.createFlow by default while calling createFlow", () => {
        const callAgreementSpy = chai.spy.on(
            testEnv.sdkFramework.cfaV1.host,
            "callAgreement"
        );
        const forwarderCreateFlowSpy = chai.spy.on(
            testEnv.sdkFramework.cfaV1.forwarder.populateTransaction,
            "createFlow"
        );
        testEnv.sdkFramework.cfaV1.createFlow({
            flowRate: getPerSecondFlowRateByMonth("100"),
            receiver: testEnv.bob.address,
            superToken: testEnv.wrapperSuperToken.address,
        });
        expect(callAgreementSpy).to.have.been.not.called();
        expect(forwarderCreateFlowSpy).to.have.been.called();
    });

    it("Should be able to create an operation from framework.", async () => {
        const callData = cfaInterface.encodeFunctionData("createFlow", [
            testEnv.wrapperSuperToken.address,
            testEnv.bob.address,
            getPerSecondFlowRateByMonth("100"),
            "0x",
        ]);
        const txn =
            testEnv.sdkFramework.host.contract.populateTransaction.callAgreement(
                testEnv.cfaV1.address,
                callData,
                "0x"
            );
        const operation = testEnv.sdkFramework.operation(
            txn,
            "SUPERFLUID_CALL_AGREEMENT"
        );
        await operation.exec(testEnv.alice);
    });

    it("Should be able to create an operation from framework and execute from batch call.", async () => {
        const callData = cfaInterface.encodeFunctionData("createFlow", [
            testEnv.wrapperSuperToken.address,
            testEnv.bob.address,
            getPerSecondFlowRateByMonth("100"),
            "0x",
        ]);
        const txn =
            testEnv.sdkFramework.host.contract.populateTransaction.callAgreement(
                testEnv.cfaV1.address,
                callData,
                "0x"
            );
        const operation = testEnv.sdkFramework.operation(
            txn,
            "SUPERFLUID_CALL_AGREEMENT"
        );
        await testEnv.sdkFramework.batchCall([operation]).exec(testEnv.alice);
    });

    it("Should be able to create a call app action operation", async () => {
        const NEW_VAL = 69;
        const { superAppTester, operation } =
            await createCallAppActionOperation(
                testEnv.alice,
                testEnv.sdkFramework,
                NEW_VAL
            );
        await operation.exec(testEnv.alice);
        expect(await superAppTester.val()).to.equal(NEW_VAL.toString());
    });

    it("Should throw an error when trying to execute a transaction with faulty callData", async () => {
        const callData = cfaInterface.encodeFunctionData("createFlow", [
            testEnv.wrapperSuperToken.address,
            testEnv.alice.address,
            getPerSecondFlowRateByMonth("-100"),
            "0x",
        ]);
        const txn =
            testEnv.sdkFramework.host.contract.populateTransaction.callAgreement(
                testEnv.cfaV1.address,
                callData,
                "0x"
            );
        const operation = new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
        try {
            await operation.exec(testEnv.alice);
        } catch (err: any) {
            expect(err.message).to.contain("cannot estimate gas");
        }
    });

    it("Should throw error when trying to sign a transaction", async () => {
        const operation = testEnv.sdkFramework.cfaV1.createFlow({
            flowRate: getPerSecondFlowRateByMonth("100"),
            receiver: testEnv.charlie.address,
            superToken: testEnv.wrapperSuperToken.address,
            shouldUseCallAgreement: true,
        });
        try {
            await operation.getSignedTransaction(testEnv.bob);
        } catch (err: any) {
            expect(err.message).to.contain(
                "signing transactions is unsupported"
            );
        }
    });

    it("Should be able to get signed transaction", async () => {
        const fDAIx = await testEnv.sdkFramework.loadSuperToken(
            testEnv.wrapperSuperToken.address
        );
        const flowRate = getPerSecondFlowRateByMonth("100");
        // NOTE: the hardhat signer does not support signing transactions, therefore, we must create
        // our own signer with a custom private key
        const signer = testEnv.sdkFramework.createSigner({
            privateKey: testEnv.constants.HARDHAT_PRIVATE_KEY,
            provider: testEnv.alice.provider,
        });
        const createFlowOp = fDAIx.createFlow({
            sender: testEnv.alice.address,
            receiver: testEnv.bob.address,
            flowRate,
            shouldUseCallAgreement: true,
        });
        const signedTxn = await createFlowOp.getSignedTransaction(signer);
        await expect(testEnv.alice.provider!.sendTransaction(signedTxn))
            .to.emit(testEnv.cfaV1, "FlowUpdated")
            .withArgs(
                testEnv.wrapperSuperToken.address,
                testEnv.alice.address,
                testEnv.bob.address,
                Number(flowRate),
                Number(flowRate) * -1,
                Number(flowRate),
                "0x"
            );
    });
});
