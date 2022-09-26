import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers"
import chai, { expect } from "chai"
import spies from 'chai-spies'
import { ethers } from "ethers"
import hre from "hardhat"
import { abi as SuperAppTesterABI } from "../artifacts/contracts/SuperAppTester.sol/SuperAppTester.json"
import { HARDHAT_PRIVATE_KEY, setup } from "../scripts/setup"
import { getPerSecondFlowRateByMonth } from "../src"
import { abi as IConstantFlowAgreementV1ABI } from "../src/abi/IConstantFlowAgreementV1.json"
import { Framework } from "../src/index"
import Operation from "../src/Operation"
import {
    IConstantFlowAgreementV1,
    SuperToken as SuperTokenType
} from "../src/typechain"
import { SuperAppTester } from "../typechain-types"
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test"
const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);

chai.use(spies)

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

    it("Should call host.callAgreement when shouldUseCallAgreement is true while calling createFlow", () => {
        const callAgreementSpy = chai.spy.on(framework.cfaV1.host, 'callAgreement');
        const forwarderCreateFlowSpy = chai.spy.on(framework.cfaV1.forwarder.populateTransaction, 'createFlow');
        framework.cfaV1.createFlow({
            flowRate: getPerSecondFlowRateByMonth("100"),
            receiver: bravo.address,
            superToken: superToken.address,
            shouldUseCallAgreement: true
        });
        expect(callAgreementSpy).to.have.been.called()
        expect(forwarderCreateFlowSpy).to.have.been.not.called()
    });

    // There is some context issue here
    it.skip("Should call forwarder.createFlow by default while calling createFlow", () => {
        const callAgreementSpy = chai.spy.on(framework.cfaV1.host, 'callAgreement');
        const forwarderCreateFlowSpy = chai.spy.on(framework.cfaV1.forwarder.populateTransaction, 'createFlow');
        framework.cfaV1.createFlow({
            flowRate: getPerSecondFlowRateByMonth("100"),
            receiver: bravo.address,
            superToken: superToken.address,
        });
        expect(callAgreementSpy).to.have.been.not.called()
        expect(forwarderCreateFlowSpy).to.have.been.called()
    });
    
    it("Should be able to create an operation from framework.", async () => {
        const callData = cfaInterface.encodeFunctionData("createFlow", [
            superToken.address,
            alpha.address,
            getPerSecondFlowRateByMonth("100"),
            "0x",
        ]);
        const txn = framework.host.contract.populateTransaction.callAgreement(
            cfaV1.address,
            callData,
            "0x"
        );
        const operation = framework.operation(txn, "SUPERFLUID_CALL_AGREEMENT");
        await operation.exec(deployer);
    });

    it("Should be able to create an operation from framework and execute from batch call.", async () => {
        const callData = cfaInterface.encodeFunctionData("createFlow", [
            superToken.address,
            alpha.address,
            getPerSecondFlowRateByMonth("100"),
            "0x",
        ]);
        const txn = framework.host.contract.populateTransaction.callAgreement(
            cfaV1.address,
            callData,
            "0x"
        );
        const operation = framework.operation(txn, "SUPERFLUID_CALL_AGREEMENT");
        await framework.batchCall([operation]).exec(deployer);
    });

    it("Should be able to create a call app action operation", async () => {
        const NEW_VAL = 69;
        const { superAppTester, operation } =
            await createCallAppActionOperation(deployer, framework, NEW_VAL);
        await operation.exec(deployer);
        expect(await superAppTester.val()).to.equal(NEW_VAL.toString());
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
            expect(err.message).to.contain(
                "cannot estimate gas"
            );
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
