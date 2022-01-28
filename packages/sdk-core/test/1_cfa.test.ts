import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { getPerSecondFlowRateByMonth } from "../src/utils";
import { IConstantFlowAgreementV1, SuperToken } from "../src/typechain";
import { setup } from "../scripts/setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";

describe("CFA V1 Tests", () => {
    let cfaV1: IConstantFlowAgreementV1;
    let framework: Framework;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperToken;
    // let bravo: SignerWithAddress;

    before(async () => {
        const { CFAV1, frameworkClass, Deployer, Alpha, SuperToken } =
            await setup({ subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT });
        cfaV1 = CFAV1;
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        superToken = SuperToken;
    });

    it("Should throw an error if one of the input addresses is invalid", async () => {
        const flowRate = getPerSecondFlowRateByMonth("100");
        try {
            framework.cfaV1.createFlow({
                flowRate,
                receiver: alpha.address + "0",
                superToken: superToken.address,
            });
        } catch (err: any) {
            expect(err.message).to.eql(
                "Invalid Address Error - The address you have entered is not a valid ethereum address."
            );
        }
    });

    it("Should throw an error on the reads as expected", async () => {
        // NOTE: using casting to pass in wrong input to force error
        // get flow throw
        try {
            await framework.cfaV1.getFlow({
                superToken: superToken.address,
                sender: deployer.address,
                receiver: alpha.address,
                providerOrSigner: "" as any,
            });
        } catch (err: any) {
            expect(err.message).to.contain(
                "ConstantFlowAgreementV1 Read Error - There was an error getting the flow"
            );
        }

        // get account flow info throw
        try {
            await framework.cfaV1.getAccountFlowInfo({
                superToken: superToken.address,
                account: deployer.address,
                providerOrSigner: "" as any,
            });
        } catch (err: any) {
            expect(err.message).to.contain(
                "ConstantFlowAgreementV1 Read Error - There was an error getting the account flow information"
            );
        }

        // get net flow throw
        try {
            await framework.cfaV1.getNetFlow({
                superToken: superToken.address,
                account: deployer.address,
                providerOrSigner: "" as any,
            });
        } catch (err: any) {
            expect(err.message).to.contain(
                "ConstantFlowAgreementV1 Read Error - There was an error getting net flow"
            );
        }
    });

    it("Should create a flow properly and should get the newly created flow, account flow info and net flow", async () => {
        const flowRate = getPerSecondFlowRateByMonth("100");
        const operation = framework.cfaV1.createFlow({
            flowRate,
            receiver: alpha.address,
            superToken: superToken.address,
        });
        await expect(operation.exec(deployer))
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

        // get flow check
        const flow = await framework.cfaV1.getFlow({
            superToken: superToken.address,
            sender: deployer.address,
            receiver: alpha.address,
            providerOrSigner: deployer,
        });
        expect(flow.flowRate).to.equal(flowRate);

        // get account flow info check
        const deployerAccountFlowInfo =
            await framework.cfaV1.getAccountFlowInfo({
                superToken: superToken.address,
                account: deployer.address,
                providerOrSigner: deployer,
            });
        const alphaAccountFlowInfo = await framework.cfaV1.getAccountFlowInfo({
            superToken: superToken.address,
            account: alpha.address,
            providerOrSigner: alpha,
        });
        expect(Number(deployerAccountFlowInfo.flowRate)).to.equal(
            Number(flowRate) * -1
        );
        expect(Number(alphaAccountFlowInfo.flowRate)).to.equal(
            Number(flowRate)
        );

        // get net flow check
        const deployerNetFlow = await framework.cfaV1.getNetFlow({
            superToken: superToken.address,
            account: deployer.address,
            providerOrSigner: deployer,
        });
        const alphaNetFlow = await framework.cfaV1.getNetFlow({
            superToken: superToken.address,
            account: alpha.address,
            providerOrSigner: alpha,
        });
        expect(Number(deployerNetFlow)).to.equal(Number(flowRate) * -1);
        expect(Number(alphaNetFlow)).to.equal(Number(flowRate));
    });

    it("Should update a flow properly (increase flow rate)", async () => {
        const flowRate = getPerSecondFlowRateByMonth("150");
        const operation = framework.cfaV1.updateFlow({
            flowRate,
            receiver: alpha.address,
            superToken: superToken.address,
        });
        await expect(operation.exec(deployer))
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

    it("Should update a flow properly (decrease flow rate)", async () => {
        const flowRate = getPerSecondFlowRateByMonth("90");
        const operation = framework.cfaV1.updateFlow({
            flowRate,
            receiver: alpha.address,
            superToken: superToken.address,
        });
        await expect(operation.exec(deployer))
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

    it("Should delete a flow", async () => {
        const operation = framework.cfaV1.deleteFlow({
            sender: deployer.address,
            receiver: alpha.address,
            superToken: superToken.address,
        });
        await expect(operation.exec(deployer))
            .to.emit(cfaV1, "FlowUpdated")
            .withArgs(
                superToken.address,
                deployer.address,
                alpha.address,
                0,
                0,
                0,
                "0x"
            );
    });

    it("Should return a v in the transaction (EIP-155)", async () => {
        const flowRate = getPerSecondFlowRateByMonth("100");
        const txnResponse = await framework.cfaV1
            .createFlow({
                flowRate,
                receiver: alpha.address,
                superToken: superToken.address,
            })
            .exec(deployer);
        const txnReceipt = await txnResponse.wait();
        const transaction = await deployer.provider!.getTransaction(
            txnReceipt.transactionHash
        );

        // per https://eips.ethereum.org/EIPS/eip-155
        expect(transaction.v).to.exist;

        await framework.cfaV1
            .deleteFlow({
                sender: deployer.address,
                receiver: alpha.address,
                superToken: superToken.address,
            })
            .exec(deployer);
    });
});
