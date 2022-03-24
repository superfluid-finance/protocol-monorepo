import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { getFlowOperatorId, getPerSecondFlowRateByMonth } from "../src/utils";
import { IConstantFlowAgreementV1, SuperToken } from "../src/typechain";
import { setup } from "../scripts/setup";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { BigNumberish, ethers } from "ethers";

const toBN = (x: BigNumberish) => ethers.BigNumber.from(x);
const MAX_FLOW_RATE = toBN(2).pow(toBN(95)).sub(toBN(1)).toString();

describe("CFA V1 Tests", () => {
    let cfaV1: IConstantFlowAgreementV1;
    let framework: Framework;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let bravo: SignerWithAddress;
    let superToken: SuperToken;
    // let bravo: SignerWithAddress;

    before(async () => {
        const { CFAV1, frameworkClass, Deployer, Alpha, Bravo, SuperToken } =
            await setup({ subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT });
        cfaV1 = CFAV1;
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        bravo = Bravo;
        superToken = SuperToken;
    });

    describe("Revert cases", () => {
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

        it("Should throw an error on getFlow functions as expected", async () => {
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

        it("Should throw an error on getFlowOperatorData functions as expected", async () => {
            // NOTE: using casting to pass in wrong input to force error
            // get flowOperatorData throw
            try {
                await framework.cfaV1.getFlowOperatorData({
                    superToken: superToken.address,
                    sender: deployer.address,
                    flowOperator: alpha.address,
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "ConstantFlowAgreementV1 Read Error - There was an error getting flow operator data"
                );
            }
            try {
                await framework.cfaV1.getFlowOperatorDataByID({
                    superToken: superToken.address,
                    flowOperatorId: "",
                    providerOrSigner: "" as any,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "ConstantFlowAgreementV1 Read Error - There was an error getting flow operator data"
                );
            }
        });
    });

    describe("Sender Change Flow", () => {
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
            const alphaAccountFlowInfo =
                await framework.cfaV1.getAccountFlowInfo({
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
    });

    describe("Flow Operator Change Flow", () => {
        let sender: string;
        let flowOperator: string;
        let receiver: string;
        before(() => {
            sender = deployer.address;
            flowOperator = alpha.address;
            receiver = bravo.address;
        });

        it("Should be able to update flow operator permissions", async () => {
            const flowRateAllowance = getPerSecondFlowRateByMonth("100");
            let permissions = 1; // ALLOW_CREATE

            const flowOperatorId = getFlowOperatorId(sender, flowOperator);

            // Update Flow Operator Permissions
            const updateFlowOperatorPermissionsOperation =
                framework.cfaV1.updateFlowOperatorPermissions({
                    flowRateAllowance,
                    sender,
                    flowOperator,
                    permissions,
                    superToken: superToken.address,
                });
            await expect(updateFlowOperatorPermissionsOperation.exec(deployer))
                .to.emit(cfaV1, "FlowOperatorUpdated")
                .withArgs(
                    superToken.address,
                    sender,
                    flowOperator,
                    permissions,
                    Number(flowRateAllowance)
                );

            // getFlowOperatorData test
            let flowOperatorData = await framework.cfaV1.getFlowOperatorData({
                superToken: superToken.address,
                sender,
                flowOperator,
                providerOrSigner: deployer,
            });
            expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
            expect(flowOperatorData.flowRateAllowance).equals(
                flowRateAllowance
            );
            expect(flowOperatorData.permissions).equals(permissions.toString());

            // Revoke Flow Operator With Full Control Permissions
            permissions = 0; // no permissions
            const revokeFlowOperatorWithFullControlOperation =
                framework.cfaV1.revokeFlowOperatorWithFullControl({
                    sender,
                    flowOperator,
                    superToken: superToken.address,
                });
            await expect(
                revokeFlowOperatorWithFullControlOperation.exec(deployer)
            )
                .to.emit(cfaV1, "FlowOperatorUpdated")
                .withArgs(
                    superToken.address,
                    deployer.address,
                    flowOperator,
                    permissions,
                    0
                );
            // getFlowOperatorDataByID test
            flowOperatorData = await framework.cfaV1.getFlowOperatorDataByID({
                superToken: superToken.address,
                flowOperatorId,
                providerOrSigner: deployer,
            });
            expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
            expect(flowOperatorData.flowRateAllowance).equals("0");
            expect(flowOperatorData.permissions).equals(permissions.toString());

            // Authorize Flow Operator With Full Control
            permissions = 7; // all permissions
            const authorizeFlowOperatorWithFullControlOperation =
                framework.cfaV1.authorizeFlowOperatorWithFullControl({
                    sender,
                    flowOperator,
                    superToken: superToken.address,
                });
            await expect(
                authorizeFlowOperatorWithFullControlOperation.exec(deployer)
            )
                .to.emit(cfaV1, "FlowOperatorUpdated")
                .withArgs(
                    superToken.address,
                    deployer.address,
                    flowOperator,
                    permissions,
                    MAX_FLOW_RATE // max flow rate ((2 ** 95) - 1)
                );

            // getFlowOperatorDataByID test
            flowOperatorData = await framework.cfaV1.getFlowOperatorDataByID({
                superToken: superToken.address,
                flowOperatorId,
                providerOrSigner: deployer,
            });
            expect(flowOperatorData.flowOperatorId).equals(flowOperatorId);
            expect(flowOperatorData.flowRateAllowance).equals(MAX_FLOW_RATE);
            expect(flowOperatorData.permissions).equals(permissions.toString());
        });

        it("Should be able to create flow by operator", async () => {
            const flowRate = getPerSecondFlowRateByMonth("100");
            const operation = framework.cfaV1.createFlowByOperator({
                flowRate,
                sender,
                receiver,
                superToken: superToken.address,
            });
            const deposit = toBN(flowRate).mul(toBN(3600));
            expect(operation.exec(deployer))
                .to.emit(cfaV1, "FlowUpdatedExtension")
                .withArgs(flowOperator, deposit.toString());
        });

        it("Should be able to update flow by operator", async () => {
            const flowRate = getPerSecondFlowRateByMonth("70");
            const operation = framework.cfaV1.updateFlowByOperator({
                flowRate,
                sender,
                receiver,
                superToken: superToken.address,
            });
            const deposit = toBN(flowRate).mul(toBN(3600));
            expect(operation.exec(deployer))
                .to.emit(cfaV1, "FlowUpdatedExtension")
                .withArgs(flowOperator, deposit.toString());
        });

        it("Should be able to delete flow by operator", async () => {
            const operation = framework.cfaV1.deleteFlowByOperator({
                sender,
                receiver,
                superToken: superToken.address,
            });
            expect(operation.exec(deployer))
                .to.emit(cfaV1, "FlowUpdatedExtension")
                .withArgs(flowOperator, "0");
        });
    });

    describe("Miscellaneous", () => {
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
});
