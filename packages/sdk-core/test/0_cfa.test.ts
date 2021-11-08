import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { getPerSecondFlowRateByMonth } from "../src/utils";
import { IConstantFlowAgreementV1, SuperToken } from "../src/typechain";
import { setup } from "./setup";

describe("CFA V1 Tests", () => {
    let cfaV1: IConstantFlowAgreementV1;
    let framework: Framework;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperToken;
    // let bravo: SignerWithAddress;

    before(async () => {
        const { CFAV1, frameworkClass, Deployer, Alpha, SuperToken } =
            await setup();
        cfaV1 = CFAV1;
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        superToken = SuperToken;
    });

    it("Should throw an error if one of the input addresses is invalid.", async () => {
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

    it("Should create a flow properly.", async () => {
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
    });

    it("Should update a flow properly (increase flow rate).", async () => {
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

    it("Should update a flow properly (decrease flow rate).", async () => {
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

    it("Should delete a flow.", async () => {
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
