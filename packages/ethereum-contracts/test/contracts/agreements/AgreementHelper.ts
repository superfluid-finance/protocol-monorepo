import {SignerWithAddress} from "@nomiclabs/hardhat-ethers/signers";
import {FunctionFragment, Interface} from "ethers/lib/utils";

import {ethers} from "hardhat";
const basePath = "../../../artifacts/contracts/";
const IConstantFlowAgreementV1Artifact = require(basePath +
    "interfaces/agreements/IConstantFlowAgreementV1.sol/IConstantFlowAgreementV1.json");
const IInstantDistributionAgreementV1Artifact = require(basePath +
    "interfaces/agreements/IInstantDistributionAgreementV1.sol/IInstantDistributionAgreementV1.json");
const SuperfluidMockArtifact = require(basePath +
    "mocks/SuperfluidMock.sol/SuperfluidMock.json");

interface CallAgreementParams {
    signer: SignerWithAddress;
    agreementAddress: string;
    callData: string;
    userData?: string;
}

interface CallAppActionParams {
    signer: SignerWithAddress;
    appAddress: string;
    callData: string;
}

interface UpdateFlowOperatorPermissionsParams {
    superToken: SignerWithAddress;
    flowOperator: string;
    permissions: string;
    flowRateAllowance: string;
}

export const FLOW_TYPE_CREATE = "createFlow";
export const FLOW_TYPE_UPDATE = "updateFlow";
export const FLOW_TYPE_DELETE = "deleteFlow";

export default class AgreementHelper {
    testEnvironment: any;
    cfaInterface: Interface;
    hostInterface: Interface;
    idaInterface: Interface;
    constructor(testEnvironment: any) {
        this.testEnvironment = testEnvironment;
        this.cfaInterface = new ethers.utils.Interface(
            IConstantFlowAgreementV1Artifact.abi
        );
        this.hostInterface = new ethers.utils.Interface(
            SuperfluidMockArtifact.abi
        );
        this.idaInterface = new ethers.utils.Interface(
            IInstantDistributionAgreementV1Artifact.abi
        );
    }

    async callAgreement(params: CallAgreementParams) {
        return await this.testEnvironment.contracts.superfluid
            .connect(params.signer)
            .callAgreement(
                params.agreementAddress,
                params.callData,
                params.userData || "0x"
            );
    }

    async callAppAction(params: CallAppActionParams) {
        return await this.testEnvironment.contracts.superfluid
            .connect(params.signer)
            .callAppAction(params.appAddress, params.callData);
    }

    getUpdateFlowOperatorPermissionsCallData(
        params: UpdateFlowOperatorPermissionsParams
    ) {
        return this.cfaInterface.encodeFunctionData(
            "updateFlowOperatorPermissions",
            [
                params.superToken,
                params.flowOperator,
                params.permissions,
                params.flowRateAllowance,
            ]
        );
    }

    getAuthorizeFlowOperatorWithFullControlCallData(params: {
        superToken: string;
        flowOperator: string;
    }) {
        return this.cfaInterface.encodeFunctionData(
            "updateFlowOperatorPermissions",
            [params.superToken, params.flowOperator]
        );
    }

    getIDACallData(fragment: string | FunctionFragment, args: readonly any[]) {
        return this.idaInterface.encodeFunctionData(fragment, args);
    }
    getModifyFlowCallData(params: {
        superToken: string;
        receiver: string;
        type: string;
        flowRate: string;
        sender: string;
    }) {
        const normalizedToken = ethers.utils.getAddress(params.superToken);
        const normalizedReceiver = ethers.utils.getAddress(params.receiver);
        const normalizedSender =
            params.type === "deleteFlow"
                ? ethers.utils.getAddress(params.sender || "")
                : "";
        const values =
            params.type === "deleteFlow"
                ? [normalizedToken, normalizedSender, normalizedReceiver, "0x"]
                : [normalizedToken, normalizedReceiver, params.flowRate, "0x"];
        return this.cfaInterface.encodeFunctionData(params.type, values);
    }

    getModifyFlowByOperatorCallData(params: {
        superToken: string;
        receiver: string;
        type: string;
        flowRate: string;
        sender: string;
    }) {
        const normalizedToken = ethers.utils.getAddress(params.superToken);
        const normalizedReceiver = ethers.utils.getAddress(params.receiver);
        const normalizedSender = ethers.utils.getAddress(params.sender);
        const values =
            params.type === "deleteFlowByOperator"
                ? [normalizedToken, normalizedSender, normalizedReceiver, "0x"]
                : [
                      normalizedToken,
                      normalizedSender,
                      normalizedReceiver,
                      params.flowRate,
                      "0x",
                  ];
        return this.cfaInterface.encodeFunctionData(params.type, values);
    }

    async modifyFlow(params: {
        superToken: string;
        receiver: string;
        type: string;
        flowRate: string;
        sender: string;
        signer: SignerWithAddress;
        userData: string;
    }) {
        const signer = params.signer
            ? params.signer
            : await ethers.getSigner(params.sender || "");
        const callData = this.getModifyFlowCallData(params);
        return this.callAgreement({
            agreementAddress: this.testEnvironment.contracts.cfa.address,
            callData,
            userData: params.userData,
            signer,
        });
    }
}