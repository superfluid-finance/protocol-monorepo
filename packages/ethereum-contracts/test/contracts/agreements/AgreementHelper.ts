import {Signer} from "ethers";
import {ethers} from "hardhat";

import IConstantFlowAgreementV1ABI from "../../../artifacts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol/IConstantFlowAgreementV1.json";

// TEMP FOR INTERFACING W/ JS
export const FLOW_TYPE_CREATE = "createFlow";
export const FLOW_TYPE_UPDATE = "updateFlow";
export const FLOW_TYPE_DELETE = "deleteFlow";

interface UpdateFlowOperatorPermissionsParams {
    readonly superToken: string;
    readonly flowOperator: string;
    readonly permissions: string;
    readonly flowRateAllowance: string;
}
interface AuthorizeFlowOperatorWithFullControlParams {
    readonly superToken: string;
    readonly flowOperator: string;
}
interface ModifyFlowParams {
    readonly type: string;
    readonly receiver: string;
    readonly superToken: string;
    readonly signer?: Signer;
    readonly flowRate: string | null;
    readonly sender: string | null;
    readonly userData: string | null;
}
interface ModifyFlowByOperatorParams extends ModifyFlowParams {
    readonly sender: string;
}
interface CallAgreementParams {
    readonly agreementAddress: string;
    readonly callData: string;
    readonly signer: Signer;
    readonly userData: string | null;
}
interface CallAppActionParams {
    readonly appAddress: string;
    readonly callData: string;
    readonly signer: Signer;
}
export class AgreementHelper {
    readonly testEnvironment: any;
    readonly cfaInterface: any;

    constructor(testEnvironment: any) {
        this.testEnvironment = testEnvironment;
        this.cfaInterface = new ethers.utils.Interface(
            IConstantFlowAgreementV1ABI.abi
        );
    }

    callAgreement = async (params: CallAgreementParams) => {
        return await this.testEnvironment.contracts.superfluid
            .connect(params.signer)
            .callAgreement(
                params.agreementAddress,
                params.callData,
                params.userData || "0x"
            );
    };

    callAppAction = async (params: CallAppActionParams) => {
        return await this.testEnvironment.contracts.superfluid
            .connect(params.signer)
            .callAppAction(params.appAddress, params.callData);
    };

    getUpdateFlowOperatorPermissionsCallData = (
        params: UpdateFlowOperatorPermissionsParams
    ) => {
        return this.cfaInterface.encodeFunctionData(
            "updateFlowOperatorPermissions",
            [
                params.superToken,
                params.flowOperator,
                params.permissions,
                params.flowRateAllowance,
            ]
        );
    };

    getAuthorizeFlowOperatorWithFullControlCallData = (
        params: AuthorizeFlowOperatorWithFullControlParams
    ) => {
        return this.cfaInterface.encodeFunctionData(
            "updateFlowOperatorPermissions",
            [params.superToken, params.flowOperator]
        );
    };

    getModifyFlowCallData = (params: ModifyFlowParams) => {
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
    };

    getModifyFlowByOperatorCallData = (params: ModifyFlowByOperatorParams) => {
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
    };

    modifyFlow = async (params: ModifyFlowParams) => {
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
    };
}
