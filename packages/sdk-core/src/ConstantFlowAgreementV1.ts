import { ethers } from "ethers";
import {
    IAgreementV1Options,
    ICreateFlowParams,
    IDeleteFlowParams,
    IUpdateFlowParams,
} from "./interfaces";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import Operation from "./Operation";
import { abi as IConstantFlowAgreementV1ABI } from "./abi/IConstantFlowAgreementV1.json";
import { normalizeAddress } from "./utils";
import { Superfluid } from "./typechain";
import { handleError } from "./errorHelper";

const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);

export default class ConstantFlowAgreementV1 {
    readonly options: IAgreementV1Options;

    constructor(options: IAgreementV1Options) {
        this.options = options;
    }

    get hostContract() {
        return new ethers.Contract(
            this.options.config.hostAddress,
            SuperfluidABI
        ) as Superfluid;
    }

    private populateTransactionAndReturnOperation = async (
        callData: string,
        userData: string | undefined
    ) => {
        try {
            const txn =
                await this.hostContract.populateTransaction.callAgreement(
                    this.options.config.cfaV1Address,
                    callData,
                    userData || "0x"
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };

    createFlow = async ({
        flowRate,
        receiver,
        token,
        userData,
    }: ICreateFlowParams): Promise<Operation> => {
        const normalizedToken = normalizeAddress(token);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("createFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);

        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    updateFlow = async ({
        flowRate,
        receiver,
        token,
        userData,
    }: IUpdateFlowParams): Promise<Operation> => {
        const normalizedToken = normalizeAddress(token);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("updateFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);

        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };

    deleteFlow = async ({
        token,
        sender,
        receiver,
        userData,
    }: IDeleteFlowParams): Promise<Operation> => {
        const normalizedToken = normalizeAddress(token);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("deleteFlow", [
            normalizedToken,
            normalizedSender,
            normalizedReceiver,
            "0x",
        ]);

        return await this.populateTransactionAndReturnOperation(
            callData,
            userData
        );
    };
}
