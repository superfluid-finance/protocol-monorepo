import { ethers } from "ethers";
import {
    ICreateFlowParams,
    IConfig,
    IDeleteFlowParams,
    IUpdateFlowParams,
} from "./interfaces";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import Operation from "./Operation";
import { abi as IConstantFlowAgreementV1ABI } from "./abi/IConstantFlowAgreementV1.json";
import { normalizeAddress } from "./utils";
import { Superfluid } from "./typechain";
import { handleError } from "./errorHelper";

interface IConstantFlowAgreementV1Options {
    readonly config: IConfig;
}

// TODO: we can probably create a modifyFlow function which will remove a lot
// of repeated code from the three functions

const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);

export default class ConstantFlowAgreementV1 {
    readonly options: IConstantFlowAgreementV1Options;

    constructor(options: IConstantFlowAgreementV1Options) {
        this.options = options;
    }

    get hostContract() {
        return new ethers.Contract(
            this.options.config.hostAddress,
            SuperfluidABI
        ) as Superfluid;
    }

    createFlow = async ({
        flowRate,
        receiver,
        sender,
        token,
        userData,
    }: ICreateFlowParams): Promise<Operation> => {
        const normalizedToken = normalizeAddress(token);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("createFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);

        try {
            const txn =
                await this.hostContract.populateTransaction.callAgreement(
                    this.options.config.cfaV1Address,
                    callData,
                    userData || "0x",
                    sender ? { from: normalizedSender } : {}
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

    updateFlow = async ({
        flowRate,
        receiver,
        sender,
        token,
        userData,
    }: IUpdateFlowParams): Promise<Operation> => {
        const normalizedToken = normalizeAddress(token);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("updateFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);

        try {
            const txn =
                await this.hostContract.populateTransaction.callAgreement(
                    this.options.config.cfaV1Address,
                    callData,
                    userData || "0x",
                    sender ? { from: normalizedSender } : {}
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

        try {
            const txn =
                await this.hostContract.populateTransaction.callAgreement(
                    this.options.config.cfaV1Address,
                    callData,
                    userData || "0x",
                    { from: normalizedSender }
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
}
