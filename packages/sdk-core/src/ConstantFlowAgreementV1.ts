import { ethers } from "ethers";
import { ICreateFlowParams, IConfig } from "./interfaces";
import Operation from "./Operation";
import { abi as IConstantFlowAgreementV1ABI } from "./abi/IConstantFlowAgreementV1.json";
import { normalizeAddress } from "./utils";
import { Superfluid } from "./typechain";
import { handleError } from "./errorHelper";

interface IConstantFlowAgreementV1Options {
    readonly config: IConfig;
    readonly hostContract: Superfluid;
}

const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);

export default class ConstantFlowAgreementV1 {
    readonly options: IConstantFlowAgreementV1Options;

    constructor(options: IConstantFlowAgreementV1Options) {
        this.options = options;
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
                await this.options.hostContract.populateTransaction.callAgreement(
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
