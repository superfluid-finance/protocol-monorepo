import { ethers } from "ethers";
import { abi as SuperfluidABI } from "../../sdk-core/src/abi/Superfluid.json";
import { getTransactionDescription, removeSigHashFromCallData } from "./utils";
import Host from "./Host";
import { IConfig } from "./interfaces";
import Operation, { OperationType } from "./Operation";
import SFError from "./SFError";

interface IBatchCallOptions {
    config: IConfig;
    operations: ReadonlyArray<Operation>;
}

interface OperationStruct {
    readonly operationType: number;
    readonly target: string;
    readonly data: string;
}

const operationTypeStringToTypeMap = new Map<OperationType, number>([
    ["ERC20_APPROVE", 1],
    ["ERC20_TRANSFER_FROM", 2],
    ["SUPERTOKEN_UPGRADE", 101],
    ["SUPERTOKEN_DOWNGRADE", 102],
    ["SUPERFLUID_CALL_AGREEMENT", 201],
    ["CALL_APP_ACTION", 202],
]);

/**
 * @dev BatchCall Helper Class
 * @description A helper class to create `BatchCall` objects which can be executed.
 */
export default class BatchCall {
    options: IBatchCallOptions;
    host: Host;

    constructor(options: IBatchCallOptions) {
        this.options = options;
        this.host = new Host(options.config.hostAddress);
    }

    /**
     * @dev Gets the call agreement function arguments.
     * @param callData callData of the function
     * @returns call agreement function arguments
     */
    getCallAgreementFunctionArgs = (callData: string) =>
        getTransactionDescription(SuperfluidABI, callData).args;

    /**
     * @dev Given an `Operation` object, gets the `OperationStruct` object.
     * @param operation an `Operation` object
     * @param index the index of the `Operation` in the batchCall
     * @returns `OperationStruct`
     */
    getOperationStruct = async (
        operation: Operation,
        index: number
    ): Promise<OperationStruct> => {
        const operationType = operationTypeStringToTypeMap.get(operation.type);
        const populatedTransaction = await operation.populateTransactionPromise;
        if (!operationType) {
            throw new SFError({
                type: "UNSUPPORTED_OPERATION",
                customMessage:
                    "The operation at index " + index + " is unsupported.",
            });
        }

        /* istanbul ignore next */
        if (!populatedTransaction.to || !populatedTransaction.data) {
            throw new SFError({
                type: "MISSING_TRANSACTION_PROPERTIES",
                customMessage:
                    "The transaction is missing the to or data property.",
            });
        }

        // Handles the Superfluid Call Agreement
        // The only operation which has a target that is not the
        // same as the to property of the transaction.
        if (operation.type === "SUPERFLUID_CALL_AGREEMENT") {
            const functionArgs = this.getCallAgreementFunctionArgs(
                populatedTransaction.data
            );
            return {
                operationType: operationType!,
                target: functionArgs["agreementClass"],
                data: functionArgs["callData"],
            };
        }

        // Handles other cases which are not call agreeement operation
        return {
            operationType: operationType!,
            target: populatedTransaction.to,
            data: removeSigHashFromCallData(populatedTransaction.data),
        };
    };

    /**
     * @dev Gets an array of `OperationStruct` objects to be passed to batchCall.
     */
    get getOperationStructArrayPromises() {
        return this.options.operations.map((x, i) =>
            this.getOperationStruct(x, i)
        );
    }

    /**
     * @dev Executes a batch call given the operations on this class.
     * @param signer the signer of the transaction
     * @returns ethers.ContractTransaction object
     */
    exec = async (signer: ethers.Signer) => {
        try {
            const operationStructArray = await Promise.all(
                this.getOperationStructArrayPromises
            );
            return await this.host.hostContract
                .connect(signer)
                .batchCall(operationStructArray);
        } catch (err: any) {
            throw new SFError({
                type: "BATCH_CALL_ERROR",
                customMessage: "There was an error executing your batch call:",
                errorObject: err,
            });
        }
    };

    /* istanbul ignore next */
    // TODO: user signs the transaction they'd like to execute and gives
    // this data to the trusted forwarder to sign
    /**
     * @dev Executes a forward batch call given the operations on this class.
     * @param signer the signer of the transaction
     * @returns ethers.ContractTransaction object
     */
    execForward = async (signer: ethers.Signer) => {
        try {
            const operationStructArray = await Promise.all(
                this.getOperationStructArrayPromises
            );
            return await this.host.hostContract
                .connect(signer)
                .forwardBatchCall(operationStructArray);
        } catch (err: any) {
            throw new SFError({
                type: "BATCH_CALL_ERROR",
                customMessage: "There was an error executing your batch call:",
                errorObject: err,
            });
        }
    };
}
