import { ethers } from "ethers";

import Host from "./Host";
import Operation, { OperationType } from "./Operation";
import { SFError } from "./SFError";
import SuperfluidABI from "./abi/Superfluid.json";
import { getTransactionDescription, removeSigHashFromCallData } from "./utils";

interface IBatchCallOptions {
    hostAddress: string;
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
 * BatchCall Helper Class
 * @description A helper class to create `BatchCall` objects which can be executed.
 */
export default class BatchCall {
    options: IBatchCallOptions;
    host: Host;

    constructor(options: IBatchCallOptions) {
        this.options = options;
        this.host = new Host(options.hostAddress);
    }

    /**
     * Gets the call agreement function arguments.
     * @param callData callData of the function
     * @returns {ethers.utils.Result} call agreement function arguments
     */
    getCallAgreementFunctionArgs = (callData: string): ethers.utils.Result =>
        getTransactionDescription(SuperfluidABI.abi, callData).args;

    /**
     * Given an `Operation` object, gets the `OperationStruct` object.
     * @param operation an `Operation` object
     * @param index the index of the `Operation` in the batchCall
     * @returns {Promise<OperationStruct>} OperationStruct object for batchCall
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
                message: "The operation at index " + index + " is unsupported.",
            });
        }

        /* istanbul ignore next */
        if (!populatedTransaction.to || !populatedTransaction.data) {
            throw new SFError({
                type: "MISSING_TRANSACTION_PROPERTIES",
                message: "The transaction is missing the to or data property.",
            });
        }

        // Handles the Superfluid Call Agreement
        // The only operation which has a target that is not the
        // same as the to property of the transaction.
        if (operation.type === "SUPERFLUID_CALL_AGREEMENT") {
            const encoder = ethers.utils.defaultAbiCoder;
            const functionArgs = this.getCallAgreementFunctionArgs(
                populatedTransaction.data
            );
            const data = encoder.encode(
                ["bytes", "bytes"],
                [functionArgs["callData"], functionArgs["userData"]]
            );

            return {
                operationType,
                target: functionArgs["agreementClass"],
                data,
            };
        }

        // Handles other cases which are not call agreeement operation
        return {
            operationType,
            target: populatedTransaction.to,
            data: removeSigHashFromCallData(populatedTransaction.data),
        };
    };

    /**
     * Gets an array of `OperationStruct` objects to be passed to batchCall.
     * @returns {Promise<OperationStruct>[]} array of operation struct promises
     */
    get getOperationStructArrayPromises(): Promise<OperationStruct>[] {
        return this.options.operations.map((x, i) =>
            this.getOperationStruct(x, i)
        );
    }

    /**
     * Executes a batch call given the operations on this class.
     * @param signer the signer of the transaction
     * @returns {Promise<ethers.ContractTransaction>} ContractTransaction object
     */
    exec = async (
        signer: ethers.Signer
    ): Promise<ethers.ContractTransaction> => {
        try {
            const operationStructArray = await Promise.all(
                this.getOperationStructArrayPromises
            );
            return await this.host.contract
                .connect(signer)
                .batchCall(operationStructArray);
        } catch (err) {
            throw new SFError({
                type: "BATCH_CALL_ERROR",
                message: "There was an error executing your batch call:",
                cause: err,
            });
        }
    };

    /* istanbul ignore next */
    // TODO: user signs the transaction they'd like to execute and gives
    // this data to the trusted forwarder to sign
    /**
     * Executes a forward batch call given the operations on this class.
     * @param signer the signer of the transaction
     * @returns {Promise<ethers.ContractTransaction>} ContractTransaction object
     */
    execForward = async (
        signer: ethers.Signer
    ): Promise<ethers.ContractTransaction> => {
        try {
            const operationStructArray = await Promise.all(
                this.getOperationStructArrayPromises
            );
            return await this.host.contract
                .connect(signer)
                .forwardBatchCall(operationStructArray);
        } catch (err) {
            throw new SFError({
                type: "BATCH_CALL_ERROR",
                message: "There was an error executing your batch call:",
                cause: err,
            });
        }
    };
}
