import { JsonFragment } from "@ethersproject/abi";
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
     * Gets function arguments given an ABI and callData.
     * @param abi the abi fragments of a contract/function
     * @param callData call data of the function
     * @returns {ethers.utils.Result} call agreement function arguments
     */
    getCallDataFunctionArgs = (
        abi:
            | string
            | readonly (string | ethers.utils.Fragment | JsonFragment)[],
        callData: string
    ): ethers.utils.Result => getTransactionDescription(abi, callData).args;

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

        const encoder = ethers.utils.defaultAbiCoder;

        // Handles Superfluid.callAgreement
        if (operation.type === "SUPERFLUID_CALL_AGREEMENT") {
            const functionArgs = this.getCallDataFunctionArgs(
                SuperfluidABI.abi,
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

        // Handles Superfluid.callAppAction
        if (operation.type === "CALL_APP_ACTION") {
            const functionArgs = this.getCallDataFunctionArgs(
                SuperfluidABI.abi,
                populatedTransaction.data
            );

            return {
                operationType,
                target: functionArgs["app"],
                data: functionArgs["callData"],
            };
        }

        // Handles remaining ERC20/SuperToken Operations
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
        const operationStructArray = await Promise.all(
            this.getOperationStructArrayPromises
        );
        return await this.host.contract
            .connect(signer)
            .batchCall(operationStructArray);
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
        const operationStructArray = await Promise.all(
            this.getOperationStructArrayPromises
        );
        return await this.host.contract
            .connect(signer)
            .forwardBatchCall(operationStructArray);
    };
}
