import { ethers } from "ethers";
import { abi as SuperfluidABI } from "../../sdk-core/src/abi/Superfluid.json";
import { getTransactionDescription } from "./utils";
import Host from "./Host";
import { IConfig } from "./interfaces";
import Operation, { OperationType } from "./Operation";
import { removeSigHashFromCallData } from ".";
import { handleError } from "./errorHelper";

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
     * @returns the function arguments
     */
    getCallAgreementFunctionArgs = (callData: string) =>
        getTransactionDescription(SuperfluidABI, callData).args;

    /**
     * @dev Given an `Operation` object, gets the `OperationStruct` object.
     * @param operation an `Operation` object
     * @param index the index of the `Operation` in the batchCall
     * @returns `OperationStruct`
     */
    getOperationStruct = (
        operation: Operation,
        index: number
    ): OperationStruct => {
        const operationType = operationTypeStringToTypeMap.get(operation.type);
        if (!operationType) {
            handleError(
                "UNSUPPORTED_OPERATION",
                "The operation at index " + index + " is unsupported",
                JSON.stringify(operation.transaction)
            );
        }

        if (!operation.transaction.to || !operation.transaction.data) {
            return handleError(
                "MISSING_TRANSACTION_PROPERTIES",
                "The transaction is missing the to or data property",
                JSON.stringify(operation.transaction)
            );
        }

        // Handles the Superfluid Call Agreement
        // The only operation which has a target that is not the
        // same as the to property of the transaction.
        if (operation.type === "SUPERFLUID_CALL_AGREEMENT") {
            const functionArgs = this.getCallAgreementFunctionArgs(
                operation.transaction.data
            );
            return {
                operationType: operationType!,
                target: functionArgs["agreementClass"],
                data: functionArgs["callData"],
            };
        }
        // Handles other cases which are not
        return {
            operationType: operationType!,
            target: operation.transaction.to,
            data: removeSigHashFromCallData(operation.transaction.data),
        };
    };

    /**
     * @dev Gets an array of `OperationStruct` objects to be passed to batchCall.
     */
    get operationStructArray(): OperationStruct[] {
        return this.options.operations.map((x, i) =>
            this.getOperationStruct(x, i)
        );
    }

    /**
     * @dev Executes a batch call given the current operations on this class.
     * @param signer the signer of the transaction
     * @returns ethers.ContractTransaction object
     */
    execBatchCall = async (signer: ethers.Signer) => {
        return await this.host.hostContract
            .connect(signer)
            .batchCall(this.operationStructArray);
    };

    /**
     * @dev Executes a forward batch call given the current operations on this class.
     * @param signer the signer of the transaction
     * @returns ethers.ContractTransaction object
     */
    execForwardBatchCall = async (signer: ethers.Signer) => {
        return await this.host.hostContract
            .connect(signer)
            .forwardBatchCall(this.operationStructArray);
    };
}
