import { TransactionRequest } from "@ethersproject/abstract-provider";
import { ethers } from "ethers";

export type OperationType =
    | "UNSUPPORTED" // 0
    | "ERC20_APPROVE" // 1
    | "ERC20_TRANSFER_FROM" // 2
    | "SUPERTOKEN_UPGRADE" // 101
    | "SUPERTOKEN_DOWNGRADE" // 102
    | "SUPERFLUID_CALL_AGREEMENT" // 201
    | "CALL_APP_ACTION"; // 202

/**
 * Operation Helper Class
 * @description A helper class to create `Operation` objects which can be executed or batched.
 */
export default class Operation {
    readonly populateTransactionPromise: Promise<ethers.PopulatedTransaction>;
    readonly type: OperationType;

    constructor(
        txn: Promise<ethers.PopulatedTransaction>,
        type: OperationType
    ) {
        this.populateTransactionPromise = txn;
        this.type = type;
    }

    /**
     * Executes the operation via the provided signer.
     * @description Populates all fields of the transaction, signs it and sends it to the network.
     * @param signer The signer of the transaction
     * @returns {ethers.providers.TransactionResponse} A TransactionResponse object which can be awaited
     */
    exec = async (
        signer: ethers.Signer
    ): Promise<ethers.providers.TransactionResponse> => {
        const populatedTransaction = await this.getPopulatedTransactionRequest(
            signer
        );
        return await signer.sendTransaction(populatedTransaction);
    };

    /**
     * Get the populated transaction by awaiting `populateTransactionPromise`.
     * @description Note that we need to populate the txn with the signer.
     * @returns {Promise<TransactionRequest>}
     */
    getPopulatedTransactionRequest = async (
        signer: ethers.Signer
    ): Promise<TransactionRequest> => {
        const prePopulated = await this.populateTransactionPromise;
        return await signer.populateTransaction(prePopulated);
    };
    /**
     * Signs the populated transaction via the provided signer (what you intend on sending to the network).
     * @param signer The signer of the transaction
     * @returns {Promise<string>} Fully serialized, signed transaction
     */
    getSignedTransaction = async (signer: ethers.Signer): Promise<string> => {
        const populatedTransaction = await this.getPopulatedTransactionRequest(
            signer
        );
        const signedTxn = await signer.signTransaction(populatedTransaction);
        return signedTxn;
    };

    /**
     * Gets the transaction hash of the transaction.
     * @description Calculates this by getting the keccak256 hash of the signedTxn.
     * @param signer The signer of the transaction
     * @returns {Promise<string>} The transaction hash of the transaction
     */
    getTransactionHash = async (signer: ethers.Signer): Promise<string> => {
        const signedTxn = await this.getSignedTransaction(signer);
        return ethers.utils.keccak256(signedTxn);
    };
}
