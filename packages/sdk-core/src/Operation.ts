import { ethers } from "ethers";
import { handleError } from "./errorHelper";

export type OperationType =
    | "UNSUPPORTED" // 0
    | "ERC20_APPROVE" // 1
    | "ERC20_TRANSFER_FROM" // 2
    | "SUPERTOKEN_UPGRADE" // 101
    | "SUPERTOKEN_DOWNGRADE" // 102
    | "SUPERFLUID_CALL_AGREEMENT" // 201
    | "CALL_APP_ACTION"; // 202

/**
 * @dev Operation Class
 */
export default class Operation {
    readonly transaction: ethers.PopulatedTransaction;
    readonly type: OperationType;

    constructor(txn: ethers.PopulatedTransaction, type: OperationType) {
        this.transaction = txn;
        this.type = type;
    }

    /**
     * @dev Executes the operation via the provided signer.
     * Populates all fields of the transaction, signs it and
     * sends it to the network.
     * @param signer The signer of the transacation
     * @returns {ethers.providers.TransactionResponse} A TransactionResponse object which can be awaited
     */
    exec = async (
        signer: ethers.Signer
    ): Promise<ethers.providers.TransactionResponse> => {
        try {
            return await signer.sendTransaction(this.transaction);
        } catch (err) {
            return handleError(
                "EXECUTE_TRANSACTION",
                "There was an error executing the transaction",
                JSON.stringify(err)
            );
        }
    };

    /**
     * @dev Signs the populated transaction via the provided signer (what you intend on sending to the network).
     * @param signer The signer of the transacation
     * @returns {string} Fully serialized, signed transaction
     */
    getSignedTransaction = async (signer: ethers.Signer): Promise<string> => {
        try {
            const signedTxn = await signer.signTransaction(this.transaction);
            return signedTxn;
        } catch (err) {
            return handleError(
                "SIGN_TRANSACTION",
                "There was an error signing the transaction",
                JSON.stringify(err)
            );
        }
    };

    /**
     * @dev Gets the transaction hash of the transaction.
     * Calculates this by getting the keccak256 hash of the signedTxn.
     * @param signer The signer of the transacation
     * @returns {string} The transaction hash of the transaction
     */
    getTransactionHash = async (signer: ethers.Signer): Promise<string> => {
        try {
            const signedTxn = await this.getSignedTransaction(signer);
            return ethers.utils.keccak256(signedTxn);
        } catch (err) {
            return handleError(
                "GET_TRANSACTION_HASH",
                "There was an error getting the transaction hash",
                JSON.stringify(err)
            );
        }
    };
}
