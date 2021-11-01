import { ethers } from "ethers";
import { handleError } from "./errorHelper";

export default class Operation {
    readonly transaction: ethers.PopulatedTransaction;

    constructor(txn: ethers.PopulatedTransaction) {
        this.transaction = txn;
    }

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

    populateTransaction = async (
        signer: ethers.Signer
    ): Promise<ethers.providers.TransactionRequest> => {
        try {
            return await signer.populateTransaction(this.transaction);
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };

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
