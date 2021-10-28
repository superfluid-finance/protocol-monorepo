import { ethers } from "ethers";

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
            throw new Error(JSON.stringify(err));
        }
    };

    populateTransaction = async (
        signer: ethers.Signer
    ): Promise<ethers.providers.TransactionRequest> => {
        try {
            return await signer.populateTransaction(this.transaction);
        } catch (err) {
            throw new Error(JSON.stringify(err));
        }
    };

    getSignedTransaction = async (signer: ethers.Signer): Promise<string> => {
        try {
            const signedTxn = await signer.signTransaction(this.transaction);
            return signedTxn;
        } catch (err) {
            throw new Error(JSON.stringify(err));
        }
    };

    getTransactionHash = async (signer: ethers.Signer): Promise<string> => {
        try {
            const signedTxn = await this.getSignedTransaction(signer);
            return ethers.utils.keccak256(signedTxn);
        } catch (err) {
            throw new Error(JSON.stringify(err));
        }
    };
}
