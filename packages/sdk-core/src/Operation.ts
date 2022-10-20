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

    // @note This property is used to ensure BatchCall operations still function
    // when using the agreement forwarder
    readonly forwarderPopulatedPromise?: Promise<ethers.PopulatedTransaction>;

    constructor(
        txn: Promise<ethers.PopulatedTransaction>,
        type: OperationType,
        forwarderPopulatedPromise?: Promise<ethers.PopulatedTransaction>
    ) {
        this.populateTransactionPromise = txn;
        this.type = type;
        this.forwarderPopulatedPromise = forwarderPopulatedPromise;
    }

    /**
     * Executes the operation via the provided signer.
     * @description Populates all fields of the transaction, signs it and sends it to the network.
     * @param signer The signer of the transaction
     * @param gasLimitMultiplier A multiplier to provide gasLimit buffer on top of the estimated gas limit (1.2x is the default)
     * @returns {ethers.providers.TransactionResponse} A TransactionResponse object which can be awaited
     */
    exec = async (
        signer: ethers.Signer,
        gasLimitMultiplier = 1.2
    ): Promise<ethers.providers.TransactionResponse> => {
        const populatedTransaction = await this.getPopulatedTransactionRequest(
            signer,
            gasLimitMultiplier
        );
        return await signer.sendTransaction(populatedTransaction);
    };

    /**
     * Get the populated transaction by awaiting `populateTransactionPromise`.
     * @description Note that we need to populate the txn with the signer.
     * NOTE: we use the forwarder populated promise if this exists
     * @returns {Promise<TransactionRequest>}
     */
    getPopulatedTransactionRequest = async (
        signer: ethers.Signer,
        gasLimitMultiplier = 1.2
    ): Promise<TransactionRequest> => {
        const txnToPopulate = this.forwarderPopulatedPromise
            ? await this.forwarderPopulatedPromise
            : await this.populateTransactionPromise;
        const signerPopulatedTransaction = await signer.populateTransaction(
            txnToPopulate
        );

        // if gasLimit exists, an Overrides object has been passed or the user has explicitly set
        // a gasLimit for their transaction prior to execution and so we keep it as is else we apply
        // a specified or the default (1.2) multiplier on the gas limit.
        return txnToPopulate.gasLimit
            ? txnToPopulate
            : {
                  ...signerPopulatedTransaction,
                  gasLimit:
                      // @note if gasLimit is null, this function will throw due to
                      // conversion to BigNumber, so we must round this number
                      // we can be more conservative by using Math.ceil instead of Math.round
                      Math.ceil(
                          Number(
                              signerPopulatedTransaction.gasLimit?.toString()
                          ) * gasLimitMultiplier
                      ),
              };
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
