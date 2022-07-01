import {SignatureLike} from '@ethersproject/bytes';
import {ethers, UnsignedTransaction} from 'ethers';

/**
 * The use-case arose from Gnosis Safe transaction serialization failing.
 */
export const trySerializeTransaction = (
    transaction: UnsignedTransaction,
    signature?: SignatureLike
): string | undefined => {
    try {
        return ethers.utils.serializeTransaction(transaction, signature);
    } catch (error) {
        console.error(error);
    }
};
