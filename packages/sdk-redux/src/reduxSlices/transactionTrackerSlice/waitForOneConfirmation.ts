import {ethers} from 'ethers';

import {MillisecondTimes} from '../../utils';

/**
 *
 * @param provider
 * @param transactionHash
 */
export const waitForOneConfirmation = (
    provider: ethers.providers.Provider,
    transactionHash: string
): Promise<ethers.providers.TransactionReceipt> =>
    provider.waitForTransaction(transactionHash, 1, MillisecondTimes.TenMinutes);
