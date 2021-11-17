import { ThunkDispatch } from '@reduxjs/toolkit';

import { initializedSuperfluidSource } from '../../superfluidApi';
import { trackTransaction } from '../transactions/transactionSlice';

// WARNING: Ethers TransactionResponse has initially wrong chain ID.
export const registerNewTransaction = async (
    chainId: number,
    transactionHash: string,
    waitForConfirmation: boolean,
    dispatch: ThunkDispatch<any, any, any>
) => {
    const framework = await initializedSuperfluidSource.getFramework(chainId);

    // Fire and forget
    dispatch(
        trackTransaction({
            hash: transactionHash,
            chainId: chainId,
        })
    );

    if (waitForConfirmation) {
        await framework.settings.provider.waitForTransaction(
            transactionHash,
            1,
            60000
        );
    }
};
