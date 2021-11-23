import { ThunkDispatch } from '@reduxjs/toolkit';

import { initializedSuperfluidSource } from '../../superfluidApi';
import {trackTransaction, waitForOneConfirmation} from './trackTransaction';

// WARNING: Ethers TransactionResponse has initially wrong chain ID.
export const registerNewTransaction = async (
    chainId: number,
    transactionHash: string,
    waitForConfirmation: boolean,
    dispatch: ThunkDispatch<any, any, any>
) => {
    const framework = await initializedSuperfluidSource.getFramework(chainId);

    dispatch(
        trackTransaction({
            hash: transactionHash,
            chainId: chainId,
        })
    );

    if (waitForConfirmation) {
        await waitForOneConfirmation(framework.settings.provider, transactionHash)
    }
};
