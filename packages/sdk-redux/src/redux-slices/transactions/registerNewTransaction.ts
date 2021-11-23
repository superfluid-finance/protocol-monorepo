import { ThunkDispatch } from '@reduxjs/toolkit';

import { initializedContext } from '../../createSdkReduxParts';
import { trackTransaction, waitForOneConfirmation } from './trackTransaction';

// WARNING: Ethers TransactionResponse has initially wrong chain ID.
export const registerNewTransaction = async (
    chainId: number,
    transactionHash: string,
    waitForConfirmation: boolean,
    dispatch: ThunkDispatch<any, any, any>
) => {
    const framework = await initializedContext.getFramework(chainId);

    dispatch(
        trackTransaction({
            hash: transactionHash,
            chainId: chainId,
        })
    );

    if (waitForConfirmation) {
        await waitForOneConfirmation(
            framework.settings.provider,
            transactionHash
        );
    }
};
