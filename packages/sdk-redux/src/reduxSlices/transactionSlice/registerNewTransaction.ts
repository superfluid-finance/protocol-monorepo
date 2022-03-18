import {ThunkDispatch} from '@reduxjs/toolkit';

import {getFramework} from '../../sdkReduxConfig';

import {trackTransaction, waitForOneConfirmation} from './trackTransaction';
import {ExecutedMutation} from './trackedTransaction';

/**
 * Transactions have to be registered for them to be tracked inside the redux store and monitored for re-orgs.
 */
export const registerNewTransaction = async (
    /**
     * The chain ID transaction is on.
     * WARNING: Don't pass `chainId` off of ether's `TransactionResponse` because it's not set correctly on timely manner.
     */
    chainId: number,
    transactionHash: string,
    /**
     * Whether to wait for one transaction confirmation.
     */
    waitForConfirmation: boolean,
    /**
     * For dispatching redux thunks.
     */
    dispatch: ThunkDispatch<any, any, any>,
    executedMutation?: ExecutedMutation
) => {
    const framework = await getFramework(chainId);

    dispatch(
        trackTransaction({
            chainId,
            hash: transactionHash,
            executedMutation,
        })
    );

    if (waitForConfirmation) {
        await waitForOneConfirmation(framework.settings.provider, transactionHash);
    }
};
