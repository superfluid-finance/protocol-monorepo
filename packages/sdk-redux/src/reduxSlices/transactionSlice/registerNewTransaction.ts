import {ThunkDispatch} from '@reduxjs/toolkit';

import {getFramework} from '../../sdkReduxConfig';

import {trackTransaction, waitForOneConfirmation} from './trackTransaction';

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
    /**
     * Any key you want to give the transaction to identify it later.
     */
    key: string,
    /**
     * Any extra data you want to attach to the transaction. Make sure it's serializable!
     */
    extra?: unknown
) => {
    const framework = await getFramework(chainId);

    dispatch(
        trackTransaction({
            chainId,
            hash: transactionHash,
            key,
            ...(extra ? {extra: extra} : {}),
        })
    );

    if (waitForConfirmation) {
        await waitForOneConfirmation(framework.settings.provider, transactionHash);
    }
};
