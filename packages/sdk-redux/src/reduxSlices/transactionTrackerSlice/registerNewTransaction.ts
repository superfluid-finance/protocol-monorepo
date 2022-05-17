import {ThunkDispatch} from '@reduxjs/toolkit';
import {ethers} from 'ethers';

import {getFramework} from '../../sdkReduxConfig';

import {initiateNewTransactionTrackingThunk} from './thunks/initiateNewTransactionTrackingThunk';
import {TransactionTitle} from './transactionTitle';
import {waitForOneConfirmation} from './waitForOneConfirmation';

export interface RegisterNewTransactionArg {
    /**
     * The chain ID transaction is on.
     * WARNING: Don't pass `chainId` off of ether's `TransactionResponse` because it's not set correctly on timely manner.
     */
    chainId: number;
    signer: string;
    transactionResponse: ethers.providers.TransactionResponse;
    /**
     * Whether to wait for one transaction confirmation.
     */
    waitForConfirmation: boolean;
    /**
     * For dispatching redux thunks.
     */
    dispatch: ThunkDispatch<any, any, any>;
    /**
     * Any key you want to give the transaction to identify it later.
     */
    title: TransactionTitle;
    /**
     * Any extra data you want to attach to the transaction. Make sure it's serializable!
     */
    extraData: Record<string, unknown> | undefined;
}

/**
 * Transactions have to be registered for them to be tracked inside the redux store and monitored for re-orgs.
 */
export const registerNewTransaction = async (arg: RegisterNewTransactionArg) => {
    const {chainId, signer, transactionResponse, waitForConfirmation, dispatch, title, extraData} = arg;
    const framework = await getFramework(chainId);

    dispatch(
        initiateNewTransactionTrackingThunk({
            chainId,
            signer,
            transactionResponse,
            title,
            extraData: extraData ?? {},
        })
    );

    if (waitForConfirmation) {
        await waitForOneConfirmation(framework.settings.provider, transactionResponse.hash);
    }
};

export const registerNewTransactionAndReturnQueryFnResult = async (arg: RegisterNewTransactionArg) => {
    registerNewTransaction(arg);
    return {
        data: {
            hash: arg.transactionResponse.hash,
            chainId: arg.chainId,
        },
    };
};
