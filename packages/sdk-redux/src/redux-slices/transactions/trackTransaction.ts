// Having a single "track" action makes it easy to use transaction tracking logic.
import {createAsyncThunk, Dispatch} from '@reduxjs/toolkit';
import {ethers} from 'ethers';

import {getApiSlice, getFramework, getSubgraphSlice, getTransactionSlice} from '../../sdkReduxConfig';
import {MillisecondTimes} from '../../utils';
import {TransactionInfo} from '../argTypes';

import {transactionSlicePrefix} from './createTransactionSlice';

/**
 *
 */
type EthersError = Error & {
    code: ethers.errors;
    reason?: 'replaced' | 'repriced' | 'cancelled';
    cancelled?: boolean;
};

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

/**
 *
 */
export const trackTransaction = createAsyncThunk<void, TransactionInfo>(
    `${transactionSlicePrefix}/trackTransaction`,
    async (arg, {dispatch}) => {
        dispatch(
            getTransactionSlice().actions.upsertTransaction({
                chainId: arg.chainId,
                hash: arg.hash,
                status: 'Pending',
            })
        );

        const framework = await getFramework(arg.chainId);

        waitForOneConfirmation(framework.settings.provider, arg.hash)
            .then((_transactionReceipt: ethers.providers.TransactionReceipt) => {
                // When Ethers successfully returns then we assume the transaction was mined as per documentation: https://docs.ethers.io/v5/api/providers/provider/#Provider-waitForTransaction

                dispatch(
                    getTransactionSlice().actions.upsertTransaction({
                        chainId: arg.chainId,
                        hash: arg.hash,
                        status: 'Succeeded',
                    })
                );

                monitorForLateErrors(framework.settings.provider, arg, dispatch);
            })
            .catch((ethersError: EthersError) => {
                notifyOfError(ethersError, arg, dispatch);
            });
    }
);

// i.e. monitor for re-orgs...
const monitorForLateErrors = (
    provider: ethers.providers.Provider,
    {chainId, hash}: TransactionInfo,
    dispatch: Dispatch
) => {
    provider
        .waitForTransaction(hash, 5, MillisecondTimes.TenMinutes)
        // When there's no error then that means a re-org didn't happen.
        .catch((ethersError: EthersError) => {
            if (ethersError.code != ethers.errors.TIMEOUT) {
                // Completely reset API cache.
                dispatch(getApiSlice().util.resetApiState());
                dispatch(getSubgraphSlice().util.resetApiState());
                notifyOfError(ethersError, {chainId, hash: hash}, dispatch);
            }
        });
};

const notifyOfError = (ethersError: EthersError, {chainId, hash}: TransactionInfo, dispatch: Dispatch) => {
    dispatch(
        getTransactionSlice().actions.upsertTransaction({
            chainId: chainId,
            hash: hash,
            status: ethersError.code === ethers.errors.TIMEOUT ? 'Unknown' : 'Failed',
            ethersErrorCode: ethersError.code,
            ethersErrorMessage: ethersError.message,
        })
    );
};
