// Having a single "track" action makes it easy to use transaction tracking logic.
import {createAsyncThunk, Dispatch} from '@reduxjs/toolkit';
import {AllEvents, EventQueryHandler, PagedResult} from '@superfluid-finance/sdk-core';
import {ethers} from 'ethers';

import {getFramework, getRpcSlice, getSubgraphSlice, getTransactionSlice} from '../../sdkReduxConfig';
import {MillisecondTimes, retry} from '../../utils';
import {TransactionInfo} from '../argTypes';
import {invalidateCacheTagsForEvents} from '../rtkQuery/cacheTags/invalidateCacheTagsForEvents';

import {transactionSlicePrefix} from './createTransactionSlice';
import {ExecutedMutation} from './trackedTransaction';

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
export const trackTransaction = createAsyncThunk<void, TransactionInfo & {executedMutation?: ExecutedMutation}>(
    `${transactionSlicePrefix}/trackTransaction`,
    async (arg, {dispatch}) => {
        dispatch(
            getTransactionSlice().actions.addTransaction({
                chainId: arg.chainId,
                hash: arg.hash,
                status: 'Pending',
                ...(arg.executedMutation ? {executedMutation: arg.executedMutation} : {}),
            })
        );

        const framework = await getFramework(arg.chainId);

        waitForOneConfirmation(framework.settings.provider, arg.hash)
            .then(async (transactionReceipt: ethers.providers.TransactionReceipt) => {
                // When Ethers successfully returns then we assume the transaction was mined as per documentation: https://docs.ethers.io/v5/api/providers/provider/#Provider-waitForTransaction

                dispatch(
                    getTransactionSlice().actions.updateTransaction({
                        id: arg.hash,
                        changes: {
                            status: 'Succeeded',
                        },
                    })
                );

                dispatch(getRpcSlice().util.resetApiState());

                monitorForLateErrors(framework.settings.provider, arg, dispatch);

                retry<PagedResult<AllEvents>>(
                    () =>
                        new EventQueryHandler().list(framework.query.subgraphClient, {
                            block: {number: transactionReceipt.blockNumber},
                            pagination: {take: Infinity},
                        }),
                    10, // retry count
                    250 // retry interval
                ).then((subgraphEventsQueryResult) =>
                    invalidateCacheTagsForEvents(arg.chainId, subgraphEventsQueryResult.data, dispatch)
                );
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
                dispatch(getRpcSlice().util.resetApiState());
                dispatch(getSubgraphSlice().util.resetApiState());
                notifyOfError(ethersError, {chainId, hash: hash}, dispatch);
            }
        });
};

const notifyOfError = (ethersError: EthersError, {hash}: TransactionInfo, dispatch: Dispatch) => {
    dispatch(
        getTransactionSlice().actions.updateTransaction({
            id: hash,
            changes: {
                status: ethersError.code === ethers.errors.TIMEOUT ? 'Unknown' : 'Failed',
                ethersErrorCode: ethersError.code,
                ethersErrorMessage: ethersError.message,
            },
        })
    );
};
