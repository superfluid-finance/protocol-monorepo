// Having a single "track" action makes it easy to use transaction tracking logic.
import {createAsyncThunk, Dispatch} from '@reduxjs/toolkit';
import {EventQueryHandler} from '@superfluid-finance/sdk-core';
import {ethers} from 'ethers';
import promiseRetry from 'promise-retry';

import {getFramework, getRpcApiSlice, getSubgraphApiSlice, getTransactionTrackerSlice} from '../../sdkReduxConfig';
import {MillisecondTimes} from '../../utils';
import {TransactionInfo} from '../argTypes';
import {invalidateCacheTagsForEvents} from '../rtkQuery/cacheTags/invalidateCacheTagsForEvents';

import {EthersError} from './ethersError';
import {TransactionKey} from './transactionKey';
import {transactionTrackerSlicePrefix} from './transactionTrackerSlice';

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
export const trackTransaction = createAsyncThunk<
    void,
    {
        chainId: number;
        transactionResponse: ethers.providers.TransactionResponse;
        from: string;
        key: TransactionKey;
        extra?: unknown;
    }
>(`${transactionTrackerSlicePrefix}/trackTransaction`, async (arg, {dispatch}) => {
    arg.transactionResponse.chainId = arg.chainId; // Recommended by Ethers to specify Chain ID when doing serialization.

    const transactionHash = arg.transactionResponse.hash;

    dispatch(
        getTransactionTrackerSlice().actions.addTransaction({
            chainId: arg.chainId,
            hash: transactionHash,
            from: ethers.utils.getAddress(arg.from),
            timestampMs: new Date().getTime(),
            status: 'Pending',
            transactionResponse: ethers.utils.serializeTransaction(arg.transactionResponse),
            key: arg.key,
            ...(arg.extra ? {extra: arg.extra} : {}),
        })
    );

    const framework = await getFramework(arg.chainId);

    waitForOneConfirmation(framework.settings.provider, transactionHash)
        .then(async (transactionReceipt: ethers.providers.TransactionReceipt) => {
            // When Ethers successfully returns then we assume the transaction was mined as per documentation: https://docs.ethers.io/v5/api/providers/provider/#Provider-waitForTransaction

            dispatch(
                getTransactionTrackerSlice().actions.updateTransaction({
                    id: transactionHash,
                    changes: {
                        status: 'Succeeded',
                        transactionReceipt: ethers.utils.serializeTransaction(transactionReceipt),
                    },
                })
            );

            dispatch(getRpcApiSlice().util.resetApiState());

            monitorForLateErrors(framework.settings.provider, {chainId: arg.chainId, hash: transactionHash}, dispatch);

            // Poll Subgraph for all the events for this block and then invalidate Subgraph cache based on that.
            promiseRetry(
                (retry, _number) =>
                    new EventQueryHandler()
                        .list(framework.query.subgraphClient, {
                            block: {number: transactionReceipt.blockNumber}, // Subgraph returns error when not indexed this far.
                            filter: {blockNumber: transactionReceipt.blockNumber.toString()}, // Only return events for this block.
                            pagination: {take: Infinity},
                        })
                        .catch(retry),
                {
                    minTimeout: 500,
                    factor: 2,
                    forever: true,
                }
            ).then((subgraphEventsQueryResult) => {
                invalidateCacheTagsForEvents(arg.chainId, subgraphEventsQueryResult.data, dispatch);
            });
        })
        .catch((ethersError: EthersError) => {
            notifyOfError(ethersError, {chainId: arg.chainId, hash: transactionHash}, dispatch);
        });
});

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
                dispatch(getRpcApiSlice().util.resetApiState());
                dispatch(getSubgraphApiSlice().util.resetApiState());
                notifyOfError(ethersError, {chainId, hash: hash}, dispatch);
            }
        });
};

const notifyOfError = (ethersError: EthersError, {hash}: TransactionInfo, dispatch: Dispatch) => {
    dispatch(
        getTransactionTrackerSlice().actions.updateTransaction({
            id: hash,
            changes: {
                status: ethersError.code === ethers.errors.TIMEOUT ? 'Unknown' : 'Failed',
                ethersErrorCode: ethersError.code,
                ethersErrorMessage: ethersError.message,
            },
        })
    );
};
