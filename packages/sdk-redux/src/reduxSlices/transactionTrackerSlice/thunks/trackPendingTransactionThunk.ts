// Having a single "track" action makes it easy to use transaction tracking logic.
import {createAsyncThunk, Dispatch} from '@reduxjs/toolkit';
import {EventQueryHandler} from '@superfluid-finance/sdk-core';
import {ethers} from 'ethers';
import promiseRetry from 'promise-retry';

import {getFramework, getRpcApiSlice, getSubgraphApiSlice, getTransactionTrackerSlice} from '../../../sdkReduxConfig';
import {MillisecondTimes} from '../../../utils';
import {TransactionInfo} from '../../argTypes';
import {createGeneralTags} from '../../rtkQuery/cacheTags/CacheTagTypes';
import {EthersError} from '../ethersError';
import {transactionTrackerSelectors} from '../transactionTrackerAdapter';
import {TransactionTrackerReducer, transactionTrackerSlicePrefix} from '../transactionTrackerSlice';
import {trySerializeTransaction} from '../trySerializeTransaction';
import {waitForOneConfirmation} from '../waitForOneConfirmation';

/**
 * Used for tracking a transaction that has already been put into the redux store.
 */
export const trackPendingTransactionThunk = createAsyncThunk<
    void,
    {
        chainId: number;
        transactionHash: string;
    }
>(`${transactionTrackerSlicePrefix}/trackPendingTransaction`, async (arg, {getState, dispatch}) => {
    const transactionHash = arg.transactionHash;
    const state = getState() as {[transactionTrackerSlicePrefix]: TransactionTrackerReducer};

    const transaction = transactionTrackerSelectors.selectById(state, transactionHash);
    if (!transaction) {
        throw new Error(`Transaction [${transactionHash}] not found in store.`);
    }

    const framework = await getFramework(arg.chainId);

    await waitForOneConfirmation(framework.settings.provider, transactionHash)
        .then(async (transactionReceipt: ethers.providers.TransactionReceipt) => {
            // When Ethers successfully returns then we assume the transaction was mined as per documentation: https://docs.ethers.io/v5/api/providers/provider/#Provider-waitForTransaction

            const {updateTransaction} = getTransactionTrackerSlice().actions;
            dispatch(
                updateTransaction({
                    id: transactionHash,
                    changes: {
                        status: 'Succeeded',
                        transactionReceipt: trySerializeTransaction(transactionReceipt),
                    },
                })
            );

            dispatch(getRpcApiSlice().util.invalidateTags(createGeneralTags({chainId: arg.chainId})));

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
            ).then((_subgraphEventsQueryResult) => {
                dispatch(getSubgraphApiSlice().util.invalidateTags(createGeneralTags({chainId: arg.chainId})));
                dispatch(
                    updateTransaction({
                        id: transactionHash,
                        changes: {
                            isSubgraphInSync: true,
                        },
                    })
                );
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
                dispatch(getRpcApiSlice().util.invalidateTags(createGeneralTags({chainId})));
                dispatch(getSubgraphApiSlice().util.invalidateTags(createGeneralTags({chainId})));
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
