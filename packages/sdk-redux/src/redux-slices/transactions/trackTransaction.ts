// Having a single "track" action makes it easy to use transaction tracking logic.
import { createAsyncThunk, Dispatch } from '@reduxjs/toolkit';
import { superfluidSource } from '../../superfluidSource';
import { MsTimes } from '../../utils';
import { rtkQuerySlice } from '../rtk-query/rtkQuerySlice';
import { transactionSlice } from './transactionSlice';
import { ethers } from 'ethers';

type EthersError = Error & {
    code: ethers.errors;
    reason?: 'replaced' | 'repriced' | 'cancelled';
    cancelled?: boolean;
};

export interface TrackTransactionArg {
    chainId: number;
    hash: string;
}

export const waitForOneConfirmation = (
    provider: ethers.providers.Provider,
    transactionHash: string
): Promise<ethers.providers.TransactionReceipt> =>
    provider.waitForTransaction(transactionHash, 1, MsTimes.TenMinutes);

export const trackTransaction = createAsyncThunk<void, TrackTransactionArg>(
    'trackTransaction',
    async (arg, { dispatch }) => {
        dispatch(
            transactionSlice.actions.upsertTransaction({
                chainId: arg.chainId,
                hash: arg.hash,
                status: 'Pending',
            })
        );

        const framework = await superfluidSource.getFramework(arg.chainId);

        waitForOneConfirmation(framework.settings.provider, arg.hash)
            .then(
                (_transactionReceipt: ethers.providers.TransactionReceipt) => {
                    // When Ethers successfully returns then we assume the transaction was mined as per documentation: https://docs.ethers.io/v5/api/providers/provider/#Provider-waitForTransaction

                    dispatch(
                        transactionSlice.actions.upsertTransaction({
                            chainId: arg.chainId,
                            hash: arg.hash,
                            status: 'Succeeded',
                        })
                    );

                    monitorForLateErrors(
                        framework.settings.provider,
                        arg,
                        dispatch
                    );
                }
            )
            .catch((ethersError: EthersError) => {
                notifyOfError(ethersError, arg, dispatch);
            });
    }
);

// i.e. monitor for re-orgs...
const monitorForLateErrors = (
    provider: ethers.providers.Provider,
    { chainId, hash }: TrackTransactionArg,
    dispatch: Dispatch
) => {
    provider
        .waitForTransaction(hash, 5, MsTimes.TenMinutes)
        .then((_transactionReceipt: ethers.providers.TransactionReceipt) => {
            // No-op: re-org didn't happen.
        })
        .catch((ethersError: EthersError) => {
            if (ethersError.code != ethers.errors.TIMEOUT) {
                // Completely reset API cache.
                dispatch(rtkQuerySlice.util.resetApiState());
                notifyOfError(ethersError, { chainId, hash }, dispatch);
            }
        });
};

const notifyOfError = (
    ethersError: EthersError,
    { chainId, hash }: TrackTransactionArg,
    dispatch: Dispatch
) => {
    dispatch(
        transactionSlice.actions.upsertTransaction({
            chainId: chainId,
            hash: hash,
            status:
                ethersError.code === ethers.errors.TIMEOUT
                    ? 'Unknown'
                    : 'Failed',
            ethersErrorCode: ethersError.code,
            ethersErrorMessage: ethersError.message,
        })
    );
};
