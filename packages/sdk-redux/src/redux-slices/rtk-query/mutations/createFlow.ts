import SuperfluidSDK, { NetworkName } from '@superfluid-finance/sdk-core';
import { ethers } from 'ethers';

import { initializedSuperfluidFrameworkSource } from '../../../superfluidApi';
import { trackTransaction } from '../../transactions/transactionSlice';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface CreateFlowArg {
    networkName: NetworkName;
    superToken: string;
    sender: string;
    receiver: string;
    flowRate: string;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        createFlow: builder.mutation<ethers.Transaction, CreateFlowArg>({
            queryFn: async (arg, queryApi) => {
                const framework =
                    await initializedSuperfluidFrameworkSource.getForWrite(
                        arg.networkName
                    );

                const superToken = new SuperfluidSDK.SuperToken({
                    networkName: arg.networkName,
                    address: arg.superToken,
                });

                const transaction = await superToken.createFlow({
                    sender: arg.sender,
                    receiver: arg.receiver,
                    flowRate: arg.flowRate,
                    userData: '0x',
                    signer: framework.ethers.getSigner() as any, // TODO
                });

                queryApi.dispatch(
                    trackTransaction({
                        networkName: arg.networkName,
                        transactionHash: transaction.hash,
                    })
                );

                await framework.ethers.waitForTransaction(
                    transaction.hash,
                    1,
                    60000
                );

                return {
                    data: transaction,
                };
            },
            invalidatesTags: (_1, _2, arg) => [
                {
                    type: 'Flow',
                    id: `${arg.networkName}_${arg.sender}`,
                },
                {
                    type: 'Flow',
                    id: `${arg.networkName}_${arg.receiver}`,
                },
            ],
        }),
    }),
    overrideExisting: false,
});

export const { useCreateFlowMutation } = extendedApi;
