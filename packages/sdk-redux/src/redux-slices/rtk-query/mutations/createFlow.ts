import SuperfluidSDK, { ChainId } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidFrameworkSource } from '../../../superfluidApi';
import { MutationArg } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface CreateFlowArg extends MutationArg {
    superToken: string;
    sender: string;
    receiver: string;
    flowRate: string;
    confirmations?: number;
}

export interface TransactionInfo {
    chainId: number;
    hash: string;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        createFlow: builder.mutation<TransactionInfo, CreateFlowArg>({
            queryFn: async (arg, queryApi) => {
                const framework =
                    await initializedSuperfluidFrameworkSource.getForWrite(
                        arg.chainId
                    );

                const superToken = new SuperfluidSDK.SuperToken({
                    chainId: arg.chainId as ChainId,
                    address: arg.superToken,
                });

                const transactionResponse = await superToken.createFlow({
                    sender: arg.sender,
                    receiver: arg.receiver,
                    flowRate: arg.flowRate,
                    userData: '0x',
                    signer: framework.ethers.getSigner() as any, // TODO
                });

                queryApi.dispatch(trackTransaction({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId
                }));

                await transactionResponse.wait(arg.confirmations);

                return {
                    data: {
                        hash: transactionResponse.hash,
                        chainId: arg.chainId,
                    },
                };
            },
            invalidatesTags: (_1, _2, arg) => [
                {
                    type: 'Flow',
                    id: `${arg.chainId}_${arg.sender}`,
                },
                {
                    type: 'Flow',
                    id: `${arg.chainId}_${arg.receiver}`,
                },
            ],
        }),
    }),
    overrideExisting: false,
});

export const { useCreateFlowMutation } = extendedApi;
