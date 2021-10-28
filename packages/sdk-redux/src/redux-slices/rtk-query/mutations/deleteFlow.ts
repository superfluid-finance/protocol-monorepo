import { Transaction } from 'web3-core';

import { initializedSuperfluidFrameworkSource } from '../../../superfluidApi';
import { MutationArg } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface DeleteFlowArg extends MutationArg {
    superToken: string;
    sender: string;
    receiver: string;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        deleteFlow: builder.mutation<Transaction, DeleteFlowArg>({
            queryFn: async (arg, queryApi) => {
                const framework =
                    await initializedSuperfluidFrameworkSource.getForWrite(
                        arg.chainId
                    );
                return {
                    data: await framework.cfa!.deleteFlow({
                        superToken: arg.superToken,
                        sender: arg.sender,
                        receiver: arg.receiver,
                        by: arg.sender, // What is this?
                        userData: undefined,
                        flowRate: '0',
                        // eslint-disable-next-line @typescript-eslint/ban-ts-comment
                        // @ts-ignore
                        onTransaction: (transactionHash) => {
                            queryApi.dispatch(
                                trackTransaction({
                                    chainId: arg.chainId,
                                    hash: transactionHash,
                                })
                            );
                        },
                    }),
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

export const { useDeleteFlowMutation } = extendedApi;
