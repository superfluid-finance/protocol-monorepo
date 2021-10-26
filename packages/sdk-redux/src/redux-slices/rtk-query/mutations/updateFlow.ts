import { Transaction } from 'web3-core';

import { initializedSuperfluidFrameworkSource } from '../../../superfluidApi';
import { trackTransaction } from '../../transactions/transactionSlice';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface UpdateFlowArg {
    networkName: string;
    superToken: string;
    sender: string;
    receiver: string;
    flowRate: string;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        updateFlow: builder.mutation<Transaction, UpdateFlowArg>({
            queryFn: async (arg, queryApi) => {
                const framework =
                    await initializedSuperfluidFrameworkSource.getForWrite(
                        arg.networkName
                    );
                return {
                    data: await framework.cfa!.updateFlow({
                        superToken: arg.superToken,
                        sender: arg.sender,
                        receiver: arg.receiver,
                        flowRate: arg.flowRate,
                        userData: undefined,
                        // eslint-disable-next-line @typescript-eslint/ban-ts-comment
                        // @ts-ignore
                        onTransaction: (transactionHash) => {
                            queryApi.dispatch(
                                trackTransaction({
                                    networkName: arg.networkName,
                                    transactionHash: transactionHash,
                                })
                            );
                        },
                    }),
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

export const { useUpdateFlowMutation } = extendedApi;
