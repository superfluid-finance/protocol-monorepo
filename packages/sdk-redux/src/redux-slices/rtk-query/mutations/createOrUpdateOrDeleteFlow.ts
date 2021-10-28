import { Transaction } from 'web3-core';

import { initializedSuperfluidFrameworkSource } from '../../../superfluidApi';
import { trackTransaction } from '../../transactions/transactionSlice';
import { rtkQuerySlice } from '../rtkQuerySlice';

import { CreateFlowArg } from './createFlow';
import { DeleteFlowArg } from './deleteFlow';
import { UpdateFlowArg } from './updateFlow';

export interface CreateOrUpdateOrDeleteFlowArg
    extends CreateFlowArg,
        UpdateFlowArg,
        DeleteFlowArg {}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        createOrUpdateOrDeleteFlow: builder.mutation<
            Transaction,
            CreateOrUpdateOrDeleteFlowArg
        >({
            queryFn: async (arg, queryApi) => {
                const framework =
                    await initializedSuperfluidFrameworkSource.getForWrite(
                        arg.chainId
                    );
                const cfa = framework.cfa!;

                if (arg.flowRate === '0') {
                    return {
                        data: await cfa.deleteFlow({
                            superToken: arg.superToken,
                            sender: arg.sender,
                            receiver: arg.receiver,
                            by: arg.sender, // What is this?
                            userData: undefined,
                            flowRate: arg.flowRate,
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
                }

                const existingFlow = await cfa.getFlow({
                    superToken: arg.superToken,
                    sender: arg.sender,
                    receiver: arg.receiver,
                });
                if (existingFlow.flowRate !== '0') {
                    return {
                        data: await cfa.updateFlow({
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
                                        chainId: arg.chainId,
                                        hash: transactionHash,
                                    })
                                );
                            },
                        }),
                    };
                }

                return {
                    data: await cfa.createFlow({
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

export const { useCreateOrUpdateOrDeleteFlowMutation } = extendedApi;
