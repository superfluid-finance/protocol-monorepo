import {
    createAsyncThunk,
    createEntityAdapter,
    createSlice,
} from '@reduxjs/toolkit';
import { createApi } from '@reduxjs/toolkit/dist/query/react';
import request, { gql } from 'graphql-request';
import type { Transaction } from 'web3-core';

import { fakeBaseQuery } from './lib/fakeBaseQuery';

export type EthersTransaction = Transaction;

import {
    SuperfluidFrameworkSource,
    superfluidFrameworkSource as superfluidFrameworkSourcePreinitialized,
} from './superfluidFrameworkSource';

let superfluidFrameworkSource: SuperfluidFrameworkSource = null!;

export type TransactionId = {
    networkName: string;
    transactionHash: string;
};

export interface FlowDetails {
    timestamp: number;
    flowRate: string;
    deposit: string;
    owedDeposit: string;
}

export interface Flow {
    sender: string;
    receiver: string;
    flowRate: string;
    superToken: string;
}

export interface FetchFlowArg {
    networkName: string;
    superToken: string;
    sender: string;
    receiver: string;
}

export interface FetchFlowsArg {
    networkName: string;
    accountAddress: string;
}

export interface CreateFlowArg {
    networkName: string;
    superToken: string;
    sender: string;
    receiver: string;
    flowRate: string;
}

export interface UpdateFlowArg {
    networkName: string;
    superToken: string;
    sender: string;
    receiver: string;
    flowRate: string;
}

export interface DeleteFlowArg {
    networkName: string;
    superToken: string;
    sender: string;
    receiver: string;
}

export interface CreateOrUpdateOrDeleteFlowArg
    extends CreateFlowArg,
        UpdateFlowArg,
        DeleteFlowArg {}

interface Error {
    message: string;
}

const subgraphUrls: { [key: string]: string } = {
    goerli: 'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-goerli',
};

export enum TransactionStatus {
    Pending = 'Pending',
    Succeeded = 'Succeeded',
    Failed = 'Failed',
}

// "Redux" stuff needs to be serializable. Blockchain transaction object is unserializable.
export interface SuperfluidTransaction {
    networkName: string;
    hash: string;
    status: TransactionStatus;
    error?: string;
}

// Not strictly necessary to use: https://redux-toolkit.js.org/api/createEntityAdapter
const transactionsAdapter = createEntityAdapter<SuperfluidTransaction>({
    selectId: (transaction) => transaction.hash,
    sortComparer: (a, b) => a.hash.localeCompare(b.hash),
});

export const superfluidTransactionSelectors =
    transactionsAdapter.getSelectors();

// Having a single "track" action makes it easy to use transaction tracking logic.
const trackTransaction = createAsyncThunk<
    SuperfluidTransaction,
    TransactionId,
    { rejectValue: SuperfluidTransaction }
>('trackTransaction', async (arg, thunkAPI) => {
    const framework = await superfluidFrameworkSource.getForRead(
        arg.networkName
    );
    try {
        const transactionReceipt = await framework.ethers.waitForTransaction(
            arg.transactionHash,
            3,
            60000
        );
        if (transactionReceipt.status === 1) {
            return {
                networkName: arg.networkName,
                hash: arg.transactionHash,
                status: TransactionStatus.Succeeded,
            };
        } else {
            return thunkAPI.rejectWithValue({
                networkName: arg.networkName,
                hash: arg.transactionHash,
                status: TransactionStatus.Failed,
                error: 'Whatever error...',
            });
        }
    } catch (e) {
        return thunkAPI.rejectWithValue({
            networkName: arg.networkName,
            hash: arg.transactionHash,
            status: TransactionStatus.Failed,
            error: 'Whatever error...',
        });
    }
});

export const transactionSlice = createSlice({
    name: 'transactions',
    initialState: transactionsAdapter.getInitialState(),
    reducers: {
        upsertTransaction: transactionsAdapter.upsertOne,
    },
    extraReducers: (builder) => {
        builder
            .addCase(trackTransaction.pending, (state, action) => {
                transactionsAdapter.upsertOne(state, {
                    networkName: action.meta.arg.networkName,
                    hash: action.meta.arg.transactionHash,
                    status: TransactionStatus.Pending,
                });
            })
            .addCase(trackTransaction.fulfilled, (state, action) => {
                transactionsAdapter.upsertOne(state, action.payload);
            })
            .addCase(trackTransaction.rejected, (state, action) => {
                if (action.payload) {
                    transactionsAdapter.upsertOne(state, action.payload);
                } else {
                    throw Error("Haven't handled this use-case yet.");
                }
            });
    },
});

const reduxApiSlice = createApi({
    reducerPath: 'superfluidApi',
    baseQuery: fakeBaseQuery<Error>(),
    tagTypes: ['Flow'],
    endpoints: (builder) => ({
        fetchFlow: builder.query<FlowDetails, FetchFlowArg>({
            queryFn: async (arg) => {
                const framework = await superfluidFrameworkSource.getForRead(
                    arg.networkName
                );
                const flow = await framework.cfa!.getFlow({
                    superToken: arg.superToken,
                    sender: arg.sender,
                    receiver: arg.receiver,
                });
                return {
                    data: { ...flow, timestamp: flow.timestamp.getTime() },
                };
            },
            providesTags: (_1, _2, arg) => [
                {
                    type: 'Flow',
                    id: `${arg.networkName}_${arg.superToken}_${arg.sender}_${arg.receiver}`,
                },
            ],
        }),
        fetchFlows: builder.query<Flow[], FetchFlowsArg>({
            queryFn: async (arg) => {
                const subgraphResponse = await request<{
                    accountTokenSnapshots: {
                        token: {
                            id: string;
                        };
                    }[];
                }>(
                    subgraphUrls[arg.networkName],
                    gql`
                        query ($accountAddress: String) {
                            accountTokenSnapshots(
                                where: { account: $accountAddress }
                            ) {
                                token {
                                    id
                                }
                            }
                        }
                    `,
                    {
                        accountAddress: arg.accountAddress.toLowerCase(),
                    }
                );

                const framework = await superfluidFrameworkSource.getForRead(
                    arg.networkName
                );

                const userFlowsPromises: Promise<Flow[]>[] =
                    subgraphResponse.accountTokenSnapshots
                        .map((x) => x.token.id)
                        .map(async (tokenId) =>
                            framework
                                .cfa!.listFlows({
                                    superToken: tokenId,
                                    account: arg.accountAddress,
                                    onlyInFlows: false,
                                    onlyOutFlows: false,
                                })
                                .then((x) => [...x.inFlows, ...x.outFlows])
                                .then((flows) =>
                                    flows.map(
                                        (x) =>
                                            ({
                                                sender: x.sender,
                                                receiver: x.receiver,
                                                flowRate: x.flowRate,
                                                superToken: tokenId,
                                            } as Flow)
                                    )
                                )
                        );

                const allUserFlows = await Promise.all(userFlowsPromises).then(
                    (arrayOfArrays) => arrayOfArrays.flat()
                );

                return {
                    data: allUserFlows,
                };
            },
            providesTags: (_1, _2, arg) => [
                {
                    type: 'Flow',
                    id: `${arg.networkName}_${arg.accountAddress}`,
                },
            ],
        }),
        createFlow: builder.mutation<EthersTransaction, CreateFlowArg>({
            queryFn: async (arg, queryApi) => {
                const framework = await superfluidFrameworkSource.getForWrite(
                    arg.networkName
                );
                return {
                    data: await framework.cfa!.createFlow({
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
        updateFlow: builder.mutation<EthersTransaction, UpdateFlowArg>({
            queryFn: async (arg, queryApi) => {
                const framework = await superfluidFrameworkSource.getForWrite(
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
        deleteFlow: builder.mutation<EthersTransaction, DeleteFlowArg>({
            queryFn: async (arg, queryApi) => {
                const framework = await superfluidFrameworkSource.getForWrite(
                    arg.networkName
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
        createOrUpdateOrDeleteFlow: builder.mutation<
            EthersTransaction,
            CreateOrUpdateOrDeleteFlowArg
        >({
            queryFn: async (arg, queryApi) => {
                const framework = await superfluidFrameworkSource.getForWrite(
                    arg.networkName
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
                                        networkName: arg.networkName,
                                        transactionHash: transactionHash,
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
                                        networkName: arg.networkName,
                                        transactionHash: transactionHash,
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
});

export type SuperfluidReduxApiSliceType = typeof reduxApiSlice;
export type SuperfluidReduxTransactionSliceType = typeof transactionSlice;

export const createSuperfluidSlice = (
    frameworkSource?: SuperfluidFrameworkSource
): [
    superfluidFrameworkSource: SuperfluidFrameworkSource,
    superfluidApiSlice: SuperfluidReduxApiSliceType,
    superfluidTransactionSlice: SuperfluidReduxTransactionSliceType
] => {
    if (superfluidFrameworkSource) {
        throw Error("You shouldn't create the slice multiple times.");
    }

    if (frameworkSource) {
        superfluidFrameworkSource = frameworkSource;
    } else {
        superfluidFrameworkSource = superfluidFrameworkSourcePreinitialized;
    }

    return [superfluidFrameworkSource, reduxApiSlice, transactionSlice];
};

export const {
    useCreateFlowMutation,
    useUpdateFlowMutation,
    useDeleteFlowMutation,
    useCreateOrUpdateOrDeleteFlowMutation,
    useFetchFlowsQuery,
} = reduxApiSlice;
