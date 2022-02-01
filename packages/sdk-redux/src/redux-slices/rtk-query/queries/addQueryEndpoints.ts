import {
    AllEvents,
    createSkipPaging,
    IIndex,
    IIndexSubscription,
    ILightAccountTokenSnapshot,
    IStream,
    ISuperToken,
    IWeb3Index,
    IWeb3Subscription,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {insertIf, typeGuard} from '../../../utils';
import {createEventTag} from '../cacheTags/eventTags';
import {getMostSpecificIndexTag} from '../cacheTags/indexTags';
import {getMostSpecificStreamTag} from '../cacheTags/streamTags';
import {getMostSpecificTokenTag} from '../cacheTags/tokenTags';
import {ApiSliceEndpointBuilder} from '../sfApiSlice';

import {
    GetAllowanceForUpgradeToSuperToken,
    GetIndex,
    GetIndexSubscriptions,
    GetRealtimeBalance,
    GetRealtimeBalanceResult,
    ListEvents,
    ListIndexes,
    ListIndexSubscriptions,
    ListStreams,
    ListSuperTokens,
    ListUserInteractedSuperTokens,
} from './queries';

export const addQueryEndpoints = (builder: ApiSliceEndpointBuilder) => ({
    getAllowanceForUpgradeToSuperToken: builder.query<string, GetAllowanceForUpgradeToSuperToken>({
        keepUnusedDataFor: 0, // We can't listen for "approval" event from Subgraph currently.
        providesTags: (_result, _error, arg) => [
            getMostSpecificTokenTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);

            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const underlyingTokenAllowance = await superToken.underlyingToken.allowance({
                providerOrSigner: framework.settings.provider,
                owner: arg.accountAddress,
                spender: superToken.address,
            });

            return {
                data: underlyingTokenAllowance,
            };
        },
    }),
    getIndex: builder.query<IWeb3Index, GetIndex>({
        providesTags: (_result, _error, arg) => [
            getMostSpecificIndexTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.publisherAddress,
                address3: arg.subscriberAddress,
                indexId: arg.indexId,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);
            const index = await superToken.getIndex({
                indexId: arg.indexId,
                publisher: arg.publisherAddress,
                providerOrSigner: framework.settings.provider,
            });
            return {
                data: index,
            };
        },
    }),
    getIndexSubscription: builder.query<IWeb3Subscription, GetIndexSubscriptions>({
        providesTags: (_result, _error, arg) => [
            getMostSpecificIndexTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.publisherAddress,
                address3: arg.subscriberAddress,
                indexId: arg.indexId,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);
            const indexSubscription = await superToken.getSubscription({
                indexId: arg.indexId,
                publisher: arg.publisherAddress,
                subscriber: arg.subscriberAddress,
                providerOrSigner: framework.settings.provider,
            });
            return {
                data: indexSubscription,
            };
        },
    }),
    getRealtimeBalance: builder.query<GetRealtimeBalanceResult, GetRealtimeBalance>({
        keepUnusedDataFor: 0, // We don't want to cache balance because it changes every second.
        providesTags: (_result, _error, arg) => [
            getMostSpecificIndexTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
                indexId: undefined,
            }),
            getMostSpecificStreamTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
            }),
            getMostSpecificTokenTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);
            const [realtimeBalance, netFlow] = await Promise.all([
                superToken.realtimeBalanceOf({
                    providerOrSigner: framework.settings.provider,
                    account: arg.accountAddress,
                    timestamp: arg.estimationTimestamp
                        ? arg.estimationTimestamp
                        : Math.floor(new Date().getTime() / 1000),
                }),
                superToken.getNetFlow({
                    account: arg.accountAddress,
                    providerOrSigner: framework.settings.provider,
                }),
            ]);
            return {
                data: typeGuard<GetRealtimeBalanceResult>({
                    availableBalanceWei: realtimeBalance.availableBalance.toString(),
                    depositWei: realtimeBalance.deposit.toString(),
                    owedDepositWei: realtimeBalance.owedDeposit.toString(),
                    timestamp: Math.floor(realtimeBalance.timestamp.getTime() / 1000),
                    netFlowRateWei: netFlow,
                }),
            };
        },
    }),
    listEvents: builder.query<PagedResult<AllEvents>, ListEvents>({
        providesTags: (_result, _error, arg) => [
            ...insertIf(!arg.accountAddress, createEventTag(arg.chainId)),
            getMostSpecificIndexTag({
                chainId: arg.chainId,
                address1: arg.accountAddress,
                address2: undefined,
                address3: undefined,
                indexId: undefined,
            }),
            getMostSpecificStreamTag({
                chainId: arg.chainId,
                address1: arg.accountAddress,
                address2: undefined,
                address3: undefined,
            }),
            getMostSpecificTokenTag({
                chainId: arg.chainId,
                address1: arg.accountAddress,
                address2: undefined,
                address3: undefined,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const pagedResult = await framework.query.listEvents(
                {
                    account: arg.accountAddress,
                    timestamp_gt: arg.timestamp_gt,
                },
                createSkipPaging({skip: arg.skip, take: arg.take})
            );
            return {
                data: pagedResult,
            };
        },
    }),
    listIndexes: builder.query<PagedResult<IIndex>, ListIndexes>({
        providesTags: (_result, _error, arg) => [
            getMostSpecificIndexTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.publisherAddress,
                address3: undefined,
                indexId: arg.indexId,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);

            return {
                data: await framework.query.listIndexes(
                    {
                        indexId: arg.indexId,
                        publisher: arg.publisherAddress,
                        token: arg.superTokenAddress,
                    },
                    createSkipPaging(arg)
                ),
            };
        },
    }),
    listIndexSubscriptions: builder.query<PagedResult<IIndexSubscription>, ListIndexSubscriptions>({
        providesTags: (_result, _error, arg) => [
            getMostSpecificIndexTag({
                chainId: arg.chainId,
                address1: arg.subscriberAddress,
                address2: undefined,
                address3: undefined,
                indexId: undefined,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);

            return {
                data: await framework.query.listIndexSubscriptions(
                    {
                        subscriber: arg.subscriberAddress,
                        approved: arg.approved,
                    },
                    createSkipPaging(arg)
                ),
            };
        },
    }),
    listStreams: builder.query<PagedResult<IStream>, ListStreams>({
        providesTags: (_result, _error, arg) => [
            getMostSpecificStreamTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address3: arg.receiverAddress,
                address2: arg.senderAddress,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);

            return {
                data: await framework.query.listStreams(
                    {
                        sender: arg.senderAddress,
                        receiver: arg.receiverAddress,
                        token: arg.superTokenAddress,
                    },
                    createSkipPaging(arg)
                ),
            };
        },
    }),
    listSuperTokens: builder.query<PagedResult<ISuperToken>, ListSuperTokens>({
        providesTags: (_result, _error, arg) => [
            getMostSpecificTokenTag({
                chainId: arg.chainId,
                address1: undefined,
                address2: undefined,
                address3: undefined,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);

            return {
                data: await framework.query.listAllSuperTokens(
                    {
                        isListed: arg.isListed,
                    },
                    createSkipPaging(arg)
                ),
            };
        },
    }),
    listUserInteractedSuperTokens: builder.query<
        PagedResult<ILightAccountTokenSnapshot>,
        ListUserInteractedSuperTokens
    >({
        providesTags: (_result, _error, arg) => [
            getMostSpecificTokenTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
            }),
            getMostSpecificIndexTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
                indexId: undefined,
            }),
            getMostSpecificStreamTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const pagedResult = await framework.query.listUserInteractedSuperTokens(
                {
                    token: arg.superTokenAddress,
                    account: arg.accountAddress,
                },
                createSkipPaging({skip: arg.skip, take: arg.take})
            );
            return {
                data: pagedResult,
            };
        },
    }),
});
