import {ethers} from 'ethers';

import {getFramework, getFrameworkAndSigner} from '../../../sdkReduxConfig';
import {MillisecondTimes, typeGuard} from '../../../utils';
import {TransactionInfo} from '../../argTypes';
import {registerNewTransaction} from '../../transactions/registerNewTransaction';
import {invalidateCacheTagsForEvents} from '../cacheTags/invalidateCacheTagsForEvents';
import {monitorAddressForNextEventToInvalidateCache} from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import {MutationMeta} from '../returnTypes';
import {ApiSliceEndpointBuilder} from '../sfApiSlice';

import {
    ApproveIndexSubscription,
    ClaimFromIndexSubscription,
    CreateFlow,
    CreateIndex,
    DeleteFlow,
    DeleteIndexSubscription,
    DistributeToIndex,
    DowngradeFromSuperToken,
    MonitorForEventsToInvalidateCache,
    RevokeIndexSubscription,
    TransferSuperToken,
    UpdateFlow,
    UpdateIndexSubscriptionUnits,
    UpgradeToSuperToken,
} from './mutations';

export const addMutationEndpoints = (builder: ApiSliceEndpointBuilder) => ({
    approveIndexSubscription: builder.mutation<TransactionInfo, ApproveIndexSubscription>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .approveSubscription({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: arg.publisherAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    claimFromIndexSubscription: builder.mutation<TransactionInfo, ClaimFromIndexSubscription>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .claim({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    subscriber: arg.subscriberAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: arg.publisherAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    createFlow: builder.mutation<TransactionInfo, CreateFlow>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const senderAddress = arg.senderAddress ? arg.senderAddress : await signer.getAddress();

            const transactionResponse = await superToken
                .createFlow({
                    sender: senderAddress,
                    receiver: arg.receiverAddress,
                    flowRate: arg.flowRateWei,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: senderAddress,
                }),
            };
        },
        // TODO(KK): Consider optimistic update.
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    createIndex: builder.mutation<TransactionInfo, CreateIndex>({
        /**
         * Creates an IDA Index.
         */
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const [superToken, signerAddress] = await Promise.all([
                framework.loadSuperToken(arg.superTokenAddress),
                signer.getAddress(),
            ]);
            const transactionResponse = await superToken
                .createIndex({
                    indexId: arg.indexId,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: signerAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    deleteFlow: builder.mutation<TransactionInfo, DeleteFlow>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const senderAddress = arg.senderAddress ? arg.senderAddress : await signer.getAddress();

            const transactionResponse = await superToken
                .deleteFlow({
                    sender: senderAddress,
                    receiver: arg.receiverAddress,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: senderAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    deleteIndexSubscription: builder.mutation<TransactionInfo, DeleteIndexSubscription>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .deleteSubscription({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    subscriber: arg.subscriberAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: arg.publisherAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    distributeToIndex: builder.mutation<TransactionInfo, DistributeToIndex>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const [superToken, signerAddress] = await Promise.all([
                framework.loadSuperToken(arg.superTokenAddress),
                signer.getAddress(),
            ]);

            const transactionResponse = await superToken
                .distribute({
                    indexId: arg.indexId,
                    userData: arg.userDataBytes,
                    amount: arg.amountWei,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: signerAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    downgradeFromSuperToken: builder.mutation<TransactionInfo, DowngradeFromSuperToken>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const [superToken, signerAddress] = await Promise.all([
                framework.loadSuperToken(arg.superTokenAddress),
                signer.getAddress(),
            ]);

            const transactionResponse = await superToken
                .downgrade({
                    amount: arg.amountWei,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: signerAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    monitorForEventsToInvalidateCache: builder.mutation<true, MonitorForEventsToInvalidateCache>({
        queryFn: () => {
            // No-op
            return {
                data: true,
            };
        },
        onCacheEntryAdded: async (arg, {dispatch, cacheDataLoaded, cacheEntryRemoved}) => {
            // TODO(KK): Consider how changing of networks inside the application can affect this.

            const framework = await getFramework(arg.chainId);

            await cacheDataLoaded;

            const unsubscribe = framework.query.on(
                (events) => {
                    invalidateCacheTagsForEvents(arg.chainId, events, dispatch);
                },
                MillisecondTimes.TwentySeconds,
                arg.address
            );

            try {
                await cacheEntryRemoved;
            } finally {
                unsubscribe();
            }
        },
    }),
    revokeIndexSubscription: builder.mutation<TransactionInfo, RevokeIndexSubscription>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .revokeSubscription({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: arg.publisherAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    transferSuperToken: builder.mutation<TransactionInfo, TransferSuperToken>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const [superToken, signerAddress] = await Promise.all([
                framework.loadSuperToken(arg.superTokenAddress),
                signer.getAddress(),
            ]);

            const transactionResponse = await superToken
                .transfer({
                    amount: arg.amountWei,
                    receiver: arg.receiverAddress,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: signerAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    updateFlow: builder.mutation<TransactionInfo, UpdateFlow>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);
            const senderAddress = arg.senderAddress ? arg.senderAddress : await signer.getAddress();
            const transactionResponse = await superToken
                .updateFlow({
                    sender: senderAddress,
                    receiver: arg.receiverAddress,
                    flowRate: arg.flowRateWei,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: senderAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    updateIndexSubscriptionUnits: builder.mutation<TransactionInfo, UpdateIndexSubscriptionUnits>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .updateSubscriptionUnits({
                    indexId: arg.indexId,
                    subscriber: arg.subscriberAddress,
                    units: arg.unitsNumber,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: arg.subscriberAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
    upgradeToSuperToken: builder.mutation<TransactionInfo, UpgradeToSuperToken>({
        queryFn: async (arg, queryApi) => {
            const [framework, signer] = await getFrameworkAndSigner(arg.chainId);

            const [superToken, signerAddress] = await Promise.all([
                framework.loadSuperToken(arg.superTokenAddress),
                signer.getAddress(),
            ]);

            const underlyingTokenAllowance = await superToken.underlyingToken
                .allowance({
                    providerOrSigner: framework.settings.provider,
                    owner: signerAddress,
                    spender: superToken.address,
                })
                .then((x) => ethers.BigNumber.from(x));

            const isAllowanceEnough = underlyingTokenAllowance.gte(ethers.BigNumber.from(arg.amountWei));
            if (!isAllowanceEnough) {
                const approveAllowanceTransactionResponse = await superToken.underlyingToken
                    .approve({
                        amount: arg.amountWei, // TODO(KK): Should we account for existing allowance amount here?
                        receiver: superToken.address,
                    })
                    .exec(signer);

                // NOTE: Always wait for transaction confirmation here.
                await registerNewTransaction(
                    arg.chainId,
                    approveAllowanceTransactionResponse.hash,
                    true,
                    queryApi.dispatch
                );
            }

            const upgradeToSuperTokenTransactionResponse = await superToken
                .upgrade({
                    amount: arg.amountWei,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                upgradeToSuperTokenTransactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch
            );

            return {
                data: typeGuard<TransactionInfo>({
                    hash: upgradeToSuperTokenTransactionResponse.hash,
                    chainId: arg.chainId,
                }),
                meta: typeGuard<MutationMeta>({
                    monitorAddress: signerAddress,
                }),
            };
        },
        onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
            queryFulfilled.then(async (queryResult) =>
                monitorAddressForNextEventToInvalidateCache(
                    queryResult.meta!.monitorAddress,
                    queryResult.data,
                    dispatch
                )
            );
        },
    }),
});
