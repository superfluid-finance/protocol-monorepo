import {getFramework, getSigner} from '../../../../sdkReduxConfig';
import {TransactionInfo} from '../../../argTypes';
import {registerNewTransactionAndReturnQueryFnResult} from '../../../transactionTrackerSlice/registerNewTransaction';
import {RpcEndpointBuilder} from '../rpcEndpointBuilder';

import {
    IndexCreateMutation,
    IndexDeleteSubscriptionMutation,
    IndexDistributeMutation,
    IndexSubscriptionApproveMutation,
    IndexSubscriptionClaimMutation,
    IndexSubscriptionRevokeMutation,
    IndexUpdateSubscriptionUnitsMutation,
} from './indexArgs';

export const createIndexEndpoints = (builder: RpcEndpointBuilder) => ({
    indexCreate: builder.mutation<TransactionInfo, IndexCreateMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .createIndex({
                    indexId: arg.indexId,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                from: await signer.getAddress(),
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'INDEX_CREATE',
                extraData: arg.transactionExtraData,
            });
        },
    }),
    indexDistribute: builder.mutation<TransactionInfo, IndexDistributeMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .distribute({
                    indexId: arg.indexId,
                    userData: arg.userDataBytes,
                    amount: arg.amountWei,
                })
                .exec(signer);

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                from: await signer.getAddress(),
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'INDEX_DISTRIBUTE',
                extraData: arg.transactionExtraData,
            });
        },
    }),
    indexUpdateSubscriptionUnits: builder.mutation<TransactionInfo, IndexUpdateSubscriptionUnitsMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .updateSubscriptionUnits({
                    indexId: arg.indexId,
                    subscriber: arg.subscriberAddress,
                    units: arg.unitsNumber,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                from: await signer.getAddress(),
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'INDEX_UPDATE_SUBSCRIPTION_UNITS',
                extraData: arg.transactionExtraData,
            });
        },
    }),
    indexSubscriptionApprove: builder.mutation<TransactionInfo, IndexSubscriptionApproveMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .approveSubscription({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                from: await signer.getAddress(),
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'INDEX_SUBSCRIPTION_APPROVE',
                extraData: arg.transactionExtraData,
            });
        },
    }),
    indexSubscriptionClaim: builder.mutation<TransactionInfo, IndexSubscriptionClaimMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .claim({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    subscriber: arg.subscriberAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                from: await signer.getAddress(),
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'INDEX_SUBSCRIPTION_CLAIM',
                extraData: arg.transactionExtraData,
            });
        },
    }),
    indexDeleteSubscription: builder.mutation<TransactionInfo, IndexDeleteSubscriptionMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .deleteSubscription({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    subscriber: arg.subscriberAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                from: await signer.getAddress(),
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'INDEX_DELETE_SUBSCRIPTION',
                extraData: arg.transactionExtraData,
            });
        },
    }),
    indexSubscriptionRevoke: builder.mutation<TransactionInfo, IndexSubscriptionRevokeMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const transactionResponse = await superToken
                .revokeSubscription({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                from: await signer.getAddress(),
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'INDEX_SUBSCRIPTION_REVOKE',
                extraData: arg.transactionExtraData,
            });
        },
    }),
});
