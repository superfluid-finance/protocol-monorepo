import {getFramework, getSigner} from '../../../../sdkReduxConfig';
import {TransactionInfo} from '../../../argTypes';
import {registerNewTransaction} from '../../../transactionSlice/registerNewTransaction';
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

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'INDEX_CREATE'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
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

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'INDEX_DISTRIBUTE'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
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

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'INDEX_UPDATE_SUBSCRIPTION_UNITS'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
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

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'INDEX_SUBSCRIPTION_APPROVE'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
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

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'INDEX_SUBSCRIPTION_CLAIM'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
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

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'INDEX_DELETE_SUBSCRIPTION'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
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

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'INDEX_SUBSCRIPTION_REVOKE'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
        },
    }),
});
