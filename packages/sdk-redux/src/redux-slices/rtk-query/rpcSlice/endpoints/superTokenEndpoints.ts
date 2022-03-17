import {getFramework, getSigner} from '../../../../sdkReduxConfig';
import {TransactionInfo} from '../../../argTypes';
import {registerNewTransaction} from '../../../transactionSlice/registerNewTransaction';
import {RpcEndpointBuilder} from '../rpcEndpointBuilder';

import {SuperTokenDowngradeMutation, SuperTokenTransferMutation, SuperTokenUpgradeMutation} from './superTokenArgs';

export const createSuperTokenEndpoints = (builder: RpcEndpointBuilder) => ({
    superTokenUpgrade: builder.mutation<TransactionInfo, SuperTokenUpgradeMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

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
                data: {
                    hash: upgradeToSuperTokenTransactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
        },
    }),
    superTokenDowngrade: builder.mutation<TransactionInfo, SuperTokenDowngradeMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

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
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
        },
    }),
    superTokenTransfer: builder.mutation<TransactionInfo, SuperTokenTransferMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

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
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
        },
    }),
});
