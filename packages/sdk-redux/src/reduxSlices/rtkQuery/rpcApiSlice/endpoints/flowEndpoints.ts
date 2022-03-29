import {getFramework, getSigner} from '../../../../sdkReduxConfig';
import {TransactionInfo} from '../../../argTypes';
import {registerNewTransaction} from '../../../transactionSlice/registerNewTransaction';
import {RpcEndpointBuilder} from '../rpcEndpointBuilder';

import {FlowCreateMutation, FlowDeleteMutation, FlowUpdateMutation} from './flowArgs';

export const createFlowEndpoints = (builder: RpcEndpointBuilder) => ({
    flowCreate: builder.mutation<TransactionInfo, FlowCreateMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const senderAddress = arg.senderAddress ? arg.senderAddress : await signer.getAddress();

            const transactionResponse = await superToken
                .createFlow({
                    sender: senderAddress,
                    receiver: arg.receiverAddress,
                    flowRate: arg.flowRateWei,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'FLOW_CREATE'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
        },
    }),
    flowUpdate: builder.mutation<TransactionInfo, FlowUpdateMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);
            const senderAddress = arg.senderAddress ? arg.senderAddress : await signer.getAddress();

            const transactionResponse = await superToken
                .updateFlow({
                    sender: senderAddress,
                    receiver: arg.receiverAddress,
                    flowRate: arg.flowRateWei,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'FLOW_UPDATE'
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
        },
    }),
    flowDelete: builder.mutation<TransactionInfo, FlowDeleteMutation>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const senderAddress = arg.senderAddress ? arg.senderAddress : await signer.getAddress();

            const transactionResponse = await superToken
                .deleteFlow({
                    sender: senderAddress,
                    receiver: arg.receiverAddress,
                    userData: arg.userDataBytes,
                })
                .exec(signer);

            await registerNewTransaction(
                arg.chainId,
                transactionResponse.hash,
                !!arg.waitForConfirmation,
                queryApi.dispatch,
                'FLOW_DELETE'
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
