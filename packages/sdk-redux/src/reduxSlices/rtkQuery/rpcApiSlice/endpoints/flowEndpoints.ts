import {getFramework, getSigner} from '../../../../sdkReduxConfig';
import {TransactionInfo} from '../../../argTypes';
import {registerNewTransactionAndReturnQueryFnResult} from '../../../transactionTrackerSlice/registerNewTransaction';
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

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                signer: senderAddress,
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'FLOW_CREATE',
                extraData: arg.transactionExtraData,
            });
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

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                signer: senderAddress,
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'FLOW_UPDATE',
                extraData: arg.transactionExtraData,
            });
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

            return await registerNewTransactionAndReturnQueryFnResult({
                transactionResponse,
                chainId: arg.chainId,
                signer: senderAddress,
                waitForConfirmation: !!arg.waitForConfirmation,
                dispatch: queryApi.dispatch,
                key: 'FLOW_DELETE',
                extraData: arg.transactionExtraData,
            });
        },
    }),
});
