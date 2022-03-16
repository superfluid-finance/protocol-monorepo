import {getFramework, getSigner} from '../../../sdkReduxConfig';
import {BaseSuperTokenMutation, NothingString, TransactionInfo} from '../../argTypes';
import {registerNewTransaction} from '../../transactions/registerNewTransaction';

import RpcApiEndpointBuilder from './rpcApiEndpointBuilder';

/**
 * Create a flow of the token of this class.
 */
export interface CreateFlow extends BaseSuperTokenMutation {
    /** The sender of the flow. Signer is used when left empty. */
    senderAddress?: string;
    /** The receiver of the flow. */
    receiverAddress: string;
    /** The specified flow rate. */
    flowRateWei: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Update a flow of the token of this class.
 */
export interface FlowUpdate extends BaseSuperTokenMutation {
    /** The sender of the flow. If not specified then signer address is used. */
    senderAddress?: string;
    /** The receiver of the flow. */
    receiverAddress: string;
    /** The specified flow rate. */
    flowRateWei: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Delete a flow of the token of this class.
 */
export interface FlowDelete extends BaseSuperTokenMutation {
    /** The sender of the flow. */
    senderAddress?: string;
    /** The receiver of the flow. */
    receiverAddress: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

export const createFlowMutationEndpoints = (builder: RpcApiEndpointBuilder) => ({
    flowCreate: builder.mutation<TransactionInfo, CreateFlow>({
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
    flowUpdate: builder.mutation<TransactionInfo, FlowUpdate>({
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
    flowDelete: builder.mutation<TransactionInfo, FlowDelete>({
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
