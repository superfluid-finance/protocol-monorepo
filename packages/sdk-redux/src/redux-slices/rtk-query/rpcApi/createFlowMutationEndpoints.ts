import { getFramework, getSigner } from "../../../sdkReduxConfig";
import { BaseSuperTokenMutation, TransactionInfo } from "../../argTypes";
import { registerNewTransaction } from "../../transactions/registerNewTransaction";
import RpcApiEndpointBuilder from "./rpcApiEndpointBuilder";

// TODO(KK): User data in arg
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
}

// TODO(KK): user bytes
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
}

// TODO(KK): Add user data
/**
 * Delete a flow of the token of this class.
 */
 export interface DeleteFlow extends BaseSuperTokenMutation {
    /** The sender of the flow. */
    senderAddress?: string;
    /** The receiver of the flow. */
    receiverAddress: string;
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
    flowDelete: builder.mutation<TransactionInfo, DeleteFlow>({
        queryFn: async (arg, queryApi) => {
            const signer = await getSigner(arg.chainId);
            const framework = await getFramework(arg.chainId);
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
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
        },
    }),
 });