import { getFramework, getSigner } from "../../../sdkReduxConfig";
import { BaseSuperTokenMutation, NothingString, TransactionInfo } from "../../argTypes";
import { registerNewTransaction } from "../../transactions/registerNewTransaction";
import RpcApiEndpointBuilder from "./rpcApiEndpointBuilder";

/**
 * Creates an IDA Index.
 */
 export interface IndexCreate extends BaseSuperTokenMutation {
    /** The id of the index. */
    indexId: string;
    /** Extra user data provided. */
    userDataBytes: string;
}

/**
 * Updates the `units` allocated to a Subscription.
 */
 export interface IndexUpdateSubscriptionUnits extends BaseSuperTokenMutation {
    /** The subscriber address whose units you want to update. */
    subscriberAddress: string;
    /** The id of the index. */
    indexId: string;
    /** The amount of units you want to update to. */
    unitsNumber: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
 */
 export interface IndexDeleteSubscription extends BaseSuperTokenMutation {
    /** The id of the index. */
    indexId: string;
    /** The subscriber address whose subscription you want to delete. */
    publisherAddress: string;
    /** The publisher address of the index you are targeting. */
    subscriberAddress: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Distributes `amount` of token to an index
 */
export interface IndexDistribute extends BaseSuperTokenMutation {
    /** The id of the index. */
    indexId: string;
    /** The amount of tokens to be distributed. */
    amountWei: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
 */
 export interface IndexSubscriptionApprove extends BaseSuperTokenMutation {
    indexId: string;
    publisherAddress: string;
    userDataBytes: string | NothingString;
}

/**
 * Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributes.
 */
 export interface IndexSubscriptionRevoke extends BaseSuperTokenMutation {
    /** The id of the index. */
    indexId: string;
    /** The index publisher address you want to revoke for the subscriber. */
    publisherAddress: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Claims any pending tokens allocated to the Subscription (unapproved).
 */
 export interface IndexSubscriptionClaim extends BaseSuperTokenMutation {
    indexId: string;
    publisherAddress: string;
    subscriberAddress: string;
    userDataBytes: string | NothingString;
}

export const createIndexMutationEndpoints = (builder: RpcApiEndpointBuilder) => ({
    indexCreate: builder.mutation<TransactionInfo, IndexCreate>({
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
    indexDistribute: builder.mutation<TransactionInfo, IndexDistribute>({
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
    indexUpdateSubscriptionUnits: builder.mutation<TransactionInfo, IndexUpdateSubscriptionUnits>({
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
    indexSubscriptionApprove: builder.mutation<TransactionInfo, IndexSubscriptionApprove>({
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
    indexSubscriptionClaim: builder.mutation<TransactionInfo, IndexSubscriptionClaim>({
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
    indexDeleteSubscription: builder.mutation<TransactionInfo, IndexDeleteSubscription>({
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
    indexSubscriptionRevoke: builder.mutation<TransactionInfo, IndexSubscriptionRevoke>({
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
                queryApi.dispatch
            );

            return {
                data: {
                    hash: transactionResponse.hash,
                    chainId: arg.chainId,
                },
            };
        },
    })
 });