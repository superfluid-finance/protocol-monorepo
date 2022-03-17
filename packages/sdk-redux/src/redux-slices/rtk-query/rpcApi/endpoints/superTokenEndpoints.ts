import {getFramework, getSigner} from '../../../../sdkReduxConfig';
import {BaseSuperTokenMutation, TransactionInfo} from '../../../argTypes';
import {registerNewTransaction} from '../../../transactions/registerNewTransaction';
import RpcApiEndpointBuilder from '../rpcApiEndpointBuilder';

/**
 * Downgrade `amount` SuperToken's.
 */
export interface SuperTokenDowngrade extends BaseSuperTokenMutation {
    /** The amount to be downgraded. */
    amountWei: string;
}

/**
 * Upgrade `amount` SuperToken's.
 * NOTE: Initiates request for allowance if necessary.
 */
export interface SuperTokenUpgrade extends BaseSuperTokenMutation {
    /** The amount to be upgraded. */
    amountWei: string;
}

/**
 * Transfer `receiver` `amount` tokens.
 */
export interface SuperTokenTransfer extends BaseSuperTokenMutation {
    /** The receiver of the transfer. */
    receiverAddress: string;
    /** The amount to be transferred. */
    amountWei: string;
}

export const createSuperTokenEndpoints = (builder: RpcApiEndpointBuilder) => ({
    superTokenUpgrade: builder.mutation<TransactionInfo, SuperTokenUpgrade>({
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
    superTokenDowngrade: builder.mutation<TransactionInfo, SuperTokenDowngrade>({
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
    superTokenTransfer: builder.mutation<TransactionInfo, SuperTokenTransfer>({
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

// TODO(KK)

// query: (arg) => ({
//     ...arg,
//     handleMutation: (framework: Framework) => framework.loadSuperToken(arg.superTokenAddress).then(superToken
//         .approveSubscription({
//             indexId: arg.indexId,
//             publisher: arg.publisherAddress,
//             userData: arg.userDataBytes,
//         })
//         .exec(arg.signer))
// }),

// monitorForEventsToInvalidateCache: builder.mutation<true, MonitorForEventsToInvalidateCache>({
//     queryFn: () => {
//         // No-op
//         return {
//             data: true,
//         };
//     },
//     onCacheEntryAdded: async (arg, {dispatch, cacheDataLoaded, cacheEntryRemoved}) => {
//         // TODO(KK): Consider how changing of networks inside the application can affect this.

//         const framework = await getFramework(arg.chainId);

//         await cacheDataLoaded;

//         const unsubscribe = framework.query.on(
//             (events) => {
//                 invalidateCacheTagsForEvents(arg.chainId, events, dispatch);
//             },
//             MillisecondTimes.TwentySeconds,
//             arg.address
//         );

//         try {
//             await cacheEntryRemoved;
//         } finally {
//             unsubscribe();
//         }
//     },
// }),

// /**
//  * Continuously poll for new events to know when to invalidate cache for re-fetching of the data.
//  */
//  export interface MonitorForEventsToInvalidateCache {
//     /** The chain to poll. */
//     chainId: number;
//     /** The address (account or token) to filter events for. */
//     address: string | NothingString;
// }
