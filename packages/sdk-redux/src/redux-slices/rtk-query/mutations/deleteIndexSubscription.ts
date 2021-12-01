import {initializedSuperfluidContext} from '../../../createSdkReduxParts';
import {typeGuard} from '../../../utils';
import {
    NothingString,
    SuperTokenMutationArg,
    TransactionInfo,
} from '../../argTypes';
import {registerNewTransaction} from '../../transactions/registerNewTransaction';
import {monitorAddressForNextEventToInvalidateCache} from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import {rtkQuerySlice} from '../rtkQuerySlice';
import {MutationMeta} from '../rtkQuerySliceBaseQuery';

/**
 * Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
 * @param indexId The id of the index.
 * @param subscriberAddress The subscriber address whose subscription you want to delete.
 * @param publisherAddress The publisher address of the index you are targeting.
 * @param userDataBytes Extra user data provided.
 */
export type DeleteIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    subscriberAddress: string;
    userDataBytes: string | NothingString;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        deleteIndexSubscription: builder.mutation<
            TransactionInfo,
            DeleteIndexSubscriptionArg
        >({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidContext.getFrameworkAndSigner(
                        arg.chainId
                    );

                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );

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
    }),
    overrideExisting: false,
});

export const {
    /**
     * Documentation: {@link DeleteIndexSubscriptionArg}
     * @category React Hooks
     */
    useDeleteIndexSubscriptionMutation,
} = apiSlice;
