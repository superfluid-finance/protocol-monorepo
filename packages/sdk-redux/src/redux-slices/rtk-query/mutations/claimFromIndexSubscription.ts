import {getSfContext} from '../../../createSdkReduxParts';
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
 * Claims any pending tokens allocated to the Subscription (unapproved).
 * @param indexId The id of the index.
 * @param subscriberAddress The subscriber address who you are claiming for.
 * @param publisherAddress The publisher address of the index you are targeting.
 * @param userDataBytes Extra user data provided.
 */
export type ClaimFromIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    subscriberAddress: string;
    userDataBytes: string | NothingString;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        claimFromIndexSubscription: builder.mutation<
            TransactionInfo,
            ClaimFromIndexSubscriptionArg
        >({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await getSfContext().getFrameworkAndSigner(arg.chainId);

                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );

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
     * Documentation: {@link ClaimFromIndexSubscriptionArg}
     * @category React Hooks
     */
    useClaimFromIndexSubscriptionMutation,
} = apiSlice;
