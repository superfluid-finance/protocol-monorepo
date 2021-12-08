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
 * Updates the `units` allocated to a Subscription.
 * @param indexId The id of the index.
 * @param subscriberAddress The subscriber address whose units you want to update.
 * @param unitsNumber The amount of units you want to update to.
 * @param userDataBytes Extra user data provided.
 */
export type UpdateIndexSubscriptionUnitsArg = SuperTokenMutationArg & {
    subscriberAddress: string;
    indexId: string;
    unitsNumber: string;
    userDataBytes: string | NothingString;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        updateIndexSubscriptionUnits: builder.mutation<
            TransactionInfo,
            UpdateIndexSubscriptionUnitsArg
        >({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await getSfContext().getFrameworkAndSigner(arg.chainId);

                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );

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
                    data: typeGuard<TransactionInfo>({
                        hash: transactionResponse.hash,
                        chainId: arg.chainId,
                    }),
                    meta: typeGuard<MutationMeta>({
                        monitorAddress: arg.subscriberAddress,
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
    endpoints: { updateIndexSubscriptionUnits },
    useUpdateIndexSubscriptionUnitsMutation,
} = apiSlice;
