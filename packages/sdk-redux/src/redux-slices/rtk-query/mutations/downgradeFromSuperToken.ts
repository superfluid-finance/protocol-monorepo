import {initializedSuperfluidContext} from '../../../createSdkReduxParts';
import {typeGuard} from '../../../utils';
import {SuperTokenMutationArg, TransactionInfo} from '../../argTypes';
import {registerNewTransaction} from '../../transactions/registerNewTransaction';
import {monitorAddressForNextEventToInvalidateCache} from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import {rtkQuerySlice} from '../rtkQuerySlice';
import {MutationMeta} from '../rtkQuerySliceBaseQuery';

/**
 * Downgrade `amount` SuperToken's.
 * @param amountWei The amount to be downgraded.
 */
export type DowngradeFromSuperToken = SuperTokenMutationArg & {
    amountWei: string;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        downgradeFromSuperToken: builder.mutation<
            TransactionInfo,
            DowngradeFromSuperToken
        >({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidContext.getFrameworkAndSigner(
                        arg.chainId
                    );

                const [superToken, signerAddress] = await Promise.all([
                    framework.loadSuperToken(arg.superTokenAddress),
                    signer.getAddress(),
                ]);

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
                    data: typeGuard<TransactionInfo>({
                        hash: transactionResponse.hash,
                        chainId: arg.chainId,
                    }),
                    meta: typeGuard<MutationMeta>({
                        monitorAddress: signerAddress,
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
     * Documentation: {@link DowngradeFromSuperToken}
     * @category React Hooks
     */
    useDowngradeFromSuperTokenMutation,
} = apiSlice;
