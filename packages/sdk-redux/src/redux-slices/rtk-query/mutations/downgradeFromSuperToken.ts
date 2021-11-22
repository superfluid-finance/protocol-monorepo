import { initializedSuperfluidSource } from '../../../superfluidApi';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../baseArg';
import { monitorAddressEventsToInvalidateCache } from '../cacheTags/monitorAddressEventsToInvalidateCache';
import { registerNewTransaction } from '../registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type DowngradeFromSuperToken = SuperTokenMutationArg & {
    amountWei: string;
};

export const { useDowngradeFromSuperTokenMutation } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            downgradeFromSuperToken: builder.mutation<
                TransactionInfo,
                DowngradeFromSuperToken
            >({
                queryFn: async (arg, queryApi) => {
                    const [framework, signer] =
                        await initializedSuperfluidSource.getFrameworkAndSigner(
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
                            observeAddress: signerAddress,
                        }),
                    };
                },
                onQueryStarted: async (_arg, { dispatch, queryFulfilled }) => {
                    queryFulfilled.then(async (queryResult) =>
                        monitorAddressEventsToInvalidateCache(
                            queryResult.meta!.observeAddress,
                            queryResult.data,
                            dispatch
                        )
                    );
                },
            }),
        }),
        overrideExisting: false,
    });
