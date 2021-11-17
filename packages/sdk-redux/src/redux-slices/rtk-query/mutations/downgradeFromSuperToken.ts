import { initializedSuperfluidSource } from '../../../superfluidApi';
import { MutationArg, TransactionInfo } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { invalidateTagsHandler } from '../invalidateTagsHandler';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type DowngradeFromSuperToken = MutationArg & {
    superToken: string;
    amount: string;
}

export const { useDowngradeFromSuperTokenMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        downgradeFromSuperToken: builder.mutation<TransactionInfo, DowngradeFromSuperToken>({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidSource.getFrameworkAndSigner(
                        arg.chainId
                    );

                const superToken = framework.loadSuperToken(arg.superToken);
                const transactionResponse = await superToken
                    .downgrade(arg)
                    .exec(signer);

                // Fire and forget
                queryApi.dispatch(
                    trackTransaction({
                        hash: transactionResponse.hash,
                        chainId: arg.chainId,
                    })
                );

                if (arg.waitForConfirmation) {
                    await framework.settings.provider.waitForTransaction(
                        transactionResponse.hash,
                        1,
                        60000
                    );
                }
                return {
                    data: {
                        hash: transactionResponse.hash,
                        chainId: arg.chainId,
                    },
                    meta: {
                        address: await signer.getAddress()
                    }
                };
            },
            onQueryStarted: async (arg, { dispatch, queryFulfilled }) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                const result = await queryFulfilled;

                // Should this be before "queryFulfilled"?
                framework.query.on(
                    (events, unsubscribe) => {
                        for (const event of events) {
                            invalidateTagsHandler(arg.chainId, event, dispatch);
                        }
                        unsubscribe();
                    },
                    2000,
                    result.meta!.signerAddress.toLowerCase(),
                    30000
                );
            },
        }),
    }),
    overrideExisting: false,
});
