import { initializedSuperfluidSource } from '../../../superfluidApi';
import { MutationArg, TransactionInfo } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { invalidateTagsHandler } from '../invalidateTagsHandler';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { typeGuard } from '../../../utils';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type TransferSuperTokenArg = MutationArg & {
    superToken: string;
    receiver: string;
    amount: string;
};

export const { useTransferSuperTokenMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        transferSuperToken: builder.mutation<
            TransactionInfo,
            TransferSuperTokenArg
        >({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidSource.getFrameworkAndSigner(
                        arg.chainId
                    );

                const [superToken, signerAddress] = await Promise.all([
                    framework.loadSuperToken(arg.superToken),
                    signer.getAddress(),
                ]);

                const transactionResponse = await superToken
                    .transfer(arg)
                    .exec(signer);

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
                    data: typeGuard<TransactionInfo>({
                        hash: transactionResponse.hash,
                        chainId: arg.chainId,
                    }),
                    meta: typeGuard<MutationMeta>({
                        observeAddress: signerAddress,
                    }),
                };
            },
            onQueryStarted: async (arg, { dispatch, queryFulfilled }) => {
                queryFulfilled.then(async (queryResult) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );
                    framework.query.on(
                        (events, unsubscribe) => {
                            for (const event of events) {
                                invalidateTagsHandler(
                                    arg.chainId,
                                    event,
                                    dispatch
                                );
                            }
                            unsubscribe();
                        },
                        2000,
                        queryResult.meta!.observeAddress,
                        30000
                    );
                });
            },
        }),
    }),
    overrideExisting: false,
});
