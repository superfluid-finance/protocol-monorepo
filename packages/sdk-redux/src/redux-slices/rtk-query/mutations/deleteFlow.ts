import { initializedSuperfluidSource } from '../../../superfluidApi';
import { MutationArg, TransactionInfo } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { invalidateTagsHandler } from '../invalidateTagsHandler';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { typeGuard } from '../../../utils';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type DeleteFlowArg = MutationArg & {
    superToken: string;
    sender: string;
    receiver: string;
};

export const { useDeleteFlowMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        deleteFlow: builder.mutation<TransactionInfo, DeleteFlowArg>({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidSource.getFrameworkAndSigner(
                        arg.chainId
                    );
                const superToken = await framework.loadSuperToken(
                    arg.superToken
                );
                const transactionResponse = await superToken
                    .deleteFlow({
                        sender: arg.sender,
                        receiver: arg.receiver,
                    })
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
                        observeAddress: arg.sender,
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
