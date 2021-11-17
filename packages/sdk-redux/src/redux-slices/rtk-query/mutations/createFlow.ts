import { initializedSuperfluidSource } from '../../../superfluidApi';
import { MutationArg, TransactionInfo } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { invalidateTagsHandler } from '../invalidateTagsHandler';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type CreateFlowArg = MutationArg & {
    superToken: string;
    sender: string;
    receiver: string;
    flowRate: string;
};

export const { useCreateFlowMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        createFlow: builder.mutation<TransactionInfo, CreateFlowArg>({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidSource.getFrameworkAndSigner(
                        arg.chainId
                    );

                const superToken = await framework.loadSuperToken(arg.superToken);
                const transactionResponse = await superToken
                    .createFlow({
                        sender: arg.sender,
                        receiver: arg.receiver,
                        flowRate: arg.flowRate,
                    })
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
                };
            },
            onQueryStarted: async (arg, { dispatch, queryFulfilled }) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                await queryFulfilled;

                // Should this be before "queryFulfilled"?
                framework.query.on(
                    (events, unsubscribe) => {
                        console.log('boom!');
                        for (const event of events) {
                            invalidateTagsHandler(arg.chainId, event, dispatch);
                        }
                        unsubscribe();
                    },
                    2000,
                    arg.sender.toLowerCase(),
                    30000
                );

                // TODO: Consider optimistic update.

                // TODO: Subscribe to re-org issues here or at "track transaction"?
            },
        }),
    }),
    overrideExisting: false,
});
