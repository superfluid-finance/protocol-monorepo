import { initializedSuperfluidSource } from '../../../superfluidApi';
import { MutationArg, TransactionInfo } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { rtkQuerySlice } from '../rtkQuerySlice';
import {invalidateTagsHandler} from "../invalidateTagsHandler";

export type UpdateFlowArg = MutationArg & {
    superToken: string;
    sender: string;
    receiver: string;
    flowRate: string;
}

export const { useUpdateFlowMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        updateFlow: builder.mutation<TransactionInfo, UpdateFlowArg>({
            queryFn: async (arg, api) => {
                const [framework, signer] =
                    await initializedSuperfluidSource.getFrameworkAndSigner(
                        arg.chainId
                    );
                const superToken = framework.loadSuperToken(arg.superToken);
                const transactionResponse = await superToken
                    .updateFlow({
                        sender: arg.sender,
                        receiver: arg.receiver,
                        flowRate: arg.flowRate,
                    }).exec(signer as any); // TODO(KK): "as any"
                api.dispatch(
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
                        chainId: arg.chainId,
                        hash: transactionResponse.hash,
                    },
                };
            },
            onQueryStarted: async (arg, { dispatch, queryFulfilled }) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                await queryFulfilled;

                framework.query.on(
                    (events, unsubscribe) => {
                        for (const event of events) {
                            invalidateTagsHandler(arg.chainId, event, dispatch);
                        }
                        unsubscribe();
                    },
                    2000,
                    arg.sender.toLowerCase(),
                    30000
                );
            },
        }),
    }),
    overrideExisting: false,
});
