import { initializedSuperfluidSource } from '../../../superfluidApi';
import { MutationArg, TransactionInfo } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { invalidateTagsHandler } from '../invalidateTagsHandler';
import { rtkQuerySlice } from '../rtkQuerySlice';
import {ITransferFromParams} from "@superfluid-finance/sdk-core";

export type TransferSuperTokenArg = MutationArg & ITransferFromParams & {
    superToken: string;
}

export const { useTransferSuperTokenMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        transferSuperToken: builder.mutation<TransactionInfo, TransferSuperTokenArg>({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidSource.getFrameworkAndSigner(
                        arg.chainId
                    );

                const superToken = framework.loadSuperToken(arg.superToken);
                const transactionResponse = await superToken
                    .transferFrom(arg)
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
