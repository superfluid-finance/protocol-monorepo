import { initializedSuperfluidSource } from '../../../superfluidApi';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../baseArg';
import { monitorAddressEventsToInvalidateCache } from '../cacheTags/monitorAddressEventsToInvalidateCache';
import { registerNewTransaction } from '../registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type CreateFlowArg = SuperTokenMutationArg & {
    senderAddress?: string;
    receiverAddress: string;
    flowRateWei: string;
};

export const { useCreateFlowMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        createFlow: builder.mutation<TransactionInfo, CreateFlowArg>({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidSource.getFrameworkAndSigner(
                        arg.chainId
                    );

                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );

                const senderAddress = arg.senderAddress
                    ? arg.senderAddress
                    : await signer.getAddress();

                const transactionResponse = await superToken
                    .createFlow({
                        sender: senderAddress,
                        receiver: arg.receiverAddress,
                        flowRate: arg.flowRateWei,
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
                        observeAddress: senderAddress,
                    }),
                };
            },
            // TODO(KK): Consider optimistic update.
            // TODO(KK): Subscribe to re-org issues here or at "track transaction"?
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
