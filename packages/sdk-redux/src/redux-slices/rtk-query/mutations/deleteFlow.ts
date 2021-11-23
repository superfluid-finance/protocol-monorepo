import { initializedContext } from '../../../createSdkReduxParts';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../argTypes';
import { monitorAddressForNextEventToInvalidateCache } from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import { registerNewTransaction } from '../../transactions/registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type DeleteFlowArg = SuperTokenMutationArg & {
    senderAddress?: string;
    receiverAddress: string;
};

export const { useDeleteFlowMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        deleteFlow: builder.mutation<TransactionInfo, DeleteFlowArg>({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedContext.getFrameworkAndSigner(arg.chainId);
                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );

                const senderAddress = arg.senderAddress
                    ? arg.senderAddress
                    : await signer.getAddress();

                const transactionResponse = await superToken
                    .deleteFlow({
                        sender: senderAddress,
                        receiver: arg.receiverAddress,
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
                        monitorAddress: senderAddress,
                    }),
                };
            },
            onQueryStarted: async (_arg, { dispatch, queryFulfilled }) => {
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
