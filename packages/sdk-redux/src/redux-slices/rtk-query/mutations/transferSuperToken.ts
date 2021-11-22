import { initializedSuperfluidSource } from '../../../superfluidApi';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../baseArg';
import { monitorAddressEventsToInvalidateCache } from '../cacheTags/monitorAddressEventsToInvalidateCache';
import { registerNewTransaction } from '../registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type TransferSuperTokenArg = SuperTokenMutationArg & {
    receiverAddress: string;
    amountWei: string;
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
                    framework.loadSuperToken(arg.superTokenAddress),
                    signer.getAddress(),
                ]);

                const transactionResponse = await superToken
                    .transfer({
                        amount: arg.amountWei,
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
