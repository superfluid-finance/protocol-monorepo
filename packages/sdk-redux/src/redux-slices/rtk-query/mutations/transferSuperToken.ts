import { initializedSuperfluidContext } from '../../../createSdkReduxParts';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../argTypes';
import { registerNewTransaction } from '../../transactions/registerNewTransaction';
import { monitorAddressForNextEventToInvalidateCache } from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

/**
 * Transfer `receiver` `amount` tokens.
 * @param receiverAddress The receiver of the transfer.
 * @param amountWei The amount to be transferred.
 */
export type TransferSuperTokenArg = SuperTokenMutationArg & {
    receiverAddress: string;
    amountWei: string;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        transferSuperToken: builder.mutation<
            TransactionInfo,
            TransferSuperTokenArg
        >({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidContext.getFrameworkAndSigner(
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
                        monitorAddress: signerAddress,
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

export const {
    /**
     * Documentation: {@link TransferSuperTokenArg}
     * @category React Hooks
     */
    useTransferSuperTokenMutation,
} = apiSlice;
