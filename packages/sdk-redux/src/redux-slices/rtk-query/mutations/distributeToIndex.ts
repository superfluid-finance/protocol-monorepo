import { initializedSuperfluidSource } from '../../../superfluidApi';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../baseArg';
import { observeAddressForNextEventToInvalidateCache } from '../cacheTags/observeAddressForNextEventToInvalidateCache';
import { registerNewTransaction } from '../../transactions/registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type DistributeToIndexArg = SuperTokenMutationArg & {
    indexId: string;
    amountWei: string;
    userDataBytes?: string;
};

export const { useDistributeToIndexMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        distributeToIndex: builder.mutation<
            TransactionInfo,
            DistributeToIndexArg
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
                    .distribute({
                        indexId: arg.indexId,
                        userData: arg.userDataBytes,
                        amount: arg.amountWei,
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
                    observeAddressForNextEventToInvalidateCache(
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
