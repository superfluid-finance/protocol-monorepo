import { initializedContext } from '../../../superfluidApi';
import { typeGuard } from '../../../utils';
import {
    NothingString,
    SuperTokenMutationArg,
    TransactionInfo,
} from '../../baseArg';
import { monitorAddressForNextEventToInvalidateCache } from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import { registerNewTransaction } from '../../transactions/registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type DistributeToIndexArg = SuperTokenMutationArg & {
    indexId: string;
    amountWei: string;
    userDataBytes: string | NothingString;
};

export const { useDistributeToIndexMutation } = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        distributeToIndex: builder.mutation<
            TransactionInfo,
            DistributeToIndexArg
        >({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedContext.getFrameworkAndSigner(arg.chainId);

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
