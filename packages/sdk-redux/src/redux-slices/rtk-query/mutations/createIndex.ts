import { initializedSuperfluidContext } from '../../../createSdkReduxParts';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../argTypes';
import { registerNewTransaction } from '../../transactions/registerNewTransaction';
import { monitorAddressForNextEventToInvalidateCache } from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

/**
 * Creates an IDA Index.
 */
export type CreateIndexArg = SuperTokenMutationArg & {
    indexId: string;
    userDataBytes: string;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        createIndex: builder.mutation<TransactionInfo, CreateIndexArg>({
            /**
             * Creates an IDA Index.
             */
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
                    .createIndex({
                        indexId: arg.indexId,
                        userData: arg.userDataBytes,
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
     * Documentation: {@link CreateIndexArg}.
     * @category React Hooks
     */
    useCreateIndexMutation,
} = apiSlice;
