import { initializedSuperfluidSource } from '../../../superfluidApi';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../baseArg';
import { observeAddressForNextEventToInvalidateCache } from '../cacheTags/observeAddressForNextEventToInvalidateCache';
import { registerNewTransaction } from '../../transactions/registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type ClaimFromIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    subscriberAddress: string;
    userDataBytes?: string;
};

export const { useClaimFromIndexSubscriptionMutation } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            claimFromIndexSubscription: builder.mutation<
                TransactionInfo,
                ClaimFromIndexSubscriptionArg
            >({
                queryFn: async (arg, queryApi) => {
                    const [framework, signer] =
                        await initializedSuperfluidSource.getFrameworkAndSigner(
                            arg.chainId
                        );

                    const superToken = await framework.loadSuperToken(
                        arg.superTokenAddress
                    );

                    const transactionResponse = await superToken
                        .claim({
                            indexId: arg.indexId,
                            publisher: arg.publisherAddress,
                            subscriber: arg.subscriberAddress,
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
                            observeAddress: arg.publisherAddress,
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
