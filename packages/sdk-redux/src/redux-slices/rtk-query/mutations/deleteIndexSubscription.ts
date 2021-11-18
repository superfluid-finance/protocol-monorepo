import { initializedSuperfluidSource } from '../../../superfluidApi';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../baseArg';
import { observeAddressToInvalidateTags } from '../observeAddressToInvalidateTags';
import { registerNewTransaction } from '../registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type DeleteIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    subscriberAddress: string;
    userDataBytes?: string;
};

export const { useDeleteIndexSubscriptionMutation } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            deleteIndexSubscription: builder.mutation<
                TransactionInfo,
                DeleteIndexSubscriptionArg
            >({
                queryFn: async (arg, queryApi) => {
                    const [framework, signer] =
                        await initializedSuperfluidSource.getFrameworkAndSigner(
                            arg.chainId
                        );

                    const superToken = await framework.loadSuperToken(arg.superTokenAddress);

                    const transactionResponse = await superToken
                        .deleteSubscription({
                            indexId: arg.indexId,
                            publisher: arg.publisherAddress,
                            subscriber: arg.subscriberAddress,
                            userData: arg.userDataBytes
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
                        observeAddressToInvalidateTags(
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
