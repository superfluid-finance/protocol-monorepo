import { initializedContext } from '../../../createSdkReduxParts';
import { typeGuard } from '../../../utils';
import {
    NothingString,
    SuperTokenMutationArg,
    TransactionInfo,
} from '../../argTypes';
import { monitorAddressForNextEventToInvalidateCache } from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import { registerNewTransaction } from '../../transactions/registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type ApproveIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    userDataBytes: string | NothingString;
};

export const { useApproveIndexSubscriptionMutation } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            approveIndexSubscription: builder.mutation<
                TransactionInfo,
                ApproveIndexSubscriptionArg
            >({
                queryFn: async (arg, queryApi) => {
                    const [framework, signer] =
                        await initializedContext.getFrameworkAndSigner(
                            arg.chainId
                        );

                    const superToken = await framework.loadSuperToken(
                        arg.superTokenAddress
                    );

                    const transactionResponse = await superToken
                        .approveSubscription({
                            indexId: arg.indexId,
                            publisher: arg.publisherAddress,
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
                            monitorAddress: arg.publisherAddress,
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
