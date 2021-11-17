import { initializedSuperfluidSource } from '../../../superfluidApi';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../baseArg';
import { observeAddressToInvalidateTags } from '../observeAddressToInvalidateTags';
import { registerNewTransaction } from '../registerNewTransaction';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type UpdateIndexSubscriptionUnitsArg = SuperTokenMutationArg & {
    subscriberAddress: string;
    indexId: string;
    unitsNumber: string;
    amountWei: string;
    userDataBytes?: string;
};

export const { useUpdateIndexSubscriptionUnitsMutation } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            updateIndexSubscriptionUnits: builder.mutation<
                TransactionInfo,
                UpdateIndexSubscriptionUnitsArg
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
                        .updateSubscriptionUnits({
                            indexId: arg.indexId,
                            subscriber: arg.subscriberAddress,
                            units: arg.unitsNumber,
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
                            observeAddress: signerAddress,
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
