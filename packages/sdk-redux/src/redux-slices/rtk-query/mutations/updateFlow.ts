import {initializedSuperfluidContext} from '../../../createSdkReduxParts';
import {typeGuard} from '../../../utils';
import {SuperTokenMutationArg, TransactionInfo} from '../../argTypes';
import {registerNewTransaction} from '../../transactions/registerNewTransaction';
import {monitorAddressForNextEventToInvalidateCache} from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import {rtkQuerySlice} from '../rtkQuerySlice';
import {MutationMeta} from '../rtkQuerySliceBaseQuery';

/**
 * Update a flow of the token of this class.
 * @param senderAddress The sender of the flow.
 * @param receiverAddress The receiver of the flow.
 * @param flowRateWei The specified flow rate.
 */
export type UpdateFlowArg = SuperTokenMutationArg & {
    senderAddress?: string;
    receiverAddress: string;
    flowRateWei: string;
};

// TODO(KK): User bytes

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        updateFlow: builder.mutation<TransactionInfo, UpdateFlowArg>({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidContext.getFrameworkAndSigner(
                        arg.chainId
                    );
                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );
                const senderAddress = arg.senderAddress
                    ? arg.senderAddress
                    : await signer.getAddress();
                const transactionResponse = await superToken
                    .updateFlow({
                        sender: senderAddress,
                        receiver: arg.receiverAddress,
                        flowRate: arg.flowRateWei,
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
            onQueryStarted: async (_arg, {dispatch, queryFulfilled}) => {
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
     * Documentation: {@link UpdateFlowArg}
     * @category React Hooks
     */
    useUpdateFlowMutation,
} = apiSlice;
