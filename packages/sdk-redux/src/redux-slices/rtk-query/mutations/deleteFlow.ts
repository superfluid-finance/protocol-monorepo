import { initializedSuperfluidSource } from '../../../superfluidApi';
import { MutationArg, TransactionInfo } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface DeleteFlowArg extends MutationArg {
    superToken: string;
    sender: string;
    receiver: string;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        deleteFlow: builder.mutation<TransactionInfo, DeleteFlowArg>({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidSource.getFrameworkAndSigner(
                        arg.chainId
                    );
                const superToken = framework.loadSuperToken(arg.superToken);
                const transactionResponse = await superToken
                    .deleteFlow({
                        sender: arg.sender,
                        receiver: arg.receiver,
                    })
                    .then((x) => x.exec(signer as any)); // TODO(KK): as any
                queryApi.dispatch(
                    trackTransaction({
                        chainId: arg.chainId,
                        hash: transactionResponse.hash,
                    })
                );
                return {
                    data: {
                        chainId: arg.chainId,
                        hash: transactionResponse.hash,
                    },
                };
            },
            onQueryStarted: async (arg, { dispatch, queryFulfilled }) => {
                const queryResult = await queryFulfilled;
                dispatch(
                    trackTransaction({
                        hash: queryResult.data.hash,
                        chainId: queryResult.data.chainId,
                    })
                )
                    .unwrap()
                    .then(() => {
                        dispatch(
                            rtkQuerySlice.util.invalidateTags([
                                {
                                    type: 'Flow',
                                    id: `${arg.chainId}_${arg.sender}`,
                                },
                                {
                                    type: 'Flow',
                                    id: `${arg.chainId}_${arg.receiver}`,
                                },
                            ])
                        );
                    });
            },
        }),
    }),
    overrideExisting: false,
});

export const { useDeleteFlowMutation } = extendedApi;
