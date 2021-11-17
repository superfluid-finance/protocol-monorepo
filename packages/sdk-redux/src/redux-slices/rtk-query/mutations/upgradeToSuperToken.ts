import { initializedSuperfluidSource } from '../../../superfluidApi';
import { SuperTokenMutationArg, TransactionInfo } from '../../baseArg';
import { trackTransaction } from '../../transactions/transactionSlice';
import { invalidateTagsHandler } from '../invalidateTagsHandler';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { ethers } from 'ethers';
import { typeGuard } from '../../../utils';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type UpgradeToSuperToken = SuperTokenMutationArg & {
    amountWei: string;
};

export const { useUpgradeToSuperTokenMutation } = rtkQuerySlice.injectEndpoints(
    {
        endpoints: (builder) => ({
            upgradeToSuperToken: builder.mutation<
                TransactionInfo,
                UpgradeToSuperToken
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

                    const underlyingTokenAllowance =
                        await superToken.underlyingToken
                            .allowance({
                                providerOrSigner: framework.settings.provider,
                                owner: signerAddress,
                                spender: superToken.address,
                            })
                            .then((x) => ethers.BigNumber.from(x));

                    const isAllowanceEnough = underlyingTokenAllowance.gte(
                        ethers.BigNumber.from(arg.amountWei)
                    );
                    if (!isAllowanceEnough) {
                        const approveAllowanceTransactionResponse =
                            await superToken.underlyingToken
                                .approve({
                                    amount: arg.amountWei, // TODO(KK): Should we account for existing allowance amount here?
                                    receiver: superToken.address,
                                })
                                .exec(signer);

                        // TODO(KK): Consider a correlation ID here.
                        // Fire and forget
                        queryApi.dispatch(
                            trackTransaction({
                                hash: approveAllowanceTransactionResponse.hash,
                                chainId: arg.chainId,
                            })
                        );

                        // TODO(KK): Thinks about user experience here a bit...
                        await framework.settings.provider.waitForTransaction(
                            approveAllowanceTransactionResponse.hash,
                            1,
                            60000
                        );
                    }

                    const upgradeToSuperTokenTransactionResponse =
                        await superToken.upgrade({
                            amount: arg.amountWei
                        }).exec(signer);

                    // Fire and forget
                    queryApi.dispatch(
                        trackTransaction({
                            hash: upgradeToSuperTokenTransactionResponse.hash,
                            chainId: arg.chainId,
                        })
                    );

                    if (arg.waitForConfirmation) {
                        await framework.settings.provider.waitForTransaction(
                            upgradeToSuperTokenTransactionResponse.hash,
                            1,
                            60000
                        );
                    }
                    return {
                        data: typeGuard<TransactionInfo>({
                            hash: upgradeToSuperTokenTransactionResponse.hash,
                            chainId: arg.chainId,
                        }),
                        meta: typeGuard<MutationMeta>({
                            observeAddress: signerAddress,
                        }),
                    };
                },
                onQueryStarted: async (arg, { dispatch, queryFulfilled }) => {
                    queryFulfilled.then(async (queryResult) => {
                        const framework =
                            await initializedSuperfluidSource.getFramework(
                                arg.chainId
                            );
                        framework.query.on(
                            (events, unsubscribe) => {
                                for (const event of events) {
                                    invalidateTagsHandler(
                                        arg.chainId,
                                        event,
                                        dispatch
                                    );
                                }
                                unsubscribe();
                            },
                            2000,
                            queryResult.meta!.observeAddress,
                            30000
                        );
                    });
                },
            }),
        }),
        overrideExisting: false,
    }
);
