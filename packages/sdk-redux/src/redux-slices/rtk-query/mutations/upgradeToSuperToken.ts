import { ethers } from 'ethers';

import { initializedSuperfluidContext } from '../../../createSdkReduxParts';
import { typeGuard } from '../../../utils';
import { SuperTokenMutationArg, TransactionInfo } from '../../argTypes';
import { registerNewTransaction } from '../../transactions/registerNewTransaction';
import { monitorAddressForNextEventToInvalidateCache } from '../cacheTags/monitorAddressForNextEventToInvalidateCache';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { MutationMeta } from '../rtkQuerySliceBaseQuery';

export type UpgradeToSuperToken = SuperTokenMutationArg & {
    amountWei: string;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        upgradeToSuperToken: builder.mutation<
            TransactionInfo,
            UpgradeToSuperToken
        >({
            queryFn: async (arg, queryApi) => {
                const [framework, signer] =
                    await initializedSuperfluidContext.getFrameworkAndSigner(
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

                    // NOTE: Always wait for transaction confirmation here.
                    await registerNewTransaction(
                        arg.chainId,
                        approveAllowanceTransactionResponse.hash,
                        true,
                        queryApi.dispatch
                    );
                }

                const upgradeToSuperTokenTransactionResponse = await superToken
                    .upgrade({
                        amount: arg.amountWei,
                    })
                    .exec(signer);

                await registerNewTransaction(
                    arg.chainId,
                    upgradeToSuperTokenTransactionResponse.hash,
                    !!arg.waitForConfirmation,
                    queryApi.dispatch
                );

                return {
                    data: typeGuard<TransactionInfo>({
                        hash: upgradeToSuperTokenTransactionResponse.hash,
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
     * Documentation: {@link UpgradeToSuperToken}
     * @category React Hooks
     */
    useUpgradeToSuperTokenMutation,
} = apiSlice;
