import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import {rtkQuerySlice, tokenTag} from '../rtkQuerySlice';

export type GetAllowanceForUpgradeToSuperTokenArg = QueryArg & {
    accountAddress: string;
    superTokenAddress: string;
};

export const {
    useGetAllowanceForUpgradeToSuperTokenQuery,
    useLazyGetAllowanceForUpgradeToSuperTokenQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        getAllowanceForUpgradeToSuperToken: builder.query<
            string,
            GetAllowanceForUpgradeToSuperTokenArg
        >({
            keepUnusedDataFor: 0, // We can't listen for "approval" event from Subgraph currently.
            providesTags: (_1, _2, arg) => [
                tokenTag(arg.chainId, arg.superTokenAddress, arg.accountAddress!),
            ],
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );

                const underlyingTokenAllowance =
                    await superToken.underlyingToken.allowance({
                        providerOrSigner: framework.settings.provider,
                        owner: arg.accountAddress,
                        spender: superToken.address,
                    });

                return {
                    data: underlyingTokenAllowance,
                };
            },
        }),
    }),
    overrideExisting: false,
});
