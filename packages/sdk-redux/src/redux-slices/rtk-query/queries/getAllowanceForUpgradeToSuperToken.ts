import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type GetAllowanceForUpgradeToSuperTokenArg = QueryArg & {
    accountAddress: string;
    superTokenAddress: string;
};

// TODO(KK): Tags?

export const {
    useGetAllowanceForUpgradeToSuperTokenQuery,
    useLazyGetAllowanceForUpgradeToSuperTokenQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        getAllowanceForUpgradeToSuperToken: builder.query<
            string,
            GetAllowanceForUpgradeToSuperTokenArg
        >({
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
