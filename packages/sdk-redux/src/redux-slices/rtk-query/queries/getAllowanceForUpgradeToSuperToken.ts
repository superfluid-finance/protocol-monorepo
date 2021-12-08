import {getSfContext} from '../../../createSdkReduxParts';
import {QueryArg} from '../../argTypes';
import {getMostSpecificTokenTag} from '../cacheTags/tokenTags';
import {rtkQuerySlice} from '../rtkQuerySlice';

/**
 *
 */
export type GetAllowanceForUpgradeToSuperTokenArg = QueryArg & {
    accountAddress: string;
    superTokenAddress: string;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        getAllowanceForUpgradeToSuperToken: builder.query<
            string,
            GetAllowanceForUpgradeToSuperTokenArg
        >({
            keepUnusedDataFor: 0, // We can't listen for "approval" event from Subgraph currently.
            providesTags: (_result, _error, arg) => [
                getMostSpecificTokenTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.accountAddress,
                    address3: undefined,
                }),
            ],
            queryFn: async (arg) => {
                const framework = await getSfContext().getFramework(
                    arg.chainId
                );

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

export const {
    endpoints: { getAllowanceForUpgradeToSuperToken },
    useGetAllowanceForUpgradeToSuperTokenQuery,
    useLazyGetAllowanceForUpgradeToSuperTokenQuery,
} = apiSlice;
