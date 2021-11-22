import {
    ILightAccountTokenSnapshot,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { NothingString, PaginatedQueryArg } from '../../baseArg';
import {
    getMostSpecificIndexTag,
    getMostSpecificStreamTag,
    getMostSpecificTokenTag,
    rtkQuerySlice
} from '../rtkQuerySlice';

export type ListUserInteractedSuperTokensArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

export const {
    useListUserInteractedSuperTokensQuery,
    useLazyListUserInteractedSuperTokensQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listUserInteractedSuperTokens: builder.query<
            PagedResult<ILightAccountTokenSnapshot>,
            ListUserInteractedSuperTokensArg
        >({
            providesTags: (_result, _error, arg) => [
                getMostSpecificTokenTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.accountAddress,
                    address3: undefined,
                }),
                getMostSpecificIndexTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.accountAddress,
                    address3: undefined,
                    indexId: undefined,
                }),
                getMostSpecificStreamTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.accountAddress,
                    address3: undefined,
                }),
            ],
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);
                const pagedResult =
                    await framework.query.listUserInteractedSuperTokens(
                        {
                            token: arg.superTokenAddress,
                            account: arg.accountAddress,
                        },
                        new Paging({ skip: arg.skip, take: arg.take })
                    );
                return {
                    data: pagedResult,
                };
            },
        }),
    }),
    overrideExisting: false,
});
