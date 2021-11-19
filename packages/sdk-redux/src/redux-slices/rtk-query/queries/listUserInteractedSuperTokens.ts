import {
    ILightAccountTokenSnapshot,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingString, PaginatedQueryArg} from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListUserInteractedSuperTokensArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    superTokenAddress: string | NothingString;
}

export const {
    useListUserInteractedSuperTokensQuery,
    useLazyListUserInteractedSuperTokensQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listUserInteractedSuperTokens: builder.query<
            PagedResult<ILightAccountTokenSnapshot>,
            ListUserInteractedSuperTokensArg
        >({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);
                const pagedResult =
                    await framework.query.listUserInteractedSuperTokens(
                        {
                            token: arg.superTokenAddress,
                            account: arg.accountAddress
                        },
                        new Paging({ skip: arg.skip, take: arg.take })
                    );
                return {
                    data: pagedResult,
                };
            },
            providesTags: (_1, _2, arg) => [
                {
                    type: 'Event',
                    id: `${arg.chainId}`,
                },
            ],
        }),
    }),
    overrideExisting: false,
});
