import {
    ISuperToken,
    Paging,
    PagedResult,
    ISuperTokenRequestFilter,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { PaginatedQueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface ListSuperTokensArg
    extends PaginatedQueryArg,
        ISuperTokenRequestFilter {
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listSuperTokens: builder.query<
            PagedResult<ISuperToken>,
            ListSuperTokensArg
        >({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                return {
                    data: await framework.query.listAllSuperTokens(
                        arg,
                        new Paging(arg)
                    ),
                };
            },
        }),
    }),
    overrideExisting: false,
});

export const { useListSuperTokensQuery, useLazyListSuperTokensQuery } =
    extendedApi;
