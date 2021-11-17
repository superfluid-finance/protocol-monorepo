import {
    ISuperToken,
    ISuperTokenRequestFilter,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { PaginatedQueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListSuperTokensArg = PaginatedQueryArg & ISuperTokenRequestFilter;

export const { useListSuperTokensQuery, useLazyListSuperTokensQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listSuperTokens: builder.query<
                PagedResult<ISuperToken>,
                ListSuperTokensArg
            >({
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );

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
