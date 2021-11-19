import {
    ISuperToken,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingBoolean, PaginatedQueryArg} from '../../baseArg';
import { rtkQuerySlice, tokenTag} from '../rtkQuerySlice';

export type ListSuperTokensArg = PaginatedQueryArg & {
    isListed: boolean | NothingBoolean;
};

export const { useListSuperTokensQuery, useLazyListSuperTokensQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listSuperTokens: builder.query<
                PagedResult<ISuperToken>,
                ListSuperTokensArg
            >({
                providesTags: (_result, _error, arg) => [
                    tokenTag(arg.chainId)
                ],
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );

                    return {
                        data: await framework.query.listAllSuperTokens(
                            {
                                isListed: arg.isListed
                            },
                            new Paging(arg)
                        ),
                    };
                },

            }),
        }),
        overrideExisting: false,
    });
