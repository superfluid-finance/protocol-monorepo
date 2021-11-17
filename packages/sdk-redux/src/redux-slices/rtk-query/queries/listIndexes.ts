import {
    IIndex,
    IIndexRequestFilter,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { PaginatedQueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListIndexesArg = PaginatedQueryArg & IIndexRequestFilter;

export const { useListIndexesQuery, useLazyListIndexesQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listIndexes: builder.query<PagedResult<IIndex>, ListIndexesArg>({
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );

                    return {
                        data: await framework.query.listIndexes(
                            arg,
                            new Paging(arg)
                        ),
                    };
                },
            }),
        }),
        overrideExisting: false,
    });
