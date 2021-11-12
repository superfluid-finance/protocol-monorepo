import { Paging, PagedResult, ILightAccountTokenSnapshot, IAccountTokenSnapshotFilter } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {PaginatedQueryArg} from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface ListUserInteractedSuperTokensArg extends PaginatedQueryArg, IAccountTokenSnapshotFilter {
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listUserInteractedSuperTokens: builder.query<PagedResult<ILightAccountTokenSnapshot>, ListUserInteractedSuperTokensArg>({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);
                const pagedResult = await framework.query.listUserInteractedSuperTokens(
                    arg,
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

export const { useListUserInteractedSuperTokensQuery, useLazyListUserInteractedSuperTokensQuery } =
    extendedApi;
