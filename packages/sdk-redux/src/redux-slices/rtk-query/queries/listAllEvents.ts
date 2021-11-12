import { AllEvents, Paging, PagedResult, IEventFilter } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {PaginatedQueryArg} from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface ListAllEventsArg extends PaginatedQueryArg, IEventFilter {
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listAllEvents: builder.query<PagedResult<AllEvents>, ListAllEventsArg>({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);
                const pagedResult = await framework.query.listEvents(
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

export const { useListAllEventsQuery, useLazyListAllEventsQuery } =
    extendedApi;
