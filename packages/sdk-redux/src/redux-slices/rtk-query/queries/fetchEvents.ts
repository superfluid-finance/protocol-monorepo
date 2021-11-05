import { AllEvents, Paging } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface FetchAllEventsArg extends QueryArg {
    skip: number;
    take: number;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        fetchAllEvents: builder.query<AllEvents[], FetchAllEventsArg>({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                const pagedResult = await framework.query.listAllEvents(new Paging({ skip: arg.skip, take: arg.take }));

                return {
                    data: pagedResult.data,
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

export const { useFetchAllEventsQuery, useLazyFetchAllEventsQuery } = extendedApi;
