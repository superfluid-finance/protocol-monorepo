import { AllEvents, Paging } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface FetchAllEventsArg extends QueryArg {
    skip: number;
    take: number;
}

export interface FetchAllEventsResult {
    data: AllEvents[]; // data.data not very cool
    skip: number;
    take: number;
    hasNextPage: boolean;
    nextPageSkip?: number;
    nextPageTake?: number;
}
// TODO: Handle paging

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        fetchAllEvents: builder.query<FetchAllEventsResult, FetchAllEventsArg>({
            keepUnusedDataFor: 15,
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                const pagedResult = await framework.query.listAllEvents(
                    new Paging({ skip: arg.skip, take: arg.take })
                );

                const nextPage = pagedResult.hasNextPage? pagedResult.nextPage() : null;

                return {
                    data: {
                        data: pagedResult.data,
                        skip: pagedResult.skip,
                        take: pagedResult.take,
                        hasNextPage: pagedResult.hasNextPage,
                        nextPageSkip: nextPage ? nextPage.skip : undefined,
                        nextPageTake: nextPage ? nextPage.take : undefined,
                    }
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

export const { useFetchAllEventsQuery, useLazyFetchAllEventsQuery } =
    extendedApi;
