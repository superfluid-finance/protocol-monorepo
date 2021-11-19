import {
    AllEvents,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingNumber, NothingString, PaginatedQueryArg} from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListEventsArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    timestamp_gte: number | NothingNumber;
};

export const { useListEventsQuery, useLazyListEventsQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listEvents: builder.query<PagedResult<AllEvents>, ListEventsArg>({
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );
                    const pagedResult = await framework.query.listEvents(
                        {
                            account: arg.accountAddress,
                            timestamp_gte: arg.timestamp_gte
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
