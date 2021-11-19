import {
    AllEvents,
    PagedResult,
    Paging} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingNumber, NothingString, PaginatedQueryArg} from '../../baseArg';
import {eventTag, indexTag, rtkQuerySlice, streamTag, tokenTag} from '../rtkQuerySlice';
import { insertIf } from '../../../utils';

export type ListEventsArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    timestamp_gte: number | NothingNumber;
};

export const { useListEventsQuery, useLazyListEventsQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listEvents: builder.query<PagedResult<AllEvents>, ListEventsArg>({
                providesTags: (_1, _2, arg) => [
                    ...insertIf(!arg.accountAddress, eventTag(arg.chainId)),
                    ...insertIf(
                        arg.accountAddress,
                        streamTag(arg.chainId, arg.accountAddress!),
                        indexTag(arg.chainId, arg.accountAddress!),
                        tokenTag(arg.chainId, arg.accountAddress!),
                    ),
                ],
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
            }),
        }),
        overrideExisting: false,
    });
