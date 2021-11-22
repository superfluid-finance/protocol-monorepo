import { AllEvents, PagedResult, Paging } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { NothingNumber, NothingString, PaginatedQueryArg } from '../../baseArg';
import {
    getMostSpecificIndexTag,
    getMostSpecificStreamTag,
    getMostSpecificTokenTag,
    rtkQuerySlice,
} from '../rtkQuerySlice';

export type ListEventsArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    timestamp_gte: number | NothingNumber;
};

export const { useListEventsQuery, useLazyListEventsQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listEvents: builder.query<PagedResult<AllEvents>, ListEventsArg>({
                providesTags: (_result, _error, arg) => [
                    getMostSpecificIndexTag({
                        chainId: arg.chainId,
                        address1: arg.accountAddress,
                        address2: undefined,
                        address3: undefined,
                        indexId: undefined,
                    }),
                    getMostSpecificStreamTag({
                        chainId: arg.chainId,
                        address1: arg.accountAddress,
                        address2: undefined,
                        address3: undefined,
                    }),
                    getMostSpecificTokenTag({
                        chainId: arg.chainId,
                        address1: arg.accountAddress,
                        address2: undefined,
                        address3: undefined,
                    }),
                ],
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );
                    const pagedResult = await framework.query.listEvents(
                        {
                            account: arg.accountAddress,
                            timestamp_gte: arg.timestamp_gte,
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
