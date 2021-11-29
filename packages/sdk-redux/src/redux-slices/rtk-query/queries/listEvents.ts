import { AllEvents, PagedResult, createSkipPaging } from '@superfluid-finance/sdk-core';

import { initializedContext } from '../../../createSdkReduxParts';
import {
    NothingNumber,
    NothingString,
    PaginatedQueryArg,
} from '../../argTypes';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { getMostSpecificIndexTag } from '../cacheTags/indexTags';
import { getMostSpecificStreamTag } from '../cacheTags/streamTags';
import { getMostSpecificTokenTag } from '../cacheTags/tokenTags';
import { insertIf } from '../../../utils';
import { createEventTag } from '../cacheTags/eventTags';

export type ListEventsArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    timestamp_gte: number | NothingNumber;
};

export const { useListEventsQuery, useLazyListEventsQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listEvents: builder.query<PagedResult<AllEvents>, ListEventsArg>({
                providesTags: (_result, _error, arg) => [
                    ...insertIf(
                        !arg.accountAddress,
                        createEventTag(arg.chainId)
                    ),
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
                    const framework = await initializedContext.getFramework(
                        arg.chainId
                    );
                    const pagedResult = await framework.query.listEvents(
                        {
                            account: arg.accountAddress,
                            timestamp_gte: arg.timestamp_gte,
                        },
                        createSkipPaging({ skip: arg.skip, take: arg.take })
                    );
                    return {
                        data: pagedResult,
                    };
                },
            }),
        }),
        overrideExisting: false,
    });
