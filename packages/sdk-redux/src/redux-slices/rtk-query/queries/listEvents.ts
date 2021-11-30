import {
    AllEvents,
    createSkipPaging,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidContext } from '../../../createSdkReduxParts';
import { insertIf } from '../../../utils';
import {
    NothingNumber,
    NothingString,
    PaginatedQueryArg,
} from '../../argTypes';
import { createEventTag } from '../cacheTags/eventTags';
import { getMostSpecificIndexTag } from '../cacheTags/indexTags';
import { getMostSpecificStreamTag } from '../cacheTags/streamTags';
import { getMostSpecificTokenTag } from '../cacheTags/tokenTags';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListEventsArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    timestamp_gte: number | NothingNumber;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listEvents: builder.query<PagedResult<AllEvents>, ListEventsArg>({
            providesTags: (_result, _error, arg) => [
                ...insertIf(!arg.accountAddress, createEventTag(arg.chainId)),
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
                    await initializedSuperfluidContext.getFramework(
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

export const {
    /**
     * @category React Hooks
     */
    useListEventsQuery,
    /**
     * @category React Hooks
     */
    useLazyListEventsQuery,
} = apiSlice;
