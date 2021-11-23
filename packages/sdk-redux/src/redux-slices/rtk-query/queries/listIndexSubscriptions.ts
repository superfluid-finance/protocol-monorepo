import {
    IIndexSubscription,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedContext } from '../../../createSdkReduxParts';
import {
    NothingBoolean,
    NothingString,
    PaginatedQueryArg,
} from '../../argTypes';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { getMostSpecificIndexTag } from '../cacheTags/indexTags';

// TODO(KK): cache key?
export type ListIndexSubscriptionsArg = PaginatedQueryArg & {
    subscriberAddress: string | NothingString;
    approved: boolean | NothingBoolean;
};

export const {
    useListIndexSubscriptionsQuery,
    useLazyListIndexSubscriptionsQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listIndexSubscriptions: builder.query<
            PagedResult<IIndexSubscription>,
            ListIndexSubscriptionsArg
        >({
            providesTags: (_result, _error, arg) => [
                getMostSpecificIndexTag({
                    chainId: arg.chainId,
                    address1: arg.subscriberAddress,
                    address2: undefined,
                    address3: undefined,
                    indexId: undefined,
                }),
            ],
            queryFn: async (arg) => {
                const framework = await initializedContext.getFramework(
                    arg.chainId
                );

                return {
                    data: await framework.query.listIndexSubscriptions(
                        {
                            subscriber: arg.subscriberAddress,
                            approved: arg.approved,
                        },
                        new Paging(arg)
                    ),
                };
            },
        }),
    }),
    overrideExisting: false,
});
