import {
    IIndexSubscription,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingBoolean, NothingString, PaginatedQueryArg} from '../../baseArg';
import { indexTag, rtkQuerySlice } from '../rtkQuerySlice';
import { insertIf } from '../../../utils';

// TODO(KK): cache key?
export type ListIndexSubscriptionsArg = PaginatedQueryArg & {
    subscriberAddress: string | NothingString;
    approved: boolean | NothingBoolean;
}

export const {
    useListIndexSubscriptionsQuery,
    useLazyListIndexSubscriptionsQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listIndexSubscriptions: builder.query<
            PagedResult<IIndexSubscription>,
            ListIndexSubscriptionsArg
        >({
            providesTags: (_1, _2, arg) => [
                ...insertIf(
                    !arg.subscriberAddress,
                    indexTag(arg.chainId)
                ),
                ...insertIf(
                    arg.subscriberAddress,
                    indexTag(arg.chainId, arg.subscriberAddress!)
                ),
            ],
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                return {
                    data: await framework.query.listIndexSubscriptions(
                        {
                            subscriber: arg.subscriberAddress,
                            approved: arg.approved
                        },
                        new Paging(arg)
                    ),
                };
            },
        }),
    }),
    overrideExisting: false,
});
