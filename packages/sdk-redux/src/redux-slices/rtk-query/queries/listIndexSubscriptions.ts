import {
    IIndexSubscription,
    IIndexSubscriptionRequestFilter,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { PaginatedQueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

// TODO(KK): cache key?
export type ListIndexSubscriptionsArg = PaginatedQueryArg &
    IIndexSubscriptionRequestFilter;

export const {
    useListIndexSubscriptionsQuery,
    useLazyListIndexSubscriptionsQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listIndexSubscriptions: builder.query<
            PagedResult<IIndexSubscription>,
            ListIndexSubscriptionsArg
        >({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                return {
                    data: await framework.query.listIndexSubscriptions(
                        arg,
                        new Paging(arg)
                    ),
                };
            },
        }),
    }),
    overrideExisting: false,
});
