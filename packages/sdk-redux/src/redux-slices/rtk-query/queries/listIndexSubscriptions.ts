import {
    IIndexSubscription,
    Paging,
    PagedResult,
    IIndexSubscriptionRequestFilter
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { PaginatedQueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface ListIndexSubscriptionsArg
    extends PaginatedQueryArg,
        IIndexSubscriptionRequestFilter {}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listIndexSubscriptions: builder.query<PagedResult<IIndexSubscription>, ListIndexSubscriptionsArg>({
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

export const { useListIndexSubscriptionsQuery, useLazyListIndexSubscriptionsQuery } = extendedApi;
