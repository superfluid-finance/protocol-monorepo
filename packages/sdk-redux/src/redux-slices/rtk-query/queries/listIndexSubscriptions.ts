import {
    createSkipPaging,
    IIndexSubscription,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import {getSfContext} from '../../../createSdkReduxParts';
import {NothingBoolean, NothingString, PaginatedQueryArg} from '../../argTypes';
import {getMostSpecificIndexTag} from '../cacheTags/indexTags';
import {rtkQuerySlice} from '../rtkQuerySlice';

// TODO(KK): cache key?
export type ListIndexSubscriptionsArg = PaginatedQueryArg & {
    subscriberAddress: string | NothingString;
    approved: boolean | NothingBoolean;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
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
                const framework = await getSfContext().getFramework(
                    arg.chainId
                );

                return {
                    data: await framework.query.listIndexSubscriptions(
                        {
                            subscriber: arg.subscriberAddress,
                            approved: arg.approved,
                        },
                        createSkipPaging(arg)
                    ),
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
    useListIndexSubscriptionsQuery,
    /**
     * @category React Hooks
     */
    useLazyListIndexSubscriptionsQuery,
} = apiSlice;
