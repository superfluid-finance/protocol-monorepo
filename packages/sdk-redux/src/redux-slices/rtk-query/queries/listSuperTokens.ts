import {
    createSkipPaging,
    ISuperToken,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import {initializedSuperfluidContext} from '../../../createSdkReduxParts';
import {NothingBoolean, PaginatedQueryArg} from '../../argTypes';
import {getMostSpecificTokenTag} from '../cacheTags/tokenTags';
import {rtkQuerySlice} from '../rtkQuerySlice';

export type ListSuperTokensArg = PaginatedQueryArg & {
    isListed: boolean | NothingBoolean;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listSuperTokens: builder.query<
            PagedResult<ISuperToken>,
            ListSuperTokensArg
        >({
            providesTags: (_result, _error, arg) => [
                getMostSpecificTokenTag({
                    chainId: arg.chainId,
                    address1: undefined,
                    address2: undefined,
                    address3: undefined,
                }),
            ],
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidContext.getFramework(
                        arg.chainId
                    );

                return {
                    data: await framework.query.listAllSuperTokens(
                        {
                            isListed: arg.isListed,
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
    useListSuperTokensQuery,
    /**
     * @category React Hooks
     */
    useLazyListSuperTokensQuery,
} = apiSlice;
