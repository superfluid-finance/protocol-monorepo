import {
    createSkipPaging,
    ILightAccountTokenSnapshot,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import {getSfContext} from '../../../createSdkReduxParts';
import {NothingString, PaginatedQueryArg} from '../../argTypes';
import {getMostSpecificIndexTag} from '../cacheTags/indexTags';
import {getMostSpecificStreamTag} from '../cacheTags/streamTags';
import {getMostSpecificTokenTag} from '../cacheTags/tokenTags';
import {rtkQuerySlice} from '../rtkQuerySlice';

export type ListUserInteractedSuperTokensArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listUserInteractedSuperTokens: builder.query<
            PagedResult<ILightAccountTokenSnapshot>,
            ListUserInteractedSuperTokensArg
        >({
            providesTags: (_result, _error, arg) => [
                getMostSpecificTokenTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.accountAddress,
                    address3: undefined,
                }),
                getMostSpecificIndexTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.accountAddress,
                    address3: undefined,
                    indexId: undefined,
                }),
                getMostSpecificStreamTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.accountAddress,
                    address3: undefined,
                }),
            ],
            queryFn: async (arg) => {
                const framework = await getSfContext().getFramework(
                    arg.chainId
                );
                const pagedResult =
                    await framework.query.listUserInteractedSuperTokens(
                        {
                            token: arg.superTokenAddress,
                            account: arg.accountAddress,
                        },
                        createSkipPaging({skip: arg.skip, take: arg.take})
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
    useListUserInteractedSuperTokensQuery,
    /**
     * @category React Hooks
     */
    useLazyListUserInteractedSuperTokensQuery,
} = apiSlice;
