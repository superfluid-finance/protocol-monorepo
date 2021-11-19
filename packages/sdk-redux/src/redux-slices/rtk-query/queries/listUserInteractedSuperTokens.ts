import {
    ILightAccountTokenSnapshot,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { NothingString, PaginatedQueryArg } from '../../baseArg';
import { rtkQuerySlice, tokenTag} from '../rtkQuerySlice';
import { insertIf } from '../../../utils';

export type ListUserInteractedSuperTokensArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

export const {
    useListUserInteractedSuperTokensQuery,
    useLazyListUserInteractedSuperTokensQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listUserInteractedSuperTokens: builder.query<
            PagedResult<ILightAccountTokenSnapshot>,
            ListUserInteractedSuperTokensArg
        >({
            providesTags: (_result, _error, arg) => [
                ...insertIf(
                    !(arg.accountAddress || arg.superTokenAddress),
                    tokenTag(arg.chainId)
                ),
                ...insertIf(
                    arg.superTokenAddress,
                    tokenTag(arg.chainId, arg.superTokenAddress!)
                ),
                ...insertIf(
                    arg.accountAddress,
                    tokenTag(arg.chainId, arg.accountAddress!)
                ),
            ],
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);
                const pagedResult =
                    await framework.query.listUserInteractedSuperTokens(
                        {
                            token: arg.superTokenAddress,
                            account: arg.accountAddress,
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
