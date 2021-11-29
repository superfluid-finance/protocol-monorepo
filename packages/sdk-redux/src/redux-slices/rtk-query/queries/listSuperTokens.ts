import { ISuperToken, PagedResult, createSkipPaging } from '@superfluid-finance/sdk-core';

import { initializedContext } from '../../../createSdkReduxParts';
import { NothingBoolean, PaginatedQueryArg } from '../../argTypes';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { getMostSpecificTokenTag } from '../cacheTags/tokenTags';

export type ListSuperTokensArg = PaginatedQueryArg & {
    isListed: boolean | NothingBoolean;
};

export const { useListSuperTokensQuery, useLazyListSuperTokensQuery } =
    rtkQuerySlice.injectEndpoints({
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
                    const framework = await initializedContext.getFramework(
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
