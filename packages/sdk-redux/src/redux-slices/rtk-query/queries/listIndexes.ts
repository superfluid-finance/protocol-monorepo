import {
    createSkipPaging,
    IIndex,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidContext } from '../../../createSdkReduxParts';
import { NothingString, PaginatedQueryArg } from '../../argTypes';
import { getMostSpecificIndexTag } from '../cacheTags/indexTags';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListIndexesArg = PaginatedQueryArg & {
    indexId: string | NothingString;
    publisherAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listIndexes: builder.query<PagedResult<IIndex>, ListIndexesArg>({
            providesTags: (_result, _error, arg) => [
                getMostSpecificIndexTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.publisherAddress,
                    address3: undefined,
                    indexId: arg.indexId,
                }),
            ],
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidContext.getFramework(
                        arg.chainId
                    );

                return {
                    data: await framework.query.listIndexes(
                        {
                            indexId: arg.indexId,
                            publisher: arg.publisherAddress,
                            token: arg.superTokenAddress,
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
    useListIndexesQuery,
    /**
     * @category React Hooks
     */
    useLazyListIndexesQuery,
} = apiSlice;
