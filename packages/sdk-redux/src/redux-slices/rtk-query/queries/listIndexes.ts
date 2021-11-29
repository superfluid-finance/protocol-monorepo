import { IIndex, PagedResult, createSkipPaging } from '@superfluid-finance/sdk-core';

import { initializedContext } from '../../../createSdkReduxParts';
import { NothingString, PaginatedQueryArg } from '../../argTypes';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { getMostSpecificIndexTag } from '../cacheTags/indexTags';

export type ListIndexesArg = PaginatedQueryArg & {
    indexId: string | NothingString;
    publisherAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

export const { useListIndexesQuery, useLazyListIndexesQuery } =
    rtkQuerySlice.injectEndpoints({
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
                    const framework = await initializedContext.getFramework(
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
