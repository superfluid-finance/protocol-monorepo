import { IIndex, PagedResult, Paging } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { NothingString, PaginatedQueryArg } from '../../baseArg';
import { indexTag, rtkQuerySlice } from '../rtkQuerySlice';
import { insertIf } from '../../../utils';

export type ListIndexesArg = PaginatedQueryArg & {
    indexId: string | NothingString;
    publisherAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

export const { useListIndexesQuery, useLazyListIndexesQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listIndexes: builder.query<PagedResult<IIndex>, ListIndexesArg>({
                providesTags: (_1, _2, arg) => [
                    ...insertIf(
                        !(
                            arg.superTokenAddress ||
                            arg.publisherAddress ||
                            arg.indexId
                        ),
                        indexTag(arg.chainId)
                    ),
                    ...insertIf(
                        arg.publisherAddress,
                        indexTag(arg.chainId, arg.publisherAddress!)
                    ),
                    ...insertIf(
                        arg.indexId,
                        indexTag(arg.chainId, arg.indexId!)
                    ),
                    ...insertIf(
                        arg.superTokenAddress,
                        indexTag(arg.chainId, arg.superTokenAddress!)
                    ),
                ],
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );

                    return {
                        data: await framework.query.listIndexes(
                            {
                                indexId: arg.indexId,
                                publisher: arg.publisherAddress,
                                token: arg.superTokenAddress,
                            },
                            new Paging(arg)
                        ),
                    };
                },
            }),
        }),
        overrideExisting: false,
    });
