import {
    IIndex,
    PagedResult,
    Paging,
} from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingString, PaginatedQueryArg} from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListIndexesArg = PaginatedQueryArg & {
    indexId: string | NothingString;
    publisherAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

export const { useListIndexesQuery, useLazyListIndexesQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listIndexes: builder.query<PagedResult<IIndex>, ListIndexesArg>({
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
                                token: arg.superTokenAddress
                            },
                            new Paging(arg)
                        ),
                    };
                },
            }),
        }),
        overrideExisting: false,
    });
