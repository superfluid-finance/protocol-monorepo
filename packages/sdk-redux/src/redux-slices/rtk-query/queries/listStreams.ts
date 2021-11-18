import { IStream, PagedResult, Paging } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { PaginatedQueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListStreamsArg = PaginatedQueryArg & {
    senderAddress?: string;
    receiverAddress?: string;
    tokenAddress?: string;
};

export const { useListStreamsQuery, useLazyListStreamsQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            listStreams: builder.query<PagedResult<IStream>, ListStreamsArg>({
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );

                    return {
                        data: await framework.query.listStreams(
                            {
                                sender: arg.senderAddress,
                                receiver: arg.receiverAddress,
                                token: arg.tokenAddress,
                            },
                            new Paging(arg)
                        ),
                    };
                },
            }),
        }),
        overrideExisting: false,
    });
