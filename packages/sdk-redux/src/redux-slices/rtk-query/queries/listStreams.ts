import { IStream, PagedResult, Paging } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingString, PaginatedQueryArg} from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListStreamsArg = PaginatedQueryArg & {
    senderAddress: string | NothingString;
    receiverAddress: string | NothingString;
    superTokenAddress: string | NothingString;
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
