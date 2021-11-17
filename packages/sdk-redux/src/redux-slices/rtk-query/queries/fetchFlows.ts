import { IStream } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type ListStreamsArg = QueryArg & {
    accountAddress: string;
};

export const { useFetchFlowsQuery, useLazyFetchFlowsQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            fetchFlows: builder.query<IStream[], ListStreamsArg>({
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );

                    // TODO(KK): Handle with a single query.

                    const inflows = await framework.query.listStreams({
                        receiver: arg.accountAddress.toLowerCase(),
                    });

                    const outflows = await framework.query.listStreams({
                        sender: arg.accountAddress.toLowerCase(),
                    });

                    // TODO: Ugly sorting logic...
                    const result = [...inflows.data, ...outflows.data].sort(
                        function (a, b) {
                            return (
                                Number(b.createdAtTimestamp) -
                                Number(a.createdAtTimestamp)
                            );
                        }
                    );
                    return {
                        data: result,
                    };
                },
                providesTags: (_1, _2, arg) => [
                    {
                        type: 'Stream',
                        id: `${arg.chainId}_${arg.accountAddress}`.toLowerCase(),
                    },
                ],
            }),
        }),
        overrideExisting: false,
    });
