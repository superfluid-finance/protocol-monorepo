import {IStream} from "@superfluid-finance/sdk-core";

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface FetchFlowsArg extends QueryArg {
    accountAddress: string;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        fetchFlows: builder.query<IStream[], FetchFlowsArg>({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);

                // TODO(KK): Handle with a single query.

                const inflows = await framework.query.listStreams({
                    receiver: arg.accountAddress.toLowerCase()
                });

                const outflows = await framework.query.listStreams({
                    sender: arg.accountAddress.toLowerCase()
                });

                const result = [...inflows.data, ...outflows.data];
                return {
                    data: result,
                };
            },
            providesTags: (_1, _2, arg) => [
                {
                    type: 'Flow',
                    id: `${arg.chainId}_${arg.accountAddress}`,
                },
            ],
        }),
    }),
    overrideExisting: false,
});

export const { useFetchFlowsQuery, useLazyFetchFlowsQuery } = extendedApi;
