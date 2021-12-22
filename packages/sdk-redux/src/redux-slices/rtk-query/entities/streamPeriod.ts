import {
    PagedResult,
    StreamPeriod,
    StreamPeriodGetQuery,
    StreamPeriodListQuery,
    StreamPeriodQueryHandler,
} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';

export const addStreamPeriodEndpoints = (builder: SfEndpointBuilder) => ({
    stream: builder.query<StreamPeriod | undefined, BaseQuery2 & StreamPeriodGetQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new StreamPeriodQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.get(query);
            return {
                data: result,
            };
        },
    }),
    streams: builder.query<PagedResult<StreamPeriod>, BaseQuery2 & StreamPeriodListQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new StreamPeriodQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.list(query);
            return {
                data: result,
            };
        },
    }),
});
