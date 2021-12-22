import {GetStreamQuery, PagedResult, Stream, StreamListQuery, StreamQueryHandler} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';

export const addStreamEndpoints = (builder: SfEndpointBuilder) => ({
    stream: builder.query<Stream | undefined, BaseQuery2 & GetStreamQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new StreamQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.get(query);
            return {
                data: result,
            };
        },
    }),
    streams: builder.query<PagedResult<Stream>, BaseQuery2 & StreamListQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new StreamQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.list(query);
            return {
                data: result,
            };
        },
    }),
});
