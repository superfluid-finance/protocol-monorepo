import {Index, IndexGetQuery, IndexListQuery, IndexQueryHandler, PagedResult} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';

export interface GetIndex extends BaseQuery2, IndexGetQuery {}

export interface ListIndexes extends BaseQuery2, IndexListQuery {}

export const addIndexEndpoints = (builder: SfEndpointBuilder) => ({
    index: builder.query<Index | undefined, GetIndex>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new IndexQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.get(query);
            return {
                data: result,
            };
        },
    }),
    indexes: builder.query<PagedResult<Index>, ListIndexes>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new IndexQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.list(query);
            return {
                data: result,
            };
        },
    }),
});
