import {DocumentNode} from 'graphql';

import {BaseQuery} from '../../argTypes';
import {CacheTime} from '../cacheTime';

import {handleSubgraphQueryWithFramework, SubgraphSliceEndpointBuilder} from './subgraphSlice';

export interface CustomSubgraphQuery extends BaseQuery<unknown> {
    document: string | DocumentNode;
    variables?: {
        [key: string]: any;
    };
}

export const createCustomQueryEndpoints = (builder: SubgraphSliceEndpointBuilder) => ({
    custom: builder.query<unknown, CustomSubgraphQuery>({
        keepUnusedDataFor: CacheTime.None,
        queryFn: (arg) =>
            handleSubgraphQueryWithFramework({
                chainId: arg.chainId,
                handle: (framework) => framework.query.subgraphClient.request<unknown>(arg.document, arg.variables),
            }),
    }),
});
