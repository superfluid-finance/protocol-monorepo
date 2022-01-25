import {DocumentNode} from 'graphql';

import {BaseQuery} from '../../argTypes';
import {CacheTime} from '../cacheTime';

import {handleSubgraphQueryWithFramework, SubgraphSliceEndpointBuilder} from './subgraphSlice';

type RequestDocument = string | DocumentNode;

declare type Variables = {
    [key: string]: any;
};

export interface CustomSubgraphQuery extends BaseQuery<unknown> {
    document: RequestDocument;
    variables?: Variables;
}

export const createCustomSubgraphQueryEndpoints = (builder: SubgraphSliceEndpointBuilder) => ({
    custom: builder.query<unknown, CustomSubgraphQuery>({
        keepUnusedDataFor: CacheTime.None,
        queryFn: (arg) =>
            handleSubgraphQueryWithFramework({
                chainId: arg.chainId,
                handle: (framework) => framework.query.subgraphClient.request<unknown>(arg.document, arg.variables),
            }),
    }),
});
