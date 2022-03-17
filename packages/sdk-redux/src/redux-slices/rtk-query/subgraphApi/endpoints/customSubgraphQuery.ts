import {DocumentNode} from 'graphql';

import {getFramework} from '../../../../sdkReduxConfig';
import {BaseQuery} from '../../../argTypes';
import {CacheTime} from '../../cacheTime';
import SubgraphApiEndpointBuilder from '../subgraphApiEndpointBuilder';

export interface CustomSubgraphQuery extends BaseQuery<unknown> {
    document: string | DocumentNode;
    variables?: {
        [key: string]: any;
    };
}

export const createCustomQueryEndpoints = (builder: SubgraphApiEndpointBuilder) => ({
    custom: builder.query<unknown, CustomSubgraphQuery>({
        keepUnusedDataFor: CacheTime.None,
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            return {
                data: await framework.query.subgraphClient.request<unknown>(arg.document, arg.variables),
            };
        },
    }),
});
