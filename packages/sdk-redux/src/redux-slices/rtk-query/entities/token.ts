import {PagedResult, Token, TokenGetQuery, TokenListQuery, TokenQueryHandler} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';

export const addTokenEndpoints = (builder: SfEndpointBuilder) => ({
    token: builder.query<Token | undefined, BaseQuery2 & TokenGetQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new TokenQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.get(query);
            return {
                data: result,
            };
        },
    }),
    tokens: builder.query<PagedResult<Token>, BaseQuery2 & TokenListQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new TokenQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.list(query);
            return {
                data: result,
            };
        },
    }),
});
