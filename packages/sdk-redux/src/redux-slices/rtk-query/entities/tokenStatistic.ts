import {
    PagedResult,
    TokenStatistic,
    TokenStatisticGetQuery,
    TokenStatisticListQuery,
    TokenStatisticsQueryHandler,
} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';

export const addTokenStatisticEndpoints = (builder: SfEndpointBuilder) => ({
    tokenStatistic: builder.query<TokenStatistic | undefined, BaseQuery2 & TokenStatisticGetQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new TokenStatisticsQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.get(query);
            return {
                data: result,
            };
        },
    }),
    tokens: builder.query<PagedResult<TokenStatistic>, BaseQuery2 & TokenStatisticListQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new TokenStatisticsQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.list(query);
            return {
                data: result,
            };
        },
    }),
});
