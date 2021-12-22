import {
    IndexSubscription,
    IndexSubscriptionGetQuery,
    IndexSubscriptionQueryHandler,
    IndexSubscriptionsListQuery,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';

export interface GetIndexSubscription extends BaseQuery2, IndexSubscriptionGetQuery {}

export interface ListIndexSubscriptions extends BaseQuery2, IndexSubscriptionsListQuery {}

export const addIndexSubscriptionEndpoints = (builder: SfEndpointBuilder) => ({
    indexSubscription: builder.query<IndexSubscription | undefined, BaseQuery2 & IndexSubscriptionGetQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new IndexSubscriptionQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.get(query);
            return {
                data: result,
            };
        },
    }),
    indexSubscriptions: builder.query<PagedResult<IndexSubscription>, BaseQuery2 & IndexSubscriptionsListQuery>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new IndexSubscriptionQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.list(query);
            return {
                data: result,
            };
        },
    }),
});
