import {getFramework} from '../../../../sdkReduxConfig';
import {MillisecondTimes} from '../../../../utils';
import {invalidateCacheTagsForEvents} from '../../cacheTags/invalidateCacheTagsForEvents';
import {CacheTime} from '../../cacheTime';
import SubgraphApiEndpointBuilder from '../subgraphApiEndpointBuilder';

import {CustomSubgraphQuery, MonitorForEventsToInvalidateCache} from './baseArgs';

export const createBaseEndpoints = (builder: SubgraphApiEndpointBuilder) => ({
    custom: builder.query<unknown, CustomSubgraphQuery>({
        keepUnusedDataFor: CacheTime.None,
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            return {
                data: await framework.query.subgraphClient.request<unknown>(arg.document, arg.variables),
            };
        },
    }),
    monitorForEventsToInvalidateCache: builder.mutation<true, MonitorForEventsToInvalidateCache>({
        queryFn: () => {
            // No-op
            return {
                data: true,
            };
        },
        onCacheEntryAdded: async (arg, {dispatch, cacheDataLoaded, cacheEntryRemoved}) => {
            // TODO(KK): Consider how changing of networks inside the application can affect this.

            const framework = await getFramework(arg.chainId);

            await cacheDataLoaded;

            const unsubscribe = framework.query.on(
                (events) => {
                    invalidateCacheTagsForEvents(arg.chainId, events, dispatch);
                },
                MillisecondTimes.TwentySeconds,
                arg.address
            );

            try {
                await cacheEntryRemoved;
            } finally {
                unsubscribe();
            }
        },
    }),
});
