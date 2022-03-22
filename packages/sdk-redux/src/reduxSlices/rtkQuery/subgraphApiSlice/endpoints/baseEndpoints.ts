import {getFramework, getSubgraphClient} from '../../../../sdkReduxConfig';
import {MillisecondTimes} from '../../../../utils';
import {invalidateCacheTagsForEvents} from '../../cacheTags/invalidateCacheTagsForEvents';
import {CacheTime} from '../../cacheTime';
import {SubgraphEndpointBuilder} from '../subgraphEndpointBuilder';

import {CustomSubgraphQuery, MonitorForEventsToInvalidateCacheMutation} from './baseArgs';

export const createBaseEndpoints = (builder: SubgraphEndpointBuilder) => ({
    custom: builder.query<unknown, CustomSubgraphQuery>({
        keepUnusedDataFor: CacheTime.None,
        queryFn: async (arg) => {
            const subgraphClient = await getSubgraphClient(arg.chainId);
            return {
                data: await subgraphClient.request<unknown>(arg.document, arg.variables),
            };
        },
    }),
    monitorForEventsToInvalidateCache: builder.mutation<true, MonitorForEventsToInvalidateCacheMutation>({
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
