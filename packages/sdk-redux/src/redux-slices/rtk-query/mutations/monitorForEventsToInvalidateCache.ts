import {getSfContext} from '../../../createSdkReduxParts';
import {MillisecondTimes} from '../../../utils';
import {NothingString} from '../../argTypes';
import {invalidateCacheTagsForEvents} from '../cacheTags/invalidateCacheTagsForEvents';
import {rtkQuerySlice} from '../rtkQuerySlice';

/**
 * Continuously poll for new events to know when to invalidate cache for re-fetching of the data.
 * @param chainId The chain to poll.
 * @param address The address (account or token) to filter events for.
 */
export type MonitorForEventsToInvalidateCacheArg = {
    chainId: number;
    address: string | NothingString;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        monitorForEventsToInvalidateCache: builder.mutation<
            true,
            MonitorForEventsToInvalidateCacheArg
        >({
            queryFn: () => {
                // No-op
                return {
                    data: true,
                };
            },
            onCacheEntryAdded: async (
                arg,
                {dispatch, cacheDataLoaded, cacheEntryRemoved}
            ) => {
                // TODO(KK): Consider how changing of networks inside the application can affect this.

                const framework = await getSfContext().getFramework(
                    arg.chainId
                );

                await cacheDataLoaded;

                const unsubscribe = framework.query.on(
                    (events) => {
                        invalidateCacheTagsForEvents(
                            arg.chainId,
                            events,
                            dispatch
                        );
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
    }),
    overrideExisting: false,
});

export const {
    endpoints: {monitorForEventsToInvalidateCache},
    useMonitorForEventsToInvalidateCacheMutation,
} = apiSlice;
