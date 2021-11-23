import { NothingString } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { initializedContext } from '../../../superfluidApi';
import { invalidateCacheTagsForEvents } from '../cacheTags/invalidateCacheTagsForEvents';
import { MsTimes } from '../../../utils';

export type MonitorForEventsToInvalidateCacheArg = {
    chainId: number;
    address: string | NothingString;
};

export const { useMonitorForEventsToInvalidateCacheMutation } =
    rtkQuerySlice.injectEndpoints({
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
                    { dispatch, cacheDataLoaded, cacheEntryRemoved }
                ) => {
                    // TODO(KK): Consider how changing of networks inside the application can affect this.

                    const framework = await initializedContext.getFramework(
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
                        MsTimes.TwentySeconds,
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
