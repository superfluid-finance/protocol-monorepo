import { NothingString } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { initializedSuperfluidSource } from '../../../superfluidApi';
import {invalidateCacheTagsForEvents} from "../cacheTags/invalidateCacheTagsForEvents";
import {MsTimes} from "../../../utils";

export type ObserveForEventsToInvalidateCacheArg = {
    chainId: number;
    address: string | NothingString;
};

export const { useObserveForEventsToInvalidateCacheMutation } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            observeForEventsToInvalidateCache: builder.mutation<
                true,
                ObserveForEventsToInvalidateCacheArg
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

                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );

                    console.log("onCacheEntryAdded")

                    await cacheDataLoaded;

                    console.log("cacheDataLoaded")

                    const unsubscribe = framework.query.on((events) => {
                        invalidateCacheTagsForEvents(arg.chainId, events, dispatch)
                    }, MsTimes.TwentySeconds, arg.address)

                    try {
                        await cacheEntryRemoved;
                        console.log("cacheEntryRemoved")
                    } finally {
                        unsubscribe();
                    }
                },
            }),
        }),
        overrideExisting: false,
    });
