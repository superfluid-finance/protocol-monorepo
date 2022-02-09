import {BaseQueryFn} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';
import {CreateApi} from '@reduxjs/toolkit/query';
import {Framework, SFError} from '@superfluid-finance/sdk-core';

import {getConfig, getFramework} from '../../../sdkReduxConfig';
import {typeGuard} from '../../../utils';
import {CacheTagTypes} from '../cacheTags/CacheTagTypes';
import {CacheTime} from '../cacheTime';
import {getSerializeQueryArgs} from '../getSerializeQueryArgs';

/**
 * For initializing "sfApi" Redux slice.
 *
 * @param createApi Pass in either {@see createApiWithReactHooks} or {@see createApiWithoutReactHooks}.
 * You can wrap the function with your own function to add even more configuration to the RTK-Query API (e.g. "redux-persist" support).
 */
export const initializeSubgraphSlice = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) => {
    const slice = createRtkQueryApi({
        keepUnusedDataFor: CacheTime.OneMinute,
        reducerPath: 'sfSubgraph',
        baseQuery: subgraphSliceBaseQuery(),
        tagTypes: typeGuard<CacheTagTypes[]>(['Event', 'Index', 'Stream', 'Token']),
        endpoints: () => ({}),
        serializeQueryArgs: getSerializeQueryArgs(),
    });
    getConfig().setSubgraphSlice(slice as any);
    return slice;
};

export type SubgraphSliceEndpointBuilder = EndpointBuilder<SubgraphSliceBaseQueryType, CacheTagTypes, 'sfSubgraph'>;

export type SfSubgraphSliceInferredType = ReturnType<typeof initializeSubgraphSlice>;

type SubgraphSliceArgs = {chainId: number; handle: (framework: Framework) => Promise<unknown>};

export const subgraphSliceBaseQuery = (): BaseQueryFn<SubgraphSliceArgs, unknown, SFError, Record<string, unknown>> =>
    handleSubgraphQueryWithFramework;

export type SubgraphSliceBaseQueryType = ReturnType<typeof subgraphSliceBaseQuery>;

export const handleSubgraphQueryWithFramework = async ({chainId, handle}: SubgraphSliceArgs) => {
    try {
        const framework = await getFramework(chainId);
        return {data: await handle(framework)};
    } catch (error) {
        if (error instanceof SFError) {
            return {
                error: error,
            };
        } else {
            throw error;
        }
    }
};
