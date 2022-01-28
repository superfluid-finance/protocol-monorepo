import {BaseQueryFn} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';
import {CreateApi} from '@reduxjs/toolkit/query';
import {Framework, SFError} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {typeGuard} from '../../../utils';
import {CacheTagTypes} from '../cacheTags/CacheTagTypes';
import {CacheTime} from '../cacheTime';
import {getSerializeQueryArgs} from '../getSerializeQueryArgs';

export const createSubgraphSlice = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) =>
    createRtkQueryApi({
        keepUnusedDataFor: CacheTime.OneMinute,
        reducerPath: 'sfSubgraph',
        baseQuery: subgraphSliceBaseQuery(),
        tagTypes: typeGuard<CacheTagTypes[]>(['Event', 'Index', 'Stream', 'Token']),
        endpoints: (_builder: SubgraphSliceEndpointBuilder) => ({}),
        serializeQueryArgs: getSerializeQueryArgs(),
    });

export type SubgraphSliceEndpointBuilder = EndpointBuilder<SubgraphSliceBaseQueryType, CacheTagTypes, 'sfSubgraph'>;

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
