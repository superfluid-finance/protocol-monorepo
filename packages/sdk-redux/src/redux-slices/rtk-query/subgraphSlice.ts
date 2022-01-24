import {BaseQueryFn} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';
import {CreateApi} from '@reduxjs/toolkit/query';
import {Framework, SFError} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../sdkReduxConfig';
import {typeGuard} from '../../utils';

import {CacheTagTypes} from './cacheTags/CacheTagTypes';
import {createSubgraphEndpoints} from './createSubgraphEndpoints';
import {getSerializeQueryArgs} from './getSerializeQueryArgs';

export const createSubgraphSlice = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) =>
    createRtkQueryApi({
        reducerPath: 'sfSubgraph',
        baseQuery: subgraphSliceBaseQuery(),
        tagTypes: typeGuard<CacheTagTypes[]>(['Event', 'Index', 'Stream', 'Token']),
        endpoints: (builder) => createSubgraphEndpoints(builder),
        serializeQueryArgs: getSerializeQueryArgs(),
    });

export const subgraphSliceBaseQuery =
    (): BaseQueryFn<
        {chainId: number; handle: (framework: Framework) => Promise<unknown>},
        unknown,
        SFError,
        Record<string, unknown>
    > =>
    async ({chainId, handle}) => {
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

export type SubgraphSliceEndpointBuilder = EndpointBuilder<
    ReturnType<typeof subgraphSliceBaseQuery>,
    CacheTagTypes,
    'sfSubgraph'
>;
