import {BaseQueryFn} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';
import {CreateApi} from '@reduxjs/toolkit/query';

import {typeGuard} from '../../utils';

import {CacheTagTypes} from './cacheTags/CacheTagTypes';
import {createSubgraphEndpoints} from './createSubgraphEndpoints';
import {getSerializeQueryArgs} from './getSerializeQueryArgs';
import {ValidationError} from './returnTypes';

export const createSubgraphSlice = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) =>
    createRtkQueryApi({
        reducerPath: 'sfSubgraph',
        baseQuery: subgraphSliceBaseQuery(),
        tagTypes: typeGuard<CacheTagTypes[]>(['Event', 'Index', 'Stream', 'Token']),
        endpoints: (builder) => createSubgraphEndpoints(builder),
        serializeQueryArgs: getSerializeQueryArgs(),
    });

export function subgraphSliceBaseQuery(): BaseQueryFn<void, unknown, ValidationError, Record<string, unknown>, never> {
    return function () {
        throw new Error('All queries & mutations must use the `queryFn` definition syntax.');
    };
}

export type SubgraphSliceEndpointBuilder = EndpointBuilder<
    ReturnType<typeof subgraphSliceBaseQuery>,
    CacheTagTypes,
    'sfSubgraph'
>;
