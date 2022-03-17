import {CreateApi} from '@reduxjs/toolkit/dist/query';
import {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';

import {typeGuard} from '../../../utils';
import {cacheTagTypes} from '../cacheTags/CacheTagTypes';
import {CacheTime} from '../cacheTime';
import {getSerializeQueryArgs} from '../getSerializeQueryArgs';

import {subgraphApiBaseQuery} from './subgraphApiBaseQuery';
import {SubgraphApiEndpointBuilder} from './subgraphApiEndpointBuilder';
import {SuperfluidApiReducerPath} from './subgraphApiReducerPath';

export const createSubgraphApiSlice = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) =>
    createRtkQueryApi({
        keepUnusedDataFor: CacheTime.OneMinute,
        reducerPath: typeGuard<SuperfluidApiReducerPath>('superfluid/subgraphApi'),
        baseQuery: subgraphApiBaseQuery(),
        tagTypes: cacheTagTypes,
        endpoints: (_builder: SubgraphApiEndpointBuilder) => ({}),
        serializeQueryArgs: getSerializeQueryArgs(),
    });

export type SubgraphApiEmpty = ReturnType<typeof createSubgraphApiSlice>;
