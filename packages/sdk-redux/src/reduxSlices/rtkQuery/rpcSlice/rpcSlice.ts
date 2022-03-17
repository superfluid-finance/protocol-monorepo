import {CreateApi} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';

import {typeGuard} from '../../../utils';
import {cacheTagTypes} from '../cacheTags/CacheTagTypes';
import {getSerializeQueryArgs} from '../getSerializeQueryArgs';

import {rpcBaseQuery} from './rpcBaseQuery';
import {RpcEndpointBuilder} from './rpcEndpointBuilder';
import {RpcReducerPath} from './rpcReducerPath';

export const createRpcSlice = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) =>
    createRtkQueryApi({
        reducerPath: typeGuard<RpcReducerPath>('superfluid/rpc'),
        baseQuery: rpcBaseQuery(),
        tagTypes: cacheTagTypes,
        endpoints: (_builder: RpcEndpointBuilder) => ({}),
        serializeQueryArgs: getSerializeQueryArgs(),
    });

export type RpcSliceEmpty = ReturnType<typeof createRpcSlice>;
