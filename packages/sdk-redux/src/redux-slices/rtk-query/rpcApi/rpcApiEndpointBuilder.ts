import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';

import {CacheTagTypes} from '../cacheTags/CacheTagTypes';

import {RpcApiBaseQuery} from './rpcApiBaseQuery';
import RpcApiReducerPath from './rpcApiReducerPath';

type RpcApiEndpointBuilder = EndpointBuilder<RpcApiBaseQuery, CacheTagTypes, RpcApiReducerPath>;

export default RpcApiEndpointBuilder;
