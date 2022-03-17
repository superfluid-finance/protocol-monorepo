import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';

import {CacheTagTypes} from '../cacheTags/CacheTagTypes';

import {RpcBaseQuery} from './rpcBaseQuery';
import {RpcReducerPath} from './rpcReducerPath';

export type RpcEndpointBuilder = EndpointBuilder<RpcBaseQuery, CacheTagTypes, RpcReducerPath>;
