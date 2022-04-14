import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';

import {CacheTagType} from '../cacheTags/CacheTagTypes';

import {RpcBaseQuery} from './rpcBaseQuery';
import {RpcReducerPath} from './rpcReducerPath';

export type RpcEndpointBuilder = EndpointBuilder<RpcBaseQuery, CacheTagType, RpcReducerPath>;
