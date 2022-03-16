import { RpcApiBaseQuery } from "./rpcApiBaseQuery";
import { CacheTagTypes } from "../cacheTags/CacheTagTypes";
import RpcApiReducerPath from "./rpcApiReducerPath";
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';

type RpcApiEndpointBuilder = EndpointBuilder<RpcApiBaseQuery, CacheTagTypes, RpcApiReducerPath>;

export default RpcApiEndpointBuilder;
