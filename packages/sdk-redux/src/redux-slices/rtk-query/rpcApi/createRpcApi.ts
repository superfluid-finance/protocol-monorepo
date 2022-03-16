import { CreateApi } from "@reduxjs/toolkit/dist/query";
import type { ModuleName } from "@reduxjs/toolkit/dist/query/apiTypes";
import { typeGuard } from "../../../utils";
import { cacheTagTypes } from "../cacheTags/CacheTagTypes";
import { getSerializeQueryArgs } from "../getSerializeQueryArgs";
import rpcApiBaseQuery from "./rpcApiBaseQuery";
import RpcApiReducerPath from "./rpcApiReducerPath";
import RpcApiEndpointBuilder from "./rpcApiEndpointBuilder";

const createRpcApi = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) =>
    createRtkQueryApi({
        reducerPath: typeGuard<RpcApiReducerPath>("Superfluid/rpcApi"),
        baseQuery: rpcApiBaseQuery(),
        tagTypes: cacheTagTypes,
        endpoints: (_builder: RpcApiEndpointBuilder) => ({}),
        serializeQueryArgs: getSerializeQueryArgs()
    });

export default createRpcApi;
