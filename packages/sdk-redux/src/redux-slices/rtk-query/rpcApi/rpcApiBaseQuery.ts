import { BaseQueryFn } from "@reduxjs/toolkit/dist/query";
import { MutationMeta, ValidationError } from "../returnTypes";

export default function rpcApiBaseQuery(): BaseQueryFn<void,
    unknown,
    ValidationError,
    Record<string, unknown>,
    MutationMeta> {
    return function() {
        throw new Error("All queries & mutations must use the `queryFn` definition syntax.");
    };
}

export type RpcApiBaseQuery = ReturnType<typeof rpcApiBaseQuery>;
