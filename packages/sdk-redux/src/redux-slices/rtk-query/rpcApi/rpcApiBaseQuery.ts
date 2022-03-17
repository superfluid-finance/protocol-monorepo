import {BaseQueryFn} from '@reduxjs/toolkit/dist/query';

export const rpcApiBaseQuery = (): BaseQueryFn<void, unknown, unknown, Record<string, unknown>> => () => {
    throw new Error('All queries & mutations must use the `queryFn` definition syntax.');
};

export type RpcApiBaseQuery = ReturnType<typeof rpcApiBaseQuery>;
