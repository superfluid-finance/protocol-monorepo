import {SerializedError} from '@reduxjs/toolkit';
import {BaseQueryFn} from '@reduxjs/toolkit/dist/query';

export const subgraphBaseQuery = (): BaseQueryFn<void, unknown, SerializedError, Record<string, unknown>> => () => {
    throw new Error('All queries & mutations must use the `queryFn` definition syntax.');
};

export type SubgraphBaseQuery = ReturnType<typeof subgraphBaseQuery>;
