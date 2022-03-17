import {BaseQueryFn} from '@reduxjs/toolkit/dist/query';

export const subgraphApiBaseQuery = (): BaseQueryFn<void, unknown, unknown, Record<string, unknown>> => () => {
    throw new Error('All queries & mutations must use the `queryFn` definition syntax.');
};

export type SubgraphApiBaseQuery = ReturnType<typeof subgraphApiBaseQuery>;
